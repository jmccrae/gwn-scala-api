package org.globalwordnet.api.web

import eu.monnetproject.lang.Language
import java.io.File
import java.io.Reader
import javax.xml.parsers.DocumentBuilderFactory
import org.globalwordnet.api.serialize._
import org.globalwordnet.api.wn.LexicalResource
import org.scalatra._
import org.scalatra.servlet.{FileUploadSupport, MultipartConfig, SizeConstraintExceededException, FileItem}
import org.xml.sax.{InputSource,ErrorHandler,SAXParseException}
import scala.util.{Try,Success,Failure}

class GWNConverter extends ScalatraServlet with FileUploadSupport {
  configureMultipartHandling(MultipartConfig(maxFileSize = Some(1024*1024*1024)))
  
  def validateInitialXML(input : Reader) {
    val factory = DocumentBuilderFactory.newInstance();
    factory.setValidating(true)
    val builder = factory.newDocumentBuilder();
    builder.setErrorHandler(new ErrorHandler() {
      override def error(exception : SAXParseException) { throw exception }
      override def fatalError(exception : SAXParseException) { throw exception }
      override def warning(exception : SAXParseException) { throw exception }
    })
    val document = builder.parse(new InputSource(input))
    if(document.getDoctype() == null || 
      "http://globalwordnet.github.io/schemas/WN-LMF-1.0.dtd" !=
      document.getDoctype().getSystemId()) {
        throw new XMLHeaderException("The DTD of the file must be http://globalwordnet.github.io/schemas/WN-LMF-1.0.dtd")
    }
  }

  def processFile(item : FileItem) : Reader = {
    new java.io.InputStreamReader(item.getInputStream)
  }

  def unzipFiles(item : FileItem) : Try[File] = {
    import java.util.zip._
    Try {
      val directory = java.nio.file.Files.createTempDirectory(item.getName).toFile()
      deleteInSixHours(directory)
      val zis = new ZipInputStream(item.getInputStream)
      var zipEntry = zis.getNextEntry()
      while(zipEntry != null){
        val fileName = zipEntry.getName()
        val newFile = new File(directory, fileName)
        System.err.println(s"Writing $fileName to ${newFile.getAbsolutePath()}")
        val fos = new java.io.FileOutputStream(newFile)
        val buffer = new Array[Byte](1024*1024)
        var len = zis.read(buffer)
        while(len > 0) {
          fos.write(buffer, 0, len)
          len = zis.read(buffer)
        }
        fos.close()
        zipEntry = zis.getNextEntry()
      }
      zis.closeEntry();
      zis.close();
      directory
    }
  }

  def createTmpFile(item : FileItem) : Try[File] = {
    Try {
      val f = File.createTempFile(item.getName, ".part")
      deleteInSixHours(f)
      val out = new java.io.FileOutputStream(f)
      val in = item.getInputStream
      val buf = new Array[Byte](1024*1024)
      var read = in.read(buf)
      while(read > 0) {
        out.write(buf, 0, read)
        read = in.read(buf)
      }
      out.close()
      in.close()
      f
    }
  }

  def loadAuxiliary() : Try[LexicalResource] = {
    def aux = processFile(fileParams("auxFile"))
    params("auxFormat") match {
      case "lmf" =>
        Try(new WNLMF().read(aux))
      case "json" =>
        Try(WNJSON.read(aux))
      case "rdf" =>
        createTmpFile(fileParams("auxFile")).flatMap(aux =>
          Try(new WNRDF().read(aux)))
    }
  }

  def deleteRecursively(f : File) {
    if(f.isDirectory) {
      f.listFiles().foreach(deleteRecursively)
    }
    f.delete()
  }

  def deleteInSixHours(f : File) = {
    f.deleteOnExit()
    new Thread(new Runnable() {
      def run {
        Thread.sleep(6*60*60*1000)
        deleteRecursively(f)
      }
    }).start()
  }

  def makeOutput() : File = {
    val f = File.createTempFile("output", ".tmp")
    deleteInSixHours(f)
    f
  }


  def optString(s : String) : Option[String] = s match {
    case null => None
    case "" => None
    case s => Some(s)
  }

  def convert() : Try[File] = {
    val resource : Try[LexicalResource] = params("inputFormat") match {
      case "lmf" =>
        Try(new WNLMF().read(input))
      case "json" =>
        Try(WNJSON.read(input))
      case "rdf" =>
        val rdfType = params("inputRdfLang")
        if(params("outputRdfBaseUrl") != "") {
          Try(new WNRDF().read(input,
            rdfType, params("outputRdfBaseUrl")))
        } else {
          Try(new WNRDF().read(inputFile, rdfType))
        }
      case "wndb" =>
        val inputFile = unzipFiles(fileParams("inputFile"))
        val auxFile = createTmpFile(fileParams("auxFile"))
        inputFile.flatMap(inputFile => 
            auxFile.flatMap(auxFile => 
              Try(new WNDB(
                auxFile,
                params("id"),
                params("label"),
                Language.get(params("language")),
                params("email"),
                params("license"),
                params("version"),
                optString(params("url")),
                optString(params("citation")),
                true, // TODO
                None,
                None).read(inputFile))))
      case "omwn" =>
        loadAuxiliary().flatMap(aux =>
          Try(OpenMultilingualWordNet.read(
              inputFile,
              aux,
              Language.get(params("language")).getIso639_3(),
              params("id") + "-")))
      case "plwn" =>
        loadAuxiliary().flatMap(aux =>
            Try(
            plWordNetReader.read(
              inputFile,
              PLWordNetConfig(
                params("email"),
                params("license"),
                params("version"),
                optString(params("url")),
                optString(params("citation"))),
              aux)))
      case "debvisdic" =>
        val auxFile = fileParams.get("auxFile").flatMap(x => createTmpFile(x).toOption)
        val dvdReader = new DebVisDic(
          params("id"),
          params("label"),
          Language.get(params("language")),
          params("email"),
          params("license"),
          params("version"),
          optString(params("url")),
          optString(params("citation")),
          auxFile)
        Try(dvdReader.read(input))
      case "w3c" =>
        val w3cReader = new W3C(
          params("id"),
          params("label"),
          Language.get(params("language")),
          params("email"),
          params("license"),
          params("version"),
          optString(params("url")),
          optString(params("citation")))
        Try(w3cReader.read(inputFile))
      case "omwnlmf" =>
        val omwnlmfReader = new OMWNLMF(
          params("email"),
          params("license"))
        Try(omwnlmfReader.read(input))
      case x =>
        Try(throw new RuntimeException("Form submission error: " + x))
    }
    val outputFile = makeOutput()
    val res = resource.flatMap(resource => {
      params("outputFormat") match {
        case "lmf" =>
          Try(new WNLMF().write(resource, outputFile))
        case "json" =>
          Try(WNJSON.write(resource, outputFile))
        case "rdf" =>
          val rdfType = params("outputRdfLang")
          val shortRelations = params.contains("shortRelations") &&
            params("shortRelations") == "on"
          Try(new WNRDF(shortRelations).write(resource, outputFile, params("outputRdfBaseUrl"), rdfType, true))
        case x =>
          Try(throw new RuntimeException("Form submission error: " + x))
      }
    })
    res.map(_ => outputFile)
  }

  def outputMimeType = params("outputFormat") match {
    case "lmf" => "text/xml"
    case "json" => "application/json"
    case "rdf" => params("outputRdfLang") match {
      case "RDF/XML" => "application/rdf+xml"
      case "TURTLE" => "text/turtle"
      case "N-TRIPLE" => "application/n-triples"
      case "N3" => "application/n3"
    }
  }

  def input = params("inputText") match {
    case "" => processFile(fileParams("inputFile"))
    case t => new java.io.StringReader(t)
  }

  def inputFile : File = throw new IllegalArgumentException()

  post("/convert") {
    convert() match {
      case Success(f) => {
        contentType = outputMimeType
        f
      }
      case Failure(t) => {
        views.html.failure(t)
      }
    }
  }

 
  post("/validate") {
   val result = params("inputFormat") match {
      case "lmf" =>
        Try(
          validateInitialXML(input)
        ).flatMap(_ => Try(new WNLMF().read(input)))
      case "json" =>
        Try(WNJSON.read(input))
      case "rdf" =>
        Try(new WNRDF().read(input, params("inputRdfLang"), ""))
    }
    views.html.validate(result)
  }

  get("/") {
    views.html.index()
  }

}

class XMLHeaderException(msg : String) extends RuntimeException(msg)
