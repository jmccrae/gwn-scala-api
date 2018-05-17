package org.globalwordnet.api.web

import org.scalatra._
import org.scalatra.servlet.{FileUploadSupport, MultipartConfig, SizeConstraintExceededException, FileItem}
import org.globalwordnet.api.serialize._
import scala.util.Try
import java.io.Reader
import javax.xml.parsers.DocumentBuilderFactory
import org.xml.sax.{InputSource,ErrorHandler,SAXParseException}

class GWNConverter extends ScalatraServlet with FileUploadSupport {
  configureMultipartHandling(MultipartConfig(maxFileSize = Some(3*1024*1024)))
  
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

  post("/validate") {
    def input = params("inputText") match {
      case "" => processFile(fileParams("inputFile"))
      case t => new java.io.StringReader(t)
    }
    val result = params("inputFormat") match {
      case "lmf" =>
        Try(
          validateInitialXML(input)
        ).flatMap(_ => Try(WNLMF.read(input)))
      case "json" =>
        Try(WNJSON.read(input))
      case "rdf" =>
        Try(WNRDF.read(input, params("inputRdfLang"), ""))
    }
    println(result)
    views.html.validate(result)
  }

  get("/") {
    views.html.index()
  }

}

class XMLHeaderException(msg : String) extends RuntimeException(msg)
