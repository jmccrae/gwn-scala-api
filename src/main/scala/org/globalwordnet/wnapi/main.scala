package org.globalwordnet.api

import eu.monnetproject.lang.Language
import java.io.{File, PrintWriter}
import org.globalwordnet.api.wn._
import org.globalwordnet.api.serialize._

object Main {
  case class GWNAPIConfig(
    inputFile : File = null,
    outputFile : File = null,
    inputFormat : String = "WNLMF",
    outputFormat : String = "WNLMF",
    auxFile : File = null,
    auxFormat : String = "WNLMF",
    id : String = "default-wordnet-id",
    label : String = "An Example Wordnet",
    language : Language = Language.ENGLISH,
    email : String = "user@example.com",
    license : String = "https://creativecommons.org/licenses/by/4.0/",
    version : String = "0.0",
    url : Option[String] = None,
    citation : Option[String] = None,
    inputRdfLang : String = "",
    outputRdfLang : String = "RDF/XML",
    inputRdfBaseUrl : String = "",
    outputRdfBaseUrl : String = "",
    validate : Boolean = false,
    coreWordNetFilter : Option[File] = None,
    bySubject : Boolean = false,
    blankNodes : Boolean = true,
    wordnetLicense : Option[File] = None,
    shortRelations : Boolean = false
  )

  final val supportedInputFormats = Seq("WNLMF", "JSON", "RDF", "WNDB", "OMWN", "PLWN", "DEBVISDIC", "W3C", "OMWNLMF")
  final val supportedOutputFormats = Seq("WNLMF", "JSON", "RDF", "WNDB")

  def main(args : Array[String]) {
    val parser = new scopt.OptionParser[GWNAPIConfig]("gwn") {
      head("Global WordNet Converter and Validator", "0.2")

      opt[Unit]('v', "validate") action { (x, c) =>
        c.copy(validate = true)
      } text("Validate the input file only")

      opt[File]('i', "input") required() valueName("<inputFile>") action { (x, c) =>
        c.copy(inputFile = x)
      } validate { x =>
        if(x.exists) { success } else { failure("Input file does not exist") }
      } text("The input file to process")

      opt[File]('o', "output") valueName("<outputFile>") action { (x, c) =>
        c.copy(outputFile = x)
      //} validate { x =>
        //if(!x.exists || !x.isDirectory) { success } else { failure("Output file exists and is a directory") }
      } text("The output file to process")

      opt[File]('a', "aux") valueName("<auxFile>") action { (x, c) =>
        c.copy(auxFile = x)
      } validate { x =>
        if(x.exists) { success } else { failure("Auxiliary file does not exist") }
      } text("The auxiliary file (normaly PWN 3.0) required by some formats")

      opt[String]('f', "from") valueName("<format>") action { (x, c) =>
        c.copy(inputFormat = x.toUpperCase)
      } validate { x =>
        if(supportedInputFormats contains (x.toUpperCase)) { success } else { failure("Not a supported output format") }
      } text("The format of the input file: " + supportedInputFormats.mkString(", "))

      opt[String]('t', "to") valueName("<format>") action { (x, c) =>
        c.copy(outputFormat = x.toUpperCase)
      } validate { x =>
        if(supportedOutputFormats contains (x.toUpperCase)) { success } else { failure("Not a supported output format") }
      } text("The format of the output file: " + supportedOutputFormats.mkString(", "))

      opt[String]("aux-format") valueName("<format>") action { (x, c) =>
        c.copy(outputFormat = x.toUpperCase)
      } validate { x =>
        if(supportedOutputFormats contains (x.toUpperCase)) { success } else { failure("Not a supported format") }
      } text("The format of the auxiliary file")

      opt[String]("id") valueName("<id>") action { (x, c) =>
        c.copy(id = x)
      } text("The identifier for the resource")

      opt[String]("label") valueName("<label>") action { (x, c) =>
        c.copy(label = x) 
      } text("The label for the resource")
    
      opt[String]("language") valueName("<isoCode>") action { (x, c) =>
        c.copy(language = Language.get(x))
      } text("The language of the resource (ISO code)")

      opt[String]("email") valueName("<address>") action { (x, c) =>
        c.copy(email = x)
      } text("The email address of the creator of the resource")

      opt[String]("license") valueName("<url>") action { (x, c) =>
        c.copy(license = x)
      } text("The license of the resource")

      opt[String]("version") valueName("<versionId>") action { (x, c) =>
        c.copy(version = x)
      } text("The version of the resource")

      opt[String]("url") valueName("<url>") action { (x, c) =>
        c.copy(url = Some(x))
      } text("The URL of the resource")

      opt[String]("citation") valueName("<paper>") action { (x, c) =>
        c.copy(citation = Some(x))
      } text("The citation string of the resource")

      opt[String]("input-rdf-lang") valueName("<RDF/XML|TURTLE|N-TRIPLE|N3>") action { (x, c) =>
        c.copy(inputRdfLang = x)
      } text("The RDF language to serialize from")

      opt[String]("output-rdf-lang") valueName("<RDF/XML|TURTLE|N-TRIPLE|N3>") action { (x, c) =>
        c.copy(outputRdfLang = x)
      } text("The RDF language to serialize to")

      opt[String]("input-base-url") valueName("<url>") action { (x, c) =>
        c.copy(inputRdfBaseUrl = x)
      } text("The Base URL, i.e., where the file is on the Web, for the input file")

      opt[String]("output-base-url") valueName("<url>") action { (x, c) =>
        c.copy(outputRdfBaseUrl = x)
      } text("The Base URL, i.e., where the file is on the Web, for the output file")

      opt[File]("core-wordnet") valueName("<core-wordnet.txt>") action { (x, c) =>
        c.copy(coreWordNetFilter = Some(x))
      } text("[WNDB Only] The Core WordNet file to select only the Core entries and synsets")

      opt[Unit]("by-subject") action { (x, c) =>
        c.copy(bySubject = true)
      } text("Split the output by the subject to create multiple smaller files")

      opt[Unit]("no-blank-nodes") action { (x, c) =>
        c.copy(blankNodes = false)
      } text("Do not generate blank nodes in RDF exports")

      opt[File]("wordnet-license") valueName("<wn-license.txt>") action { (x, c) =>
        c.copy(wordnetLicense = Some(x))
      } text("WordNet license to be included in the header of all files")
      opt[Unit]("short-relations") action { (x, c) =>
        c.copy(shortRelations = true)
      } text("Generate short relations in RDF exports")
    }

    parser.parse(args, GWNAPIConfig()) match {
      case Some(config) =>
        if(config.validate) {
          validate(config)
        } else {
          convertFormat(config)
        }
      case None =>
        System.exit(-1)
    }
  }

  def loadAuxiliary(config : GWNAPIConfig) : LexicalResource = {
    if(config.auxFile == null ||
       !config.auxFile.exists) {
        System.err.println("Auxiliary file is required")
        System.exit(-1)
    }
    config.auxFormat match {
      case "WNLMF" =>
        new WNLMF(false).read(config.auxFile)
      case "JSON" =>
        WNJSON.read(config.auxFile)
      case "RDF" =>
        new WNRDF().read(config.auxFile)
      case _ =>
        System.err.println("Invalid auxiliary format")
        System.exit(-1)
        null
    }
  }

  def validate(config : GWNAPIConfig) {
    try {
      val resource : LexicalResource = config.inputFormat match {
        case "WNLMF" =>
          new WNLMF().read(config.inputFile)
        case "JSON" =>
          WNJSON.read(config.inputFile)
        case "RDF" =>
          val rdfType = config.inputRdfLang match {
            case "" => 
              WNRDF.guessLang(config.inputFile)
            case format =>
              format
          }
          if(config.outputRdfBaseUrl != "") {
            new WNRDF().read(config.inputFile, rdfType, config.inputRdfBaseUrl)
          } else {
            new WNRDF().read(config.inputFile, rdfType)
          }
        case format =>
          System.err.println("Validation of unexpected format: " + format)
          System.exit(-1)
          null
      }
      System.out.println("Validation successful")
      val maxIdSize = resource.lexicons.map(_.id.size).max
      System.out.println("| " + (" " * maxIdSize) + " | Lang | Words     | Synsets   |")
      System.out.println("|:" + ("-" * maxIdSize) + "-|:----:|:---------:|:---------:|")
      for(lexicon <- resource.lexicons) {
        System.out.println(("| %" + maxIdSize + "s | %3s  | % 9d | % 9d |" format 
          (lexicon.id, lexicon.language.toString(), lexicon.entries.size, lexicon.synsets.size)))
      }
    } catch {
      case x : Exception =>
        x.printStackTrace()
        System.out.println("Validation failed: " + x.getMessage())
        System.exit(-1)
    }
  }
 
  def convertFormat(config : GWNAPIConfig) {
    val resource : LexicalResource = config.inputFormat match {
      case "WNLMF" =>
        new WNLMF().read(config.inputFile)
      case "JSON" =>
        WNJSON.read(config.inputFile)
      case "RDF" =>
        val rdfType = config.inputRdfLang match {
          case "" => 
            WNRDF.guessLang(config.inputFile)
          case format =>
            format
        }
        if(config.outputRdfBaseUrl != "") {
          new WNRDF().read(config.inputFile, rdfType, config.outputRdfBaseUrl)
        } else {
          new WNRDF().read(config.inputFile, rdfType)
        }
      case "WNDB" =>
        if(!config.inputFile.isDirectory) {
          System.err.println("For WNDB format please point to the directory giving the WordNet files")
          System.exit(-1)
        }
        if(config.auxFile == null) {
          System.err.println("Need an auxiliary file for WNDB conversion")
          System.exit(-1)
        }
        new WNDB(
          config.auxFile,
          config.id,
          config.label,
          config.language,
          config.email,
          config.license,
          config.version,
          config.url,
          config.citation,
          config.wordnetLicense == None,
          config.coreWordNetFilter,
          config.wordnetLicense).read(config.inputFile)
      case "OMWN" =>
        OpenMultilingualWordNet.read(
            config.inputFile,
            loadAuxiliary(config),
            config.language.getIso639_3(),
            config.id + "-")
      case "PLWN" =>
        plWordNetReader.read(
          config.inputFile,
          PLWordNetConfig(
            config.email,
            config.license,
            config.version,
            config.url,
            config.citation),
          loadAuxiliary(config))
      case "DEBVISDIC" =>
        val dvdReader = new DebVisDic(
          config.id,
          config.label,
          config.language,
          config.email,
          config.license,
          config.version,
          config.url,
          config.citation,
          Option(config.auxFile))
        dvdReader.read(config.inputFile)
      case "W3C" =>
        val w3cReader = new W3C(
          config.id,
          config.label,
          config.language,
          config.email,
          config.license,
          config.version,
          config.url,
          config.citation)
        w3cReader.read(config.inputFile)
      case "OMWNLMF" =>
        val omwnlmfReader = new OMWNLMF(
          config.email,
          config.license)
        omwnlmfReader.read(config.inputFile)
      case _ =>
        throw new RuntimeException("Unreachable")
    }
    config.outputFormat match {
      case "WNLMF" =>
        if(config.bySubject) {
          if(config.outputFile == null) {
            System.err.println("Output file not specified, dumping each file to Syste.out in sequence!")
          }
          val entriesForSynset = resource.entriesForSynset
          for((subject, section) <- BySubject.splitBySubject(resource)) {
            if(config.outputFile != null) {
              val outPath = if(config.outputFile.getPath().endsWith(".xml")) {
                s"${config.outputFile.getPath().dropRight(4)}-$subject.xml"
              } else {
                s"${config.outputFile.getPath()}-$subject"
              }
              new WNLMF(relaxed=true).write(section, new File(outPath),
                entriesForSynset)
            } else {
              new WNLMF(relaxed=true).write(section, new PrintWriter(System.out),
                entriesForSynset)
            }
          }
        } else if(config.outputFile != null) {
          new WNLMF().write(resource, config.outputFile)
        } else {
          new WNLMF().write(resource, new PrintWriter(System.out))
        }
      case "JSON" =>
        if(config.outputFile != null && config.outputFile.getName().endsWith(".zip")) {
          WNJSON.writeAsZIP(resource, config.outputFile)
        } else if(config.outputFile != null) {
          WNJSON.write(resource, config.outputFile)
        } else {
          WNJSON.write(resource, new PrintWriter(System.out))
        }
      case "RDF" =>
        val rdfType = config.outputRdfLang match {
          case "" => 
            if(config.outputFile != null) {
              WNRDF.guessLang(config.outputFile)
            } else {
              "RDF/XML"
            }
          case format =>
            format
        }
        if(config.outputRdfBaseUrl != "") {
          if(config.outputFile != null) {
            new WNRDF(config.shortRelations).write(resource, config.outputFile, config.outputRdfBaseUrl, rdfType, config.blankNodes)
          } else {
            new WNRDF(config.shortRelations).write(resource, new PrintWriter(System.out), config.outputRdfBaseUrl, rdfType, config.blankNodes)
          }
        } else if(config.outputFile != null) {
          new WNRDF(config.shortRelations).write(resource, config.outputFile, rdfType, config.blankNodes)
        } else {
          System.err.println("RDF can only be written to a file: Please specify a file with -o or URL with --output-base-url")
          System.exit(-1)
        }
      case "WNDB" =>
        if(config.outputFile == null) {
          System.err.println("Please specify output folder with -o")
          System.exit(-1)
        }
        if(!config.outputFile.exists) {
          if(!config.outputFile.mkdirs()) {
            System.err.println("Could not create a directory for WNDB output")
            System.exit(-1)
          }
        }
        if(!config.outputFile.isDirectory) {
          System.err.println("WNDB output target must be folder")
          System.exit(-1)
        }
        new WNDB(
          config.auxFile,
          config.id,
          config.label,
          config.language,
          config.email,
          config.license,
          config.version,
          config.url,
          config.citation,
          config.wordnetLicense == None,
          config.coreWordNetFilter,
          config.wordnetLicense).write(resource, config.outputFile)
      case _ =>
        System.err.println("Unsupported format: " + config.outputFormat)
        System.exit(-1)
    }

  }
}
