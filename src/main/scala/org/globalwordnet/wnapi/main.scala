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
    id : String = "my-awesome-wordnet",
    label : String = "My Awesome WordNet",
    language : Language = Language.ENGLISH,
    email : String = "user@example.com",
    license : String = "https://creativecommons.org/licenses/by/4.0/",
    version : String = "0.1",
    url : Option[String] = None,
    citation : Option[String] = None
  )

  final val supportedInputFormats = Seq("WNLMF", "JSON", "RDF", "WNDB", "OMWN", "PLWN")
  final val supportedOutputFormats = Seq("WNLMF", "JSON", "RDF")

  def main(args : Array[String]) {
    val parser = new scopt.OptionParser[GWNAPIConfig]("gwn") {
      head("Global WordNet Converter and Validator", "0.1")

      opt[File]('i', "input") required() valueName("<inputFile>") action { (x, c) =>
        c.copy(inputFile = x)
      } validate { x =>
        if(x.exists) { success } else { failure("Input file does not exist") }
      } text("The input file to process")

      opt[File]('o', "output") valueName("<outputFile>") action { (x, c) =>
        c.copy(outputFile = x)
      } validate { x =>
        if(!x.exists || !x.isDirectory) { success } else { failure("Output file exists and is a directory") }
      } text("The output file to process")

      opt[File]('a', "aux") valueName("<auxFile>") action { (x, c) =>
        c.copy(auxFile = x)
      } validate { x =>
        if(x.exists) { success } else { failure("Auxiliary file does not exist") }
      } text("The auxiliary file (normaly PWN 3.0) required by some formats")

      opt[String]('f', "from") valueName("<format>") action { (x, c) =>
        c.copy(inputFormat = x.toUpperCase)
      } validate { x =>
        if(supportedInputFormats contains (x.toUpperCase)) { success } else { failure("Not a supported format") }
      } text("The format of the input file: " + supportedInputFormats.mkString(", "))

      opt[String]('t', "to") valueName("<format>") action { (x, c) =>
        c.copy(outputFormat = x.toUpperCase)
      } validate { x =>
        if(supportedOutputFormats contains (x.toUpperCase)) { success } else { failure("Not a supported format") }
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
      } text("The email address for this resource")

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
    }

    parser.parse(args, GWNAPIConfig()) match {
      case Some(config) =>
        convertFormat(config)
      case None =>
        System.exit(-1)
    }
  }

  def loadAuxiliary(config : GWNAPIConfig) : LexicalResource = {
    config.auxFormat match {
      case "WNLMF" =>
        WNLMF.read(config.auxFile)
      case "JSON" =>
        WNJSON.read(config.auxFile)
      case "RDF" =>
        WNRDF.read(config.auxFile)
      case _ =>
        System.err.println("Invalid auxiliary format")
        System.exit(-1)
        null
    }
  }

  def convertFormat(config : GWNAPIConfig) {
    val resource : LexicalResource = config.inputFormat match {
      case "WNLMF" =>
        WNLMF.read(config.inputFile)
      case "JSON" =>
        WNJSON.read(config.inputFile)
      case "RDF" =>
        WNRDF.read(config.inputFile)
      case "WNDB" =>
        if(!config.inputFile.isDirectory) {
          System.err.println("For WNDB format please point to the directory giving the WordNet files")
          System.exit(-1)
        }
        WNDB.read(config.inputFile, new WNDBProperties(
          config.auxFile.getAbsolutePath(),
          config.id,
          config.label,
          config.language,
          config.email,
          config.license,
          config.version,
          config.url,
          config.citation))
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
      case _ =>
        System.err.println("TODO")
        System.exit(-1)
        null
    }
    config.outputFormat match {
      case "WNLMF" =>
        if(config.outputFile != null) {
          WNLMF.write(resource, config.outputFile)
        } else {
          WNLMF.write(resource, new PrintWriter(System.out))
        }
      case "JSON" =>
        if(config.outputFile != null) {
          WNJSON.write(resource, config.outputFile)
        } else {
          WNJSON.write(resource, new PrintWriter(System.out))
        }
      case "RDF" =>
        if(config.outputFile != null) {
          WNRDF.write(resource, config.outputFile)
        } else {
          System.err.println("RDF can only be written to a file: Please specify a file with -o")
          System.exit(-1)
        }
      case _ =>
        System.err.println("Unsupported format: " + config.outputFormat)
        System.exit(-1)
    }

  }
}
