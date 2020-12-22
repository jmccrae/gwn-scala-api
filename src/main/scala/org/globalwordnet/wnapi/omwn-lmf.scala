package org.globalwordnet.api.serialize

import java.io.File
import org.globalwordnet.api._
import org.globalwordnet.api.wn._
import scala.xml.{XML, Elem, Node}
import eu.monnetproject.lang.{Language, Script}

class OMWNLMF(email : String, license : String) extends Format {
  def read(file : File) : LexicalResource = {
    val xml = try {
      XML.loadFile(file)
    } catch {
      case x : java.io.IOException => {
        System.err.println("Failed to load the input XML file, this could be caused by a number of issues")
        System.err.println("* The file does not exist or is not accessible")
        System.err.println("* The XML is not well-formed")
        System.err.println("* The XML refers to an external entity using a <!DOCTYPE> tag, that is not available")
        x.printStackTrace()
        throw x
      }
    }
    readLexicalResource(xml)
  }

  private def readLexicalResource(elem : Elem) : LexicalResource = {
    val label = (elem \ "GlobalInformation" \ "@label").text
    LexicalResource((elem \ "Lexicon").map(readLexicon(_,label)))
  }
  
  private def attText(elem : Node, prop : String, deflt : String) : String = {
    val r = (elem \ prop).text
    if(r == null) {
      System.err.println("Mandatory property " + prop + " is missing, defaulting to \"" + deflt + "\"")
      deflt
    } else {
      r
    }
  }

  private def readLexicon(elem : Node, label : String) : Lexicon = {
    Lexicon(
      attText(elem, "@id", attText(elem, "@language", "id")),
      label,
      Language.get(attText(elem, "@language", "en")),
      email,
      license,
      attText(elem, "@version", ""),
      (elem \ "@owner").headOption.map(_.text),
      None,
      (elem \ "LexicalEntry").map(readEntry),
      (elem \ "Synset").map(readSynset))
  }
 
  private def readEntry(elem : Node) : LexicalEntry = {
    readMeta(LexicalEntry(
      (elem \ "@id").text,
      readLemma((elem \ "Lemma").head),
      Nil,
      (elem \ "Sense").map(readSense),
      Nil), elem)
  }

  private def readLemma(elem : Node) : Lemma = {
    Lemma(
      (elem \ "@writtenForm").text,
      readPartOfSpeech((elem \ "@partOfSpeech").text))
  }


  private def readMeta[A <: Meta](a : A, elem : Node) : A = {
    (elem \ "Meta" \ "@author").foreach(x => a.contributor = Some(x.text))
    (elem \ "Meta" \ "@date").foreach(x => a.date = Some(x.text))
    (elem \ "Meta" \ "@source").foreach(x => a.source = Some(x.text))
    (elem \ "Meta" \ "@status").foreach(x => a.status = Some(x.text))
    (elem \ "Meta" \ "@confidenceScore").foreach(x => a.confidenceScore = Some(x.text.toDouble))
    return a
  }

  private def readSense(elem : Node) : Sense = {
    readMeta(Sense(
      (elem \ "@id").text,
      (elem \ "@synset").text), elem)
  }

  private def readSynset(elem : Node) : Synset = {
    readMeta(Synset(
      (elem \ "@id").text,
      None,
      (elem \ "Definition").map(readDefinition),
      None,
      (elem \ "SynsetRelations" \ "SynsetRelation").flatMap(readSynsetRelation),
      (elem \ "Definition").flatMap(e => (e \ "Statement").map(readExample))), elem)
  }

  private def readDefinition(elem : Node) : Definition = {
    readMeta(Definition(
      attText(elem, "@gloss", "")), elem)
  }

  private def readSynsetRelation(elem : Node) : Seq[SynsetRelation] = {
    (elem \ "@targets").text.split(" ").map { target =>
      readMeta(SynsetRelation(
        target,
        readSynsetRelType((elem \ "@relType").text)), elem)
    }
  }

  private def readSynsetRelType(code : String) = SynsetRelType.fromOMWString(code)

  private def readPartOfSpeech(code : String) = PartOfSpeech.fromString(code)

  private def readExample(elem : Node) : Example = {
    Example((elem \ "@example").text)
  }


  def write(lr : LexicalResource, file : File) = {
    throw new UnsupportedOperationException("Not an output format")
  }
}
