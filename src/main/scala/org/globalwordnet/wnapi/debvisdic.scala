package org.globalwordnet.api.serialize

import org.globalwordnet.api._
import org.globalwordnet.api.wn._
import eu.monnetproject.lang.Language
import java.io.{Reader, Writer, PrintWriter, File}
import scala.xml.{XML, Elem, Node}
import scala.xml.Utility.trim
import org.apache.commons.lang3.StringEscapeUtils.{escapeXml10 => escapeXml, escapeJava}

class DebVisDic(id : String, label : String, language : Language,
    email : String, license : String, version : String, url : Option[String] = None,
    citation : Option[String] = None) extends Format {
  def read(file : File) : LexicalResource = {
    readLexicalResource(XML.loadFile(file))
  }

  def read(input : Reader) : LexicalResource = {
    readLexicalResource(XML.load(input))
  }

  private def readLexicalResource(elem : Elem) : LexicalResource = {
    LexicalResource(Seq(readWN(elem)))
  }

  private def readWN(elem : Node) : Lexicon = {
    val entries = (elem \ "SYNSET").flatMap(readEntry).groupBy(_.id).values.map(mergeEntries).toSeq
    val synsets = (elem \ "SYNSET").map(readSynset)
    Lexicon(id, label, language, email, license, version, url, citation, entries, synsets)
  }


  private def xmlClean(form : String) : String = {
    escapeXml(form.replaceAll(" ", "_").replaceAll("'", "-ap-").replaceAll("\\(","-lb-").replaceAll("\\)","-rb-").replaceAll("/","-sl-").replaceAll(";","-sc-"))
  }

  private def makeId(form : String, pos : String) : String = {
    val lemma_key = form + "-" + pos
    return id + "-" + xmlClean(lemma_key)
  }

  private def mergeEntries(entries : Seq[LexicalEntry]) = 
    LexicalEntry(
      entries.head.id,
      entries.head.lemma,
      Nil,
      entries.flatMap(_.senses))

  private def readEntry(elem : Node) : Seq[LexicalEntry] = {
    (elem \ "SYNONYM" \ "LITERAL").map({ l =>
      val form = trim(l).text
      val pos = (elem \ "POS").text
      LexicalEntry(
        makeId(form, pos),
        Lemma(writtenForm=form,
              partOfSpeech=PartOfSpeech.fromString(pos),
              script=None),
        // TODO relationships
        Nil,
        Seq(Sense(
          checkAdd(id + "-", (elem \ "ID").text + "-" + (l \ "@sense").text + "-" + xmlClean(form)),
          checkAdd(id + "-", (elem \ "ID").text))))
    })
  }

  private def checkAdd(pre : String, target : String) = {
    if(target startsWith pre) {
      target
    } else {
      pre + target
    }
  }

  private def readSynset(elem : Node) : Synset = {
    Synset(
      id=checkAdd(id + "-", (elem \ "ID").text),
      ili=None,
      definitions=(elem \ "DEF").map(x => trim(x).text).filter(_.length > 0).map(Definition(_)),
      synsetExamples=(elem \ "USAGE").map(x => trim(x).text).filter(_.length > 0).map(Example(_)),
      partOfSpeech=Some(PartOfSpeech.fromString((elem \ "POS").text))) withNote buildNote(elem)
  }

  private def buildNote(elem : Node) : String = {
    Seq(
      if((elem \ "BCS").isEmpty) { "" } else { "BCS=" + (elem \ "BCS").text },
      if((elem \ "DOMAIN").isEmpty) { "" } else { "DOMAIN=" +  (elem \ "DOMAIN").text },
      if((elem \ "SUMO").isEmpty) { "" } else { "SUMO=" + 
        (elem \ "SUMO" \ "@type").text + (elem \ "SUMO").text }).mkString(", ")
  }

  def write(resource : LexicalResource, output : File) {
    write(resource, new java.io.FileWriter(output))
  }

  def write(resource : LexicalResource, _output : Writer) {
    val out = new PrintWriter(_output)
    writeLexicalResource(out, resource)
    out.flush
    out.close
  } 

  private def writeLexicalResource(out : PrintWriter, e : LexicalResource) {
    out.print("""<?xml version="1.0" encoding="UTF-8"?>
<WN>""")
    if(e.lexicons.size != 1) {
      throw new IllegalArgumentException("Cannot write multilingual DebVisDic file")
    }
    val synset2Entries : Map[String, Seq[LexicalEntry]] = e.lexicons(0).entries.flatMap({ e =>
      e.senses.map(s => s.synsetRef -> e) 
    }).groupBy(_._1).mapValues(_.map(_._2))
    for(synset <- e.lexicons(0).synsets) {
      out.print(s"""
  <SYNSET>
    <ID>${synset.id}</ID>
    <POS>${synset.partOfSpeech.getOrElse(throw new IllegalArgumentException("POS is required on synset for DebVisDic serialization")).shortForm}</POS>
    <SYNONYM>""")
      for((entry, index) <- synset2Entries.getOrElse(synset.id, Nil).zipWithIndex) {
        out.print(s"""
      <LITERAL sense="${index+1}">${entry.lemma.writtenForm}</LITERAL>""")
      }
      out.print(s"""
    </SYNONYM>""")
      synset.definitions.headOption.map { defn =>
          out.print(s"""
    <DEF>${escapeXml(defn.content)}</DEF>""")
      }
      synset.synsetExamples.map { example =>
          out.print(s"""
    <USAGE>${escapeXml(example.content)}</USAGE>""")
      }
      // TODO: More tags here
      out.print("""
  </SYNSET>""")
    }
    out.print("""
    </WN>""")

  }
 

}
