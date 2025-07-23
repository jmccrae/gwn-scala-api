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
    citation : Option[String] = None, iliMapFile : Option[File]) extends Format {

  lazy val iliMap = iliMapFile.map(loadILIMap)

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
    Lexicon(id, label, language, email, license, version, url, citation, None, Nil, entries, synsets)
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
      val form = textDirectChild(l)
      val pos = (elem \ "POS").text
      LexicalEntry(
        makeId(form, pos),
        Lemma(writtenForm=form,
              partOfSpeech=PartOfSpeech.fromString(pos),
              script=None),
        Nil,
        Seq(Sense(
          checkAdd(id + "-", (elem \ "ID").text + "-" + (l \ "@sense").text + "-" + xmlClean(form)),
          checkAdd(id + "-", (elem \ "ID").text))))
    })
  }

  private def textDirectChild(e : Node) : String = {
    trim(e).child.flatMap({
      case x : scala.xml.Text => Some(x.text)
      case x : scala.xml.PCData => Some(x.text)
      case _ => None
    }).mkString(" ")
  }

  private def checkAdd(pre : String, target : String) = {
    if(target startsWith pre) {
      target
    } else {
      pre + target
    }
  }

  private val pwnLikeId = "(.*)-(\\d{8})-([nvars])".r

  private def readSynset(elem : Node) : Synset = {
    val ili = (elem \ "ILR").flatMap(ilr => {
      textDirectChild(ilr) match {
        case pwnLikeId(_, num, pos) => iliMap.flatMap(_.get(num.toInt, pos))
        case _ => None
      }
    }).headOption

    Synset(
      id=checkAdd(id + "-", (elem \ "ID").text),
      ili=ili,
      definitions=(elem \ "DEF").map(x => trim(x).text).filter(_.length > 0).map(Definition(_)),
      synsetExamples=(elem \ "USAGE").map(x => trim(x).text).filter(_.length > 0).map(Example(_)),
      partOfSpeech=Some(PartOfSpeech.fromString((elem \ "POS").text)),
      synsetRelations=(elem \ "SR").map(readRelation)) withNote buildNote(elem)
  }

  private def readRelation(elem : Node) : SynsetRelation = {
    val target = textDirectChild(elem)
    val relType = (elem \ "TYPE").text.toLowerCase
    SynsetRelation(id + "-" + target, SynsetRelType.fromStringOrOther(relType))
  }

  private def buildNote(elem : Node) : String = {
    Seq(
      if((elem \ "BCS").isEmpty) { "" } else { "BCS=" + (elem \ "BCS").text },
      if((elem \ "DOMAIN").isEmpty) { "" } else { "DOMAIN=" +  (elem \ "DOMAIN").text },
      if((elem \ "SUMO").isEmpty) { "" } else { "SUMO=" + 
        (elem \ "SUMO" \ "@type").text + (elem \ "SUMO").text }).mkString(", ")
  }

  def write(resource : LexicalResource, output : File) : Unit = {
    write(resource, new java.io.FileWriter(output))
  }

  def write(resource : LexicalResource, _output : Writer) : Unit = {
    val out = new PrintWriter(_output)
    writeLexicalResource(out, resource)
    out.flush
    out.close
  } 

  private def writeLexicalResource(out : PrintWriter, e : LexicalResource) : Unit = {
    out.print("""<?xml version="1.0" encoding="UTF-8"?>
<WN>""")
    if(e.lexicons.size != 1) {
      throw new IllegalArgumentException("Cannot write multilingual DebVisDic file")
    }
    val synset2Entries : Map[String, Seq[LexicalEntry]] = e.lexicons(0).entries.flatMap({ e =>
      e.senses.map(s => s.synsetRef -> e) 
    }).groupBy(_._1).view.mapValues(_.map(_._2)).toMap
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
      out.print("""
  </SYNSET>""")
    }
    out.print("""
    </WN>""")

  }

  private val iliIdType1 = "ili:(i\\d+)".r
  private val iliIdType2 = "<(i\\d+)>".r
 
  private def loadILIMap(fileName : File) : Map[(Int, String), String] = {
    (io.Source.fromFile(fileName).getLines().filter(_.contains("owl:sameAs")).flatMap { line =>
      val elems = line.split("\\s+")
      val ili = elems(0) match {
        case iliIdType1(id) => id
        case iliIdType2(id) => id
      }
      val offset = if(elems(2).startsWith("pwn31:")) {
        elems(2).drop(7).dropRight(2).toInt
      } else if(elems(2).startsWith("pwn30:")) {
        elems(2).drop(6).dropRight(2).toInt
      } else {
        throw new RuntimeException(elems(2))
      }
      val pos = elems(2).takeRight(1)
      if(elems(1) == "owl:sameAs") {
        if(pos == "s" && elems(2).startsWith("pwn31")) {
          Some((offset, "a") -> ili)
        } else {
          Some((offset, pos) -> ili)
        }
      } else {
        None
      }
    }).toMap
  }



}
