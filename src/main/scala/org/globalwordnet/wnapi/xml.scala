package org.globalwordnet.api.serialize

import org.globalwordnet.api._
import org.globalwordnet.api.wn._
import java.io.{Reader, Writer, PrintWriter, File}
import scala.xml.{XML, Elem, Node}
import scala.xml.Utility.trim
import org.apache.commons.lang3.StringEscapeUtils.{escapeXml10 => escapeXml}

object WNLMF extends Format {
  def read(file : File) : LexicalResource = {
    readLexicalResource(XML.loadFile(file))
  }

  def read(input : Reader) : LexicalResource = {
    readLexicalResource(XML.load(input))
  }

  private def readLexicalResource(elem : Elem) : LexicalResource = {
    LexicalResource((elem \ "Lexicon").map(readLexicon))
  }

  private def readLexicon(elem : Node) : Lexicon = {
    readMeta(Lexicon(
      (elem \ "LexicalEntry").map(readEntry),
      (elem \ "Synset").map(readSynset),
      (elem \ "@id").text,
      (elem \ "@label").text,
      (elem \ "@language").text,
      (elem \ "@email").text,
      (elem \ "@license").text,
      (elem \ "@version").text,
      (elem \ "@url").headOption.map(_.text),
      (elem \ "@citation").headOption.map(_.text)), elem)
  }

  private def readMeta[A <: Meta](a : A, elem : Node) : A = {
    (elem \ "@{http://purl.org/dc/elements/1.1/}contributor").foreach(x => a.contributor = Some(x.text))
    (elem \ "@{http://purl.org/dc/elements/1.1/}coverage").foreach(x => a.coverage = Some(x.text))
    (elem \ "@{http://purl.org/dc/elements/1.1/}creator").foreach(x => a.creator = Some(x.text))
    (elem \ "@{http://purl.org/dc/elements/1.1/}date").foreach(x => a.date = Some(x.text))
    (elem \ "@{http://purl.org/dc/elements/1.1/}description").foreach(x => a.description = Some(x.text))
    (elem \ "@{http://purl.org/dc/elements/1.1/}format").foreach(x => a.format = Some(x.text))
    (elem \ "@{http://purl.org/dc/elements/1.1/}identifier").foreach(x => a.identifier = Some(x.text))
    (elem \ "@{http://purl.org/dc/elements/1.1/}publisher").foreach(x => a.publisher = Some(x.text))
    (elem \ "@{http://purl.org/dc/elements/1.1/}relation").foreach(x => a.relation = Some(x.text))
    (elem \ "@{http://purl.org/dc/elements/1.1/}rights").foreach(x => a.rights = Some(x.text))
    (elem \ "@{http://purl.org/dc/elements/1.1/}source").foreach(x => a.source = Some(x.text))
    (elem \ "@{http://purl.org/dc/elements/1.1/}subject").foreach(x => a.subject = Some(x.text))
    (elem \ "@{http://purl.org/dc/elements/1.1/}title").foreach(x => a.title = Some(x.text))
    (elem \ "@{http://purl.org/dc/elements/1.1/}type").foreach(x => a.`type` = Some(x.text))
    (elem \ "@status").foreach(x => a.status = Some(x.text))
    (elem \ "@confidenceScore").foreach(x => a.confidenceScore = Some(x.text.toDouble))
    return a
  }

  private def readEntry(elem : Node) : LexicalEntry = {
    readMeta(LexicalEntry(
      readLemma((elem \ "Lemma").head),
      (elem \ "Form").map(readForm),
      (elem \ "Sense").map(readSense),
      (elem \ "SyntacticBehaviour").map(readSyntacticBehaviour),
      (elem \ "@id").text), elem)
  }

  private def readLemma(elem : Node) : Lemma = {
    Lemma(
      (elem \ "@writtenForm").text,
      readPartOfSpeech((elem \ "@partOfSpeech").text))
  }

  private def readPartOfSpeech(code : String) = PartOfSpeech.fromString(code)

  private def readForm(elem : Node) : Form = {
    Form(
      (elem \ "@writtenForm").text,
      (elem \ "@tag").headOption.map(_.text))
  }

  private def readSense(elem : Node) : Sense = {
    readMeta(Sense(
      (elem \ "SenseRelation").map(readSenseRelation),
      (elem \ "Example").map(readSenseExample),
      (elem \ "@id").text,
      (elem \ "@synset").text,
      (elem \ "Count").map(readCount)), elem)
  }

  private def readSenseRelation(elem : Node) : SenseRelation = {
    readMeta(SenseRelation(
      (elem \ "@target").text,
      readSenseRel((elem \ "@relType").text, (elem \ "@{http://purl.org/dc/elements/1.1/}type").headOption.map(_.text))), elem)
  }

  private def readSenseRel(code : String, `type` : Option[String]) = {
    if(code == "other") {
      other(`type`.getOrElse(throw new IllegalArgumentException("other without type")))
    } else {
      SenseRelType.fromString(code)
    }
  }

  private def readSenseExample(elem : Node) = {
    readMeta(Example(trim(elem).text), elem)
  }

  private def readSyntacticBehaviour(elem : Node) = {
    SyntacticBehaviour((elem \ "@subcategorizationFrame").text)
  }

  private def readSynset(elem : Node) : Synset = {
    readMeta(Synset(
      (elem \ "Definition").map(readDefinition),
      (elem \ "ILIDefinition").headOption.map(readILIDefinition),
      (elem \ "SynsetRelation").map(readSynsetRelation),
      (elem \ "@id").text,
      (elem \ "@ili").headOption.map(_.text),
      (elem \ "Example").map(readSenseExample)), elem)
  }

  private def readDefinition(elem : Node) : Definition = {
    readMeta(Definition(
      trim(elem).text,
      (elem \ "@language").headOption.map(_.text)), elem)
  }

  private def readILIDefinition(elem : Node) : ILIDefinition = {
    readMeta(ILIDefinition(trim(elem).text), elem)
  }

  private def readSynsetRelation(elem : Node) : SynsetRelation = {
    readMeta(SynsetRelation(
      (elem \ "@target").text,
      readSynsetRelType((elem \ "@relType").text, (elem \ "@{http://purl.org/dc/elements/1.1/}type").headOption.map(_.text))), elem)
  }

  private def readSynsetRelType(code : String, `type` : Option[String]) = {
    if(code == "other") {
      other(`type`.getOrElse(throw new IllegalArgumentException("other without type")))
    } else {
      SynsetRelType.fromString(code)
    }
  }

  private def readCount(elem : Node) : Count = {
    readMeta(Count(elem.text.toInt), elem)
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
<!DOCTYPE LexicalResource SYSTEM "http://globalwordnet.github.io/schemas/WN-LMF-1.0.dtd">
<LexicalResource xmlns:dc="http://purl.org/dc/elements/1.1/">""")
    for(lexicon <- e.lexicons) {
      writeLexicon(out, lexicon)
    }
    out.println("""
</LexicalResource>""")
  }

  private def writeLexicon(out : PrintWriter, e : Lexicon) {
    out.print(s"""
  <Lexicon id="${e.id}" 
           label="${escapeXml(e.label)}" 
           language="${e.language}"
           email="${e.email}"
           license="${e.license}"
           version="${e.version}" """)
   e.citation.foreach(c =>
        out.print(s"""
           citation="$c" """))
    e.url.foreach(u =>
        out.print(s"""
           url="$u" """))
     writeMeta(out, 11, e)
    out.print(""">""")
    for(entry <- e.entries) {
      writeEntry(out, entry)
    }
    for(synset <- e.synsets) {
      writeSynset(out, synset)
    }
    out.print("""
  </Lexicon>""")
  }

  private def writeMeta(out : PrintWriter, indent : Int, e : Meta) {
    def writeProp(v : Option[String], n : String) = v match {
      case Some(x) =>
        out.print("\n" + (" " * indent) + n + "=\"" + escapeXml(x) + "\"")
      case None =>
    }
    writeProp(e.contributor, "dc:contributor")
    writeProp(e.coverage, "dc:coverage")
    writeProp(e.creator, "dc:creator")
    writeProp(e.date, "dc:date")
    writeProp(e.description, "dc:description")
    writeProp(e.format, "dc:format")
    writeProp(e.identifier, "dc:identifier")
    writeProp(e.publisher, "dc:publisher")
    writeProp(e.relation, "dc:relation")
    writeProp(e.rights, "dc:rights")
    writeProp(e.source, "dc:source")
    writeProp(e.subject, "dc:subject")
    writeProp(e.title, "dc:title")
    writeProp(e.`type`, "dc:type")
    writeProp(e.status, "status")
    writeProp(e.confidenceScore.map(s => "%.8f" format s), "confidenceScore")
  }
 
  private def writeEntry(out : PrintWriter, e : LexicalEntry) {
    out.print(s"""
    <LexicalEntry id="${e.id}" """)
    writeMeta(out, 18, e)
    out.print(s""">
      <Lemma writtenForm="${escapeXml(e.lemma.writtenForm)}" partOfSpeech="${e.lemma.partOfSpeech.shortForm}"/>""")
    for(form <- e.forms) {
      writeForm(out, form)
    }
    for(sense <- e.senses) {
      writeSense(out, sense)
    }
    for(synBeh <- e.syntacticBehaviours) {
      writeSyntacticBehaviour(out, synBeh)
    }
    out.print("""
    </LexicalEntry>""")
  }

  def writeForm(out : PrintWriter, e : Form) {
    out.print(s"""
      <Form writtenForm="${escapeXml(e.writtenForm)}" """)
    e.tag match {
      case Some(t) =>
        out.print(s"""tag="$t" """)
      case None =>
    }
    out.print("/>")
  }

  def writeSense(out : PrintWriter, e : Sense) {
    out.print(s"""
      <Sense id="${e.id}" synset="${e.synsetRef}" """)
    writeMeta(out, 13, e)
    out.print(">")
    for(rel <- e.senseRelations) {
      writeSenseRel(out, rel)
    }
    for(x <- e.senseExamples) {
      writeSenseExample(out, x)
    }
    for(c <- e.counts) {
      writeCount(out, c)
    }
    out.print("""
      </Sense>""")
  }

  def writeSenseRel(out : PrintWriter, e : SenseRelation) {
    out.print(s"""
        <SenseRelation relType="${e.relType.name}" target="${e.target}" """)
    writeMeta(out, 23, e)
    out.print("/>")
  }

  def writeSenseExample(out : PrintWriter, e : Example) {
    out.print(s"""
        <Example """)
    writeMeta(out, 22, e)
    out.print(s""">${escapeXml(e.content)}</Example>""")
  }

  def writeSyntacticBehaviour(out : PrintWriter, e : SyntacticBehaviour) {
    out.print(s"""
      <SyntacticBehaviour subcategorizationFrame="${e.subcategorizationFrame}"/>""")
  }

  private def writeSynset(out : PrintWriter, e : Synset) {
    out.print(s"""
    <Synset id="${e.id}" ili="${e.ili.getOrElse("")}" """)
    writeMeta(out, 12, e)
    out.print(">")
    for(d <- e.definitions) {
      writeDefinition(out, d)
    }
    e.iliDefinition match {
      case Some(i) =>
        writeILIDefinition(out, i)
      case None =>
    }
    for(r <- e.synsetRelations) {
      writeSynsetRel(out, r)
    }
    for(x <- e.synsetExamples) {
      writeSenseExample(out, x)
    }
   out.print(s"""
    </Synset>""")
  }

  private def writeDefinition(out : PrintWriter, e : Definition) {
    out.print(s"""
      <Definition """)
    e.language match {
      case Some(l) =>
        out.print("""                  language="$l" """)
      case None =>
    }
    writeMeta(out, 18, e)
    out.print(s""">${escapeXml(e.content)}</Definition>""")
  }

  private def writeILIDefinition(out : PrintWriter, e : ILIDefinition) {
    out.print(s"""
      <ILIDefinition """)
    writeMeta(out, 21, e)
    out.print(s""">${escapeXml(e.content)}</ILIDefinition>""")
  }

  def writeSynsetRel(out : PrintWriter, e : SynsetRelation) {
    out.print(s"""
      <SynsetRelation relType="${e.relType.name}" target="${e.target}" """)
    writeMeta(out, 22, e)
    out.print("/>")
  }


  private def writeCount(out : PrintWriter, e : Count) {
    out.print(s"""
      <Count """)
    writeMeta(out, 14, e)
    out.print(s""">${e.value}</Count>""")
  }
}
