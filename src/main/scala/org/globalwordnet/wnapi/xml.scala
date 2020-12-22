package org.globalwordnet.api.serialize

import eu.monnetproject.lang.{Language, Script}
import org.globalwordnet.api._
import org.globalwordnet.api.wn._
import java.io.{Reader, Writer, PrintWriter, File}
import scala.xml.{XML, Elem, Node}
import scala.xml.Utility.trim
import org.apache.commons.lang3.StringEscapeUtils.{escapeXml10 => escapeXml}

object MoreStringEscapeUtils {

  private def nameStartChar(c : Char) : Boolean = 
    c == ':' || c == '_' || 
  (c >= 'A' && c <= 'Z') ||
  (c >= 'a' && c <= 'z') ||
  (c >= '\u00C0' && c <= '\u00D6') ||
  (c >= '\u00D8' && c <= '\u00F6') ||
  (c >= '\u00F8' && c <= '\u02FF') ||
  (c >= '\u0370' && c <= '\u037D') ||
  (c >= '\u037F' && c <= '\u1FFF') ||
  (c >= '\u200C' && c <= '\u200D') ||
  (c >= '\u2070' && c <= '\u218F') ||
  (c >= '\u2C00' && c <= '\u2FEF') ||
  (c >= '\u3001' && c <= '\uD7FF') ||
  (c >= '\uF900' && c <= '\uFDCF') ||
  (c >= '\uFDF0' && c <= '\uFFFD') 

  private def nameChar(c : Char) : Boolean =
    nameStartChar(c) ||
    c == '-' ||
    c == '.' ||
    (c >= '0' && c <= '9') ||
    c == '\u00B7' ||
    (c >= '\u0300' && c <= '\u036F') ||
    (c >= '\u203F' && c <= '\u2040')

  private def replacement(c : Char) : String = c match {
    case ' ' => "_"
    case '\'' => "-ap-"
    case '(' => "-lb-"
    case ')' => "-rb-"
    case '/' => "-sl-"
    case '!' => "-ex-"
    case '"' => "-dq-"
    case '#' => "-hs-"
    case '$' => "-dl-"
    case '%' => "-pc-"
    case '&' => "-am-"
    case '*' => "-as-"
    case '+' => "-pl-"
    case ',' => "-cm-"
    case ';' => "-sc-"
    case '<' => "-lt-"
    case '>' => "-gt-"
    case '=' => "-eq-"
    case '?' => "-qu-"
    case '@' => "-at-"
    case '[' => "-lsq-"
    case ']' => "-rsq-"
    case '\\' => "-bs-"
    case '^' => "-cr-"
    case '`' => "-bt-"
    case '{' => "-lcb-"
    case '}' => "-rcb-"
    case '|' => "-br-"
    case '~' => "-tl-"
    case nonascii => 
      "-%04X-" format c.toInt
  }


  def escapeXmlId(id : String) : String = {
    val builder = new StringBuilder(id)
    var i = 0

    if(!nameStartChar(builder.charAt(0))) {
      val r = replacement(builder.charAt(0))
      builder.replace(0, 1, r)
      i += r.length
    } else {
      i += 1
    }

    while(i < builder.size) {
      if(!nameChar(builder.charAt(i))) {
        val r = replacement(builder.charAt(i))
        builder.replace(i, i+1, r)
        i += r.length
      } else {
        i += 1
      }
    }
    builder.toString
  }
}

class WNLMF(comments : Boolean = true, relaxed : Boolean = false) extends Format {
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

  def read(input : Reader) : LexicalResource = {
    val xml = try {
      XML.load(input)
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
    LexicalResource((elem \ "Lexicon").map(readLexicon))
  }

  private def attText(elem : Node, prop : String, deflt : String) : String = {
    val r = (elem \ prop).headOption.map(_.text)
    r match {
      case Some(r) => r
      case None => {
      System.err.println("Mandatory property " + prop + " is missing, defaulting to \"" + deflt + "\"")
      deflt
      }
    }
  }

  private def readLexicon(elem : Node) : Lexicon = {
    readMeta(Lexicon(
      attText(elem, "@id", "id"),
      attText(elem, "@label", ""),
      Language.get(attText(elem, "@language", "en")),
      attText(elem, "@email", ""),
      attText(elem, "@license", ""),
      attText(elem, "@version", ""),
      (elem \ "@url").headOption.map(_.text),
      (elem \ "@citation").headOption.map(_.text),
      (elem \ "LexicalEntry").map(readEntry),
      (elem \ "Synset").map(readSynset)), elem)
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
    (elem \ "@note").foreach(x => a.note = Some(x.text))
    (elem \ "@confidenceScore").foreach(x => a.confidenceScore = Some(x.text.toDouble))
    return a
  }

  private def readEntry(elem : Node) : LexicalEntry = {
    readMeta(LexicalEntry(
        (elem \ "@id").text,
      readLemma(
        (elem \ "Lemma").head),
      (elem \ "Form").map(readForm),
      (elem \ "Sense").map(readSense),
      (elem \ "SyntacticBehaviour").map(readSyntacticBehaviour)), elem)
  }

  private def readTag(elem : Node) : Tag = {
    Tag(
      (elem \ "@category").text,
      trim(elem).text)
  }

  private def readLemma(elem : Node) : Lemma = {
    Lemma(
      (elem \ "@writtenForm").text,
      readPartOfSpeech((elem \ "@partOfSpeech").text),
      (elem \ "@script").headOption.map(s => Script.getByAlpha4Code(s.text)),
      (elem \ "Tag").map(readTag))
  }

  private def readPartOfSpeech(code : String) = PartOfSpeech.fromString(code)

  private def readForm(elem : Node) : Form = {
    Form(
      (elem \ "@writtenForm").text,
      (elem \ "Tag").map(readTag),
      (elem \ "@script").headOption.map(s => Script.getByAlpha4Code(s.text)))
  }

  private def readSense(elem : Node) : Sense = {
    readMeta(Sense(
      (elem \ "@id").text,
      (elem \ "@synset").text,
      (elem \ "SenseRelation").map(readSenseRelation),
      (elem \ "Example").map(readSenseExample),
      (elem \ "Count").map(readCount),
      (elem \ "@adjposition").map(readAdjPosition).headOption), elem)
  }

  private def readAdjPosition(elem : Node) : AdjPosition = {
    elem.text match {
      case "a" => attributive
      case "p" => predicative
      case "ip" => postpositive
    }
  }

  private def readSenseRelation(elem : Node) : SenseRelation = {
    readMeta(SenseRelation(
      (elem \ "@target").text,
      readSenseRel((elem \ "@relType").text, (elem \ "@{http://purl.org/dc/elements/1.1/}type").headOption.map(_.text))), elem)
  }

  private def readSenseRel(code : String, `type` : Option[String]) = {
    SenseRelType.fromString(code, `type`)
  }

  private def readSenseExample(elem : Node) = {
    readMeta(Example(
      //trim(elem).text,
      elem.text.replaceAll("^\\s*","").replaceAll("\\s*$",""),
      (elem \ "@language").headOption.map(l => Language.get(l.text))), elem)
  }

  private def readSyntacticBehaviour(elem : Node) = {
    SyntacticBehaviour(
      (elem \ "@subcategorizationFrame").text,
      if((elem \ "@senses").isEmpty) { Nil } else { (elem \ "@senses").text.split(" ").toSeq })
  }

  private def readSynset(elem : Node) : Synset = {
    readMeta(Synset(
      (elem \ "@id").text,
      (elem \ "@ili").headOption.map(_.text),
      (elem \ "Definition").map(readDefinition),
      (elem \ "ILIDefinition").headOption.map(readILIDefinition),
      (elem \ "SynsetRelation").map(readSynsetRelation),
      (elem \ "Example").map(readSenseExample),
      (elem \ "@partOfSpeech").headOption.map(e => readPartOfSpeech(e.text))), elem)
  }

  private def readDefinition(elem : Node) : Definition = {
    readMeta(Definition(
      //trim(elem).text,
      elem.text.replaceAll("^\\s*","").replaceAll("\\s*$",""),
      (elem \ "@language").headOption.map(l => Language.get(l.text)),
      (elem \ "@sourceSense").headOption.map(_.text)), elem)
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
    SynsetRelType.fromString(code, `type`)
  }

  private def readCount(elem : Node) : Count = {
    readMeta(Count(elem.text.toInt), elem)
  }

  def write(resource : LexicalResource, output : File) {
    write(resource, new java.io.FileWriter(output), resource.entriesForSynset)
  }

  def write(resource : LexicalResource, output : File,
    entriesForSynset : Map[String, Seq[String]]) {
    write(resource, new java.io.FileWriter(output), entriesForSynset)
  }

  def write(resource : LexicalResource, output : Writer) {
    write(resource, output, resource.entriesForSynset)
  }

  def write(resource : LexicalResource, output : Writer,
    entriesForSynset : Map[String, Seq[String]] ) {
    val out = new PrintWriter(output)
    writeLexicalResource(out, resource, entriesForSynset)
    out.flush
    out.close
  } 

  import MoreStringEscapeUtils._

  private def writeLexicalResource(out : PrintWriter, e : LexicalResource,
    entriesForSynset : Map[String, Seq[String]]) {
    out.print("""<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE LexicalResource SYSTEM "http://globalwordnet.github.io/schemas/WN-LMF""" + (if(relaxed) { "-relaxed" } else { "" }) + """-1.0.dtd">
<LexicalResource xmlns:dc="http://purl.org/dc/elements/1.1/">""")
    for(lexicon <- e.lexicons) {
      writeLexicon(out, lexicon, entriesForSynset)
    }
    out.println("""
</LexicalResource>""")
  }

  private def writeLexicon(out : PrintWriter, e : Lexicon, entriesForSynset : Map[String, Seq[String]]) {
     out.print(s"""
  <Lexicon id="${escapeXmlId(e.id)}" 
           label="${escapeXml(e.label)}" 
           language="${e.language}"
           email="${escapeXml(e.email)}"
           license="${escapeXml(e.license)}"
           version="${escapeXml(e.version)}"""")
   e.citation.foreach(c =>
        out.print(s"""
           citation="${escapeXml(c)}""""))
    e.url.foreach(u =>
        out.print(s"""
           url="${escapeXml(u)}""""))
     writeMeta(out, 11, e)
    out.print(""">""")
    for(entry <- e.entries) {
      writeEntry(out, entry, entriesForSynset)
    }
    for(synset <- e.synsets) {
      writeSynset(out, synset, entriesForSynset)
    }
    out.print("""
  </Lexicon>""")
  }

  private def writeMeta(out : PrintWriter, indent : Int, e : Meta) {
    var line = false
    def writeProp(v : Option[String], n : String) = v match {
      case Some(x) =>
        if(line) {
          out.print("\n" + (" " * indent) + n + "=\"" + escapeXml(x) + "\"")
        } else {
          line = true
          out.print(" " + n + "=\"" + escapeXml(x) + "\"")
        }
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
    writeProp(e.note, "note")
    writeProp(e.confidenceScore.map(s => "%.8f" format s), "confidenceScore")
  }
 
  private def writeEntry(out : PrintWriter, e : LexicalEntry, entriesForSynset : Map[String, Seq[String]]) {
    out.print(s"""
    <LexicalEntry id="${escapeXmlId(e.id)}"""")
    writeMeta(out, 18, e)
    out.print(s""">
      <Lemma writtenForm="${escapeXml(e.lemma.writtenForm)}" partOfSpeech="${e.lemma.partOfSpeech.shortForm}"""")
    e.lemma.script match {
      case Some(t) =>
        out.print(s""" script="$t" """)
      case None =>
    }
    if(e.lemma.tag.isEmpty) {
      out.print("/>")
    } else {
      out.print(">")
      for(t <- e.lemma.tag) {
        out.print(s"""
        <Tag category="${escapeXml(t.category)}">${escapeXml(t.value)}</Tag>""")
      }
      out.print("""
      </Lemma>""")
    }
    for(form <- e.forms) {
      writeForm(out, form, entriesForSynset)
    }
    for(sense <- e.senses) {
      writeSense(out, sense, entriesForSynset)
    }
    for(synBeh <- e.syntacticBehaviours) {
      writeSyntacticBehaviour(out, synBeh, entriesForSynset)
    }
    out.print("""
    </LexicalEntry>""")
  }

  private def writeForm(out : PrintWriter, e : Form, entriesForSynset : Map[String, Seq[String]]) {
    out.print(s"""
      <Form writtenForm="${escapeXml(e.writtenForm)}"""")
    e.script match {
      case Some(s) =>
        out.print(s""" script="$s" """)
      case None =>
    }
    if(e.tag.isEmpty) {
      out.print("/>")
    } else {
      out.print(">")
      for(t <- e.tag) {
        out.print(s"""
        <Tag category="${escapeXml(t.category)}">${escapeXml(t.value)}</Tag>""")
      }
      out.print("""
      </Form>""")
    }
  }

  private def writeSense(out : PrintWriter, e : Sense, entriesForSynset : Map[String, Seq[String]]) {
    out.print(s"""
      <Sense id="${escapeXmlId(e.id)}" synset="${escapeXmlId(e.synsetRef)}"""")
    e.adjposition match {
      case Some(a) => out.print(s""" adjposition="${a.shortForm}"""")
      case None => {}
    }
    writeMeta(out, 13, e)
    if(e.senseRelations.isEmpty && e.senseExamples.isEmpty &&
      e.counts.isEmpty) {
        out.print("/>")
    } else {
      out.print(">")
      for(rel <- e.senseRelations) {
        writeSenseRel(out, rel, entriesForSynset)
      }
      for(x <- e.senseExamples) {
        writeSenseExample(out, x, entriesForSynset)
      }
      for(c <- e.counts) {
        writeCount(out, c)
      }
      out.print("""
        </Sense>""")
    }
  }

  private val pwnSenseId = "(.*)-.*-([nvarsp])-(\\d{8})-\\d{2}".r
  private def pwnSenseToSynset(targ : String) : String = targ match {
    case pwnSenseId(res, pos, id) => s"$res-$id-$pos"
    case _ => "not found"
  }

  private def writeSenseRel(out : PrintWriter, e : SenseRelation, entriesForSynset : Map[String, Seq[String]]) {
    out.print(s"""
        <SenseRelation relType="${e.relType.name}" target="${escapeXmlId(e.target)}"""")
    writeMeta(out, 23, e)
    out.print("/>")
    if(comments) {
      val targs = entriesForSynset.getOrElse(pwnSenseToSynset(e.target), Nil)
      if(!targs.isEmpty) {
        out.print(s" <!-- ${targs.mkString(", ")} -->")
      } else {
        out.print(s" <!-- ${pwnSenseToSynset(e.target)} ${targs.mkString(", ")} -->")
      }
    }
  }

  private def writeSenseExample(out : PrintWriter, e : Example, entriesForSynset : Map[String, Seq[String]]) {
    out.print(s"""
      <Example""")
    e.language match {
      case Some(l) =>
        out.print(s""" language="$l" """)
      case None =>
    }
    writeMeta(out, 22, e)
    out.print(s""">${escapeXml(e.content)}</Example>""")
  }

  private def writeSyntacticBehaviour(out : PrintWriter, e : SyntacticBehaviour, entriesForSynset : Map[String, Seq[String]]) {
    out.print(s"""
      <SyntacticBehaviour subcategorizationFrame="${escapeXml(e.subcategorizationFrame)}"""")
    if(!e.senses.isEmpty) {
      out.print(s""" senses="${e.senses.mkString(" ")}"""")
    }
    out.print("""/>""")
  }

  private def writeSynset(out : PrintWriter, e : Synset, entriesForSynset : Map[String, Seq[String]]) {
    if(comments) out.print(s"""
    <!-- ${entriesForSynset.getOrElse(e.id,Nil).mkString(", ")} -->""")
    out.print(s"""
    <Synset id="${escapeXmlId(e.id)}" ili="${e.ili.getOrElse("")}"""")
    e.partOfSpeech.foreach(x => out.print(s""" partOfSpeech="${x.shortForm}""""))
    writeMeta(out, 12, e)
    out.print(">")
    for(d <- e.definitions) {
      writeDefinition(out, d, entriesForSynset)
    }
    e.iliDefinition match {
      case Some(i) =>
        writeILIDefinition(out, i, entriesForSynset)
      case None =>
    }
    for(r <- e.synsetRelations) {
      writeSynsetRel(out, r, entriesForSynset)
    }
    for(x <- e.synsetExamples) {
      writeSenseExample(out, x, entriesForSynset)
    }
   out.print(s"""
    </Synset>""")
  }

  private def writeDefinition(out : PrintWriter, e : Definition, entriesForSynset : Map[String, Seq[String]]) {
    out.print(s"""
      <Definition""")
    e.language match {
      case Some(l) =>
        out.print(s""" language="$l" """)
      case None =>
    }
    e.sourceSense match {
      case Some(l) =>
        out.print(s""" sourceSense="${escapeXml(l)}" """)
      case None =>
    }
    writeMeta(out, 18, e)
    out.print(s""">${escapeXml(e.content)}</Definition>""")
  }

  private def writeILIDefinition(out : PrintWriter, e : ILIDefinition, entriesForSynset : Map[String, Seq[String]]) {
    out.print(s"""
      <ILIDefinition""")
    writeMeta(out, 21, e)
    out.print(s""">${escapeXml(e.content)}</ILIDefinition>""")
  }

  private def writeSynsetRel(out : PrintWriter, e : SynsetRelation, entriesForSynset : Map[String, Seq[String]]) {
    out.print(s"""
      <SynsetRelation relType="${e.relType.name}" target="${escapeXmlId(e.target)}"""")
    writeMeta(out, 22, e)
    out.print("/>")
    if(comments) out.print(s" <!-- ${entriesForSynset.getOrElse(e.target,Nil).mkString(", ")} -->")
  }


  private def writeCount(out : PrintWriter, e : Count) {
    out.print(s"""
      <Count""")
    writeMeta(out, 14, e)
    out.print(s""">${e.value}</Count>""")
  }
}
