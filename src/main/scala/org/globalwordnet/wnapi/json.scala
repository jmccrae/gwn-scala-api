package org.globalwordnet.api.serialize

import org.globalwordnet.api.wn._
import java.io.Reader
import spray.json._
import java.nio.charset.Charset

private class ReaderAsSprayParserInput(reader : Reader) extends ParserInput.DefaultParserInput {
  private final val EOI = '\uFFFF' 
  def nextChar() : Char = {
    try {
      val c = reader.read()
      _cursor += 1
      c.toChar
    } catch {
      case eof : java.io.EOFException => 
        EOI
    }
  }
  def nextUtf8Char() = nextChar()
  def length = throw new RuntimeException()
  def sliceCharArray(start : Int, end : Int) = {
    if(start < _cursor) {
      throw new RuntimeException()
    } 
    reader.skip(start - _cursor)
    _cursor = start
    val buf = new Array[Char](end - start)
    reader.read(buf)
    buf
  }
  def sliceString(start: Int, end: Int) = new String(sliceCharArray(start, end))
}

case class WNJsonException(msg : String = "", cause : Throwable = null) extends RuntimeException(msg, cause)

object WNJSON {
  object WNJSONFormat extends DefaultJsonProtocol {
    class MetaFormat[A <: Meta](f : JsonFormat[A]) extends JsonFormat[A] {
      def write(a : A) = {
        val jo = f.write(a).asInstanceOf[JsObject]
        JsObject(jo.fields ++
          a.contributor.map(x => "contributor" -> JsString(x)) ++
          a.coverage.map(x => "coverage" -> JsString(x)) ++
          a.creator.map(x => "creator" -> JsString(x)) ++
          a.date.map(x => "date" -> JsString(x)) ++
          a.description.map(x => "description" -> JsString(x)) ++
          a.format.map(x => "format" -> JsString(x)) ++
          a.identifier.map(x => "identifier" -> JsString(x)) ++
          a.publisher.map(x => "publisher" -> JsString(x)) ++
          a.relation.map(x => "relation" -> JsString(x)) ++
          a.rights.map(x => "rights" -> JsString(x)) ++
          a.source.map(x => "source" -> JsString(x)) ++
          a.subject.map(x => "subject" -> JsString(x)) ++
          a.title.map(x => "title" -> JsString(x)) ++
          a.`type`.map(x => "type" -> JsString(x)) ++
          a.status.map(x => "status" -> JsString(x)) ++
          a.confidenceScore.map(x => "confidenceScore" -> JsString("%.8f" format x)))
      }
      def read(v : JsValue) = v match {
        case v : JsObject =>
          val a = f.read(v)
          a.contributor = v.fields.get("contributor").map(_.toString)
          a.coverage = v.fields.get("coverage").map(_.toString)
          a.creator = v.fields.get("creator").map(_.toString)
          a.date = v.fields.get("date").map(_.toString)
          a.description = v.fields.get("description").map(_.toString)
          a.format = v.fields.get("format").map(_.toString)
          a.identifier = v.fields.get("identifier").map(_.toString)
          a.publisher = v.fields.get("publisher").map(_.toString)
          a.relation = v.fields.get("relation").map(_.toString)
          a.rights = v.fields.get("rights").map(_.toString)
          a.source = v.fields.get("source").map(_.toString)
          a.subject = v.fields.get("subject").map(_.toString)
          a.title = v.fields.get("title").map(_.toString)
          a.`type` = v.fields.get("type").map(_.toString)
          a.status = v.fields.get("status").map(_.toString)
          a.confidenceScore = v.fields.get("confidenceScore").map(_.toString.toDouble)
          a
        case _ =>
          throw new WNJsonException("Expected object")
      }
    }
    def stringOrFail(v : JsValue) = v match {
      case JsString(s) => s
      case _ => throw new WNJsonException("Expected String but got: " + v)
    }

    implicit object partOfSpeechFormat extends JsonFormat[PartOfSpeech] {
      def write(p : PartOfSpeech) = JsString(p.shortForm)
      def read(v : JsValue) = v match {
        case JsString(s) => PartOfSpeech.fromString(s)
      }
    }
    implicit val lemmaFormat = jsonFormat2(Lemma)
    implicit val formFormat = jsonFormat2(Form)
    object definitionFormat extends JsonFormat[Definition] {
      def write(d : Definition) = JsObject(
        ("gloss" -> JsString(d.content)) :: 
          d.language.map(l => "language" -> JsString(l)).toList)
      def read(v : JsValue) = v match {
        case JsObject(m) =>
          Definition(stringOrFail(m("gloss")), 
                     m.get("language").map(stringOrFail))
      }
    }
    implicit val metaDefinitionFormat = new MetaFormat(definitionFormat)
    object iliDefinitionFormat extends JsonFormat[ILIDefinition] {
      def write(d : ILIDefinition) = JsObject(
        "gloss" -> JsString(d.content))
      def read(v : JsValue) = v match {
        case JsObject(m) =>
          ILIDefinition(stringOrFail(m("gloss")))
      }
    }
    implicit val metaILIDefinitionFormat = new MetaFormat(iliDefinitionFormat)

    object senseExampleFormat extends JsonFormat[SenseExample] {
      def write(e : SenseExample) = JsObject(
        "value" -> JsString(e.content))
      def read(v : JsValue) = v match {
        case JsObject(m) =>
          SenseExample(stringOrFail(m("value")))
      }
    }
    implicit val metaSenseExampleFormat = new MetaFormat(senseExampleFormat)
    implicit object synsetRelTypeFormat extends JsonFormat[SynsetRelType] {
      def write(p : SynsetRelType) = JsString(p.name)
      def read(v : JsValue) = v match {
        case JsString(s) => SynsetRelType.fromString(s)
      }
    }
    implicit object senseRelTypeFormat extends JsonFormat[SenseRelType] {
      def write(p : SenseRelType) = JsString(p.name)
      def read(v : JsValue) = v match {
        case JsString(s) => SenseRelType.fromString(s)
      }
    }
    object synsetRelationFormat extends JsonFormat[SynsetRelation] {
      def write(r : SynsetRelation) = JsObject()
      def read(v : JsValue) = SynsetRelation("", hypernym)
    }
    implicit val metaSynsetRelationFormat = new MetaFormat(synsetRelationFormat)
    object senseRelationFormat extends JsonFormat[SenseRelation] {
      def write(r : SenseRelation) = JsObject()
      def read(v : JsValue) = SenseRelation("", antonym)
    }
    implicit val metaSenseRelationFormat = new MetaFormat(senseRelationFormat)
    object senseFormat extends JsonFormat[Sense] {
      def write(s : Sense) = new JsObject(
        Map("example" -> JsArray(s.senseExamples.map(metaSenseExampleFormat.write).toList)))
      def read(v : JsValue) = v match {
        case v : JsObject =>
          Sense(Nil, v.fields.get("example") match {
            case Some(JsArray(v)) => v.map(metaSenseExampleFormat.read)
            case None => Nil
            case _ => throw new WNJsonException()
          }, "", "")
      }
    }
    implicit val metaSenseFormat = new MetaFormat(senseFormat)
    implicit val syntacticBehaviourFormat = jsonFormat1(SyntacticBehaviour)
    object lexicalEntryFormat extends JsonFormat[LexicalEntry] {
      def write(e : LexicalEntry) = new JsObject(Map(
        "lemma" -> lemmaFormat.write(e.lemma)))
      def read(v : JsValue) = v match {
        case v : JsObject =>
          LexicalEntry(Lemma("", noun), Nil, Nil, Nil, "")
      }
    }
    implicit val metaLexicalEntryFormat = new MetaFormat(lexicalEntryFormat)
    object synsetFormat extends JsonFormat[Synset] {
      def write(s : Synset) = JsObject()
      def read(v : JsValue) = v match {
        case v : JsObject =>
          Synset(Nil, None, Nil, "", "", None)
      }
    }
    implicit val metaSynsetFormat = new MetaFormat(synsetFormat)
    object lexiconFormat extends JsonFormat[Lexicon] {
      def write(l : Lexicon) = JsObject()
      def read(v : JsValue) = v match {
        case v : JsObject =>
          Lexicon(
            v.fields.get("entry") match {
              case Some(JsArray(vs)) => vs.map(metaLexicalEntryFormat.read)
            },            
            Nil, "", "", "", "", "", "", None, None)
      }
    }
    implicit val metaLexiconFormat = new MetaFormat(lexiconFormat)
    implicit object lexicalResourceFormat extends JsonFormat[LexicalResource] {
      def write(lr : LexicalResource) = JsObject(
        "@context" -> JsString("http://globalwordnet.github.io/schemas/wn-json-context-1.0.json"),
        "@graph" -> JsArray(lr.lexicons.map(metaLexiconFormat.write).toList))
      def read(v : JsValue) = v match {
        case v : JsObject =>
          LexicalResource(
            v.fields.get("@graph") match {
              case Some(JsArray(vs)) => vs.map(metaLexiconFormat.read)
            })
      }
    }
  }

  def read(input : Reader) : LexicalResource = {
    import WNJSONFormat._
    JsonParser(new ReaderAsSprayParserInput(input)).convertTo[LexicalResource]
  }

}
