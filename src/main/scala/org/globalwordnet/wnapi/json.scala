package org.globalwordnet.api.serialize

import eu.monnetproject.lang.{Language, Script}
import org.globalwordnet.api._
import org.globalwordnet.api.wn._
import java.io.{Writer, Reader, PrintWriter}
import spray.json._
import java.nio.charset.Charset

private class ReaderAsSprayParserInput(reader : Reader) extends ParserInput.DefaultParserInput {
  private final val EOI = '\uFFFF' 
  private final val NUL = '\u0000'
  private var lastChar : Char = NUL
  private var inString = false
  private var peekChar : Char = NUL
  private final val MAX_QUEUE = 1024
  private val queue = collection.mutable.Queue[Char]()
  def nextChar() : Char = {
    val c = _nextChar()
    queue.enqueue(c)
    if(queue.size > MAX_QUEUE) {
      queue.dequeue()
    }
    c
  }

  private def _nextChar() : Char = {
    peekChar match {
      case '\u0000' =>
      case c =>
        lastChar = c
        peekChar = NUL
        return c
    }
    try {
      val c = reader.read().toChar
      _cursor += 1
      if(c == '/' && !inString) {
        peekChar = reader.read().toChar
        _cursor += 1
        if(peekChar == '/') {
          while(lastChar != EOI) {
            _cursor += 1
            reader.read().toChar match {
              case '\n' =>
                peekChar = NUL
                return nextChar()
              case c => {
              }
            }
          }
        } else if(peekChar == '*') {
          lastChar = NUL
          while(lastChar != EOI) {
            _cursor += 1
            reader.read().toChar match {
              case '/' if lastChar == '*' =>
                lastChar = '/'
                peekChar = NUL
                return nextChar()
              case c =>
                lastChar = c
            }
          }
        }
      } else if(lastChar != '\\' && c == '\"') {
        inString = !inString
      }
      lastChar = c
      return c
    } catch {
      case eof : java.io.EOFException => 
        EOI
    }
  }
  def nextUtf8Char() = nextChar()
  def length = throw new RuntimeException("Requesting length of reader")
  def sliceCharArray(start : Int, end : Int) : Array[Char]= {
    if(start < _cursor && start > _cursor - MAX_QUEUE) {
      if(end <= _cursor) {
        queue.takeRight(_cursor - start + 1).take(end - start).toArray
      } else {
        queue.takeRight(_cursor - start + 1).toArray ++ sliceCharArray(_cursor, end)
      }
    } else {
      if(start < _cursor) {
        throw new RuntimeException("Seeking backwards: " + _cursor + " " + start + " " + end)
      } 
      reader.skip(start - _cursor)
      _cursor = start
      val buf = new Array[Char](end - start)
      reader.read(buf)
      buf
    }
  }
  def sliceString(start: Int, end: Int) = new String(sliceCharArray(start, end))
}

case class WNJsonException(msg : String = "", cause : Throwable = null) extends RuntimeException(msg, cause)

object WNJSON extends Format {
  object WNJSONFormat extends DefaultJsonProtocol {
    private def checkDrop(p : String, s : String) = if(s.startsWith(p)) {
      s.drop(p.length) 
    } else {
      throw new WNJsonException("Expected the string %s to start with %s" format (s,p))
    }
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
          a.note.map(x => "note" -> JsString(x)) ++
          a.confidenceScore.map(x => "confidenceScore" -> JsString("%.8f" format x)))
      }
      def read(v : JsValue) = v match {
        case v : JsObject =>
          val a = f.read(v)
          a.contributor = v.fields.get("contributor").map(stringOrFail)
          a.coverage = v.fields.get("coverage").map(stringOrFail)
          a.creator = v.fields.get("creator").map(stringOrFail)
          a.date = v.fields.get("date").map(stringOrFail)
          a.description = v.fields.get("description").map(stringOrFail)
          a.format = v.fields.get("format").map(stringOrFail)
          a.identifier = v.fields.get("identifier").map(stringOrFail)
          a.publisher = v.fields.get("publisher").map(stringOrFail)
          a.relation = v.fields.get("relation").map(stringOrFail)
          a.rights = v.fields.get("rights").map(stringOrFail)
          a.source = v.fields.get("source").map(stringOrFail)
          a.subject = v.fields.get("subject").map(stringOrFail)
          a.title = v.fields.get("title").map(stringOrFail)
          a.`type` = v.fields.get("type").map(stringOrFail)
          a.status = v.fields.get("status").map(stringOrFail)
          a.note = v.fields.get("note").map(stringOrFail)
          a.confidenceScore = v.fields.get("confidenceScore").map(numberOrFail).map(_.toDouble)
          a
        case _ =>
          throw new WNJsonException("Expected object")
      }
    }
    def stringOrFail(v : JsValue) = v match {
      case JsString(s) => s
      case _ => throw new WNJsonException("Expected string but got: " + v)
    }

    def numberOrFail(v : JsValue) = v match {
      case JsNumber(n) => n
      case JsString(s) => BigDecimal(s)
      case _ => throw new WNJsonException("Expected number but got: " + v)
    }

    object countFormat extends JsonFormat[Count] {
      def write(c : Count) = JsObject("value" -> JsNumber(c.value))
      def read(v : JsValue) = v match {
        case JsObject(m) => 
          Count(numberOrFail(m.getOrElse("value", throw new WNJsonException("Count needs a value"))).toInt)
        case _ =>
          throw new WNJsonException("Count must be an object")
      }
    }
    implicit val metaCountFormat = new MetaFormat(countFormat)

    implicit object tagFormat extends JsonFormat[Tag] {
      def write(t : Tag) = JsObject(Map(
        "category" -> JsString(t.category),
        "value" -> JsString(t.value)))

      def read(v : JsValue) = v match {
        case JsObject(m) => 
          Tag(
            category=stringOrFail(m.getOrElse("category", throw new WNJsonException("Tag needs a category"))),
            value=stringOrFail(m.getOrElse("value", throw new WNJsonException("Tag needs a value"))))
        case _ => throw new WNJsonException("Tag must be an object")
      }
    }

    implicit object formFormat extends JsonFormat[Form] {
      def write(f : Form) = JsObject(Map(
        "writtenForm" -> JsString(f.writtenForm)) ++
        (if(f.tag.isEmpty) Map[String,JsValue]() else Map("tag" -> JsArray(
          f.tag.map(tagFormat.write):_*))) ++
        f.script.map(s => "script" -> JsString(s.toString())))
      def read(v : JsValue) = v match {
        case JsObject(m) =>
          Form(
            writtenForm=stringOrFail(m.getOrElse("writtenForm", throw new WNJsonException("Form needs a written form"))),
            tag=m.getOrElse("tag", JsArray()) match {
              case JsArray(x) => x.map(tagFormat.read)
              case _ => throw new WNJsonException("Tag must be list of objects")
            },
            script=m.get("script").map(stringOrFail).map(Script.getByAlpha4Code))
        case _ =>
          throw new WNJsonException("Form must be an object")
      }
    }
    object definitionFormat extends JsonFormat[Definition] {
      def write(d : Definition) = JsObject(
        Map("gloss" -> JsString(d.content)) ++
          d.language.map(l => "language" -> JsString(l.toString())) ++
          d.sourceSense.map(s => "sourceSense" -> JsString(s)))
      def read(v : JsValue) = v match {
        case JsObject(m) =>
          Definition(content=stringOrFail(m.getOrElse("gloss", throw new WNJsonException("Definition requires gloss"))), 
                     language=m.get("language").map(stringOrFail).map(Language.get),
                     sourceSense=m.get("sourceSense").map(stringOrFail))
        case _ =>
          throw new WNJsonException("Definition must be an object")
      }
    }
    implicit val metaDefinitionFormat = new MetaFormat(definitionFormat)
    object iliDefinitionFormat extends JsonFormat[ILIDefinition] {
      def write(d : ILIDefinition) = JsObject(
        "gloss" -> JsString(d.content))
      def read(v : JsValue) = v match {
        case JsObject(m) =>
          ILIDefinition(stringOrFail(m.getOrElse("gloss", throw new WNJsonException("ILI Definition requires gloss"))))
        case _ =>
          throw new WNJsonException("ILI Definition must be an object")
      }
    }
    implicit val metaILIDefinitionFormat = new MetaFormat(iliDefinitionFormat)

    object senseExampleFormat extends JsonFormat[Example] {
      def write(e : Example) = JsObject(Map(
        "value" -> JsString(e.content)) ++
        e.language.map(l => "language" -> JsString(l.toString())))
      def read(v : JsValue) = v match {
        case JsObject(m) =>
          Example(
            content=stringOrFail(m.getOrElse("value", throw new WNJsonException("Example requires value"))),
            language=m.get("language").map(stringOrFail).map(Language.get))
        case _ =>
          throw new WNJsonException("Sense example must be an object")
      }
    }
    implicit val metaExampleFormat = new MetaFormat(senseExampleFormat)
    object synsetRelationFormat extends JsonFormat[SynsetRelation] {
      def write(r : SynsetRelation) = r.relType match {
        case other(x) => JsObject(
          "type" -> JsString(x),
          "target" -> JsString(r.target))
        case _ => JsObject(
          "category" -> JsString("wn:" + r.relType.name),
          "target" -> JsString(r.target))
      }
      def read(v : JsValue) = v match {
        case JsObject(m) =>
          SynsetRelation(
            target=stringOrFail(m.getOrElse("target", throw new WNJsonException("Synset relation requires target"))),
            relType=m.get("category") match {
              case Some(c) if stringOrFail(c).startsWith("wn:") => SynsetRelType.fromString(checkDrop("wn:", stringOrFail(c)), None)
              case _ => other(stringOrFail(m.getOrElse("type", throw new WNJsonException("Relation must have category or type"))))
            })
        case _ =>
          throw new WNJsonException("Synset relation must be an object")
      }
    }
    implicit val metaSynsetRelationFormat = new MetaFormat(synsetRelationFormat)

    object senseRelationFormat extends JsonFormat[SenseRelation] {
      def write(r : SenseRelation) = r.relType match {
        case other(x) => JsObject(
          "type" -> JsString(x),
          "target" -> JsString(r.target))
        case _ => JsObject(
        "category" -> JsString("wn:" + r.relType.name),
        "target" -> JsString(r.target))
      }
      def read(v : JsValue) = v match {
        case JsObject(m) =>
          SenseRelation(
            target=stringOrFail(m.getOrElse("target", throw new WNJsonException("Sense relation requires target"))),
            relType=m.get("category") match {
              case Some(c) if stringOrFail(c).startsWith("wn:") => SenseRelType.fromString(checkDrop("wn:", stringOrFail(c)), None)
              case _ => other(stringOrFail(m.getOrElse("type", throw new WNJsonException("Relation must have category or type"))))
            })
        case _ =>
          throw new WNJsonException("Sense relation must be an object")
      }
    }
    implicit val metaSenseRelationFormat = new MetaFormat(senseRelationFormat)

    object senseFormat extends JsonFormat[Sense] {
      def write(s : Sense) = new JsObject(Map(
          "@id" -> JsString(s.id),
          "synset" -> JsString(s.synsetRef)) ++
          (s.counts.map(metaCountFormat.write).toList match {
            case Nil => Map()
            case vals => Map("count" -> JsArray(vals:_*))
          }) ++
          (s.senseRelations.map(metaSenseRelationFormat.write).toList match {
             case Nil => Map()
             case vals => Map("relations" -> JsArray(vals:_*))
          }) ++
          (s.senseExamples.map(metaExampleFormat.write).toList match {
            case Nil => Map()
            case vals => Map("example" -> JsArray(vals:_*))
          }))
      def read(v : JsValue) = v match {
        case JsObject(m) =>
          Sense(
            senseRelations=m.getOrElse("relations", JsArray()) match {
              case JsArray(x) => x.map(metaSenseRelationFormat.read)
              case _ => throw new WNJsonException("Relations should be a list of values")
            },
            senseExamples=m.getOrElse("example", JsArray()) match {
              case JsArray(x) => x.map(metaExampleFormat.read)
              case _ => throw new WNJsonException("Examples should be a list of values")
            },
            id=stringOrFail(m.getOrElse("@id", throw new WNJsonException("Sense must have an id"))),
            synsetRef=stringOrFail(m.getOrElse("synset", throw new WNJsonException("Sense must have a synset"))),
            counts=m.getOrElse("count", JsArray()) match {
              case JsArray(x) => x.map(metaCountFormat.read)
              case _ => throw new WNJsonException("Counts should be a list of values")
            })
        case _ =>
          throw new WNJsonException("Sense should be an object")
      }
    }
    implicit val metaSenseFormat = new MetaFormat(senseFormat)

    implicit object syntacticBehaviourFormat extends JsonFormat[SyntacticBehaviour] {
      def write(e : SyntacticBehaviour) = JsObject("label" -> JsString(e.subcategorizationFrame))
      def read(v : JsValue) = v match {
        case JsObject(m) => SyntacticBehaviour(stringOrFail(m.getOrElse("label", throw new WNJsonException("Syntactic Behaviour must have a label"))))
        case _ =>
          throw new WNJsonException("Syntactic Behaviour must be an object")
      }
    }

    object lexicalEntryFormat extends JsonFormat[LexicalEntry] {
      def write(e : LexicalEntry) = JsObject(Map(
        "lemma" -> JsObject(Map("writtenForm" -> JsString(e.lemma.writtenForm)) ++
          e.lemma.script.map(x => "script" -> JsString(x.toString())) ++
          (if(e.lemma.tag.isEmpty) Map[String, JsValue]() else Map("tag" ->
            JsArray(e.lemma.tag.map(tagFormat.write):_*)))
        ),
        "partOfSpeech" -> JsString("wn:" + e.lemma.partOfSpeech.name),
        "@id" -> JsString(e.id)) ++
        (e.forms.map(formFormat.write).toList match {
          case Nil => Map()
          case vals => Map("form" -> JsArray(vals:_*))
        }) ++
        (e.senses.map(metaSenseFormat.write).toList match {
          case Nil => Map()
          case vals => Map("sense" -> JsArray(vals:_*))
        }) ++
        (e.syntacticBehaviours.map(syntacticBehaviourFormat.write).toList match {
          case Nil => Map()
          case vals => Map("synBehavior" -> JsArray(vals:_*))
        }))
      def read(v : JsValue) = v match {
        case JsObject(m) =>
          LexicalEntry(
            lemma=Lemma(
              m.getOrElse("lemma", throw new WNJsonException("Lexical entry must have a lemma")) match {
                case JsObject(m) => stringOrFail(m.getOrElse("writtenForm", throw new WNJsonException("Lemma must have a written form")))
                case _ => throw new WNJsonException("Lemma must be an object")
              },
              PartOfSpeech.fromName(checkDrop("wn:", stringOrFail(m.getOrElse("partOfSpeech", throw new WNJsonException("Lexical entry must have a part of speech"))))),
              m.get("lemma").flatMap({
                  case JsObject(m) => m.get("script").map(stringOrFail).map(Script.getByAlpha4Code)
                  case _ => throw new WNJsonException("Lemma must be an object")
                }),
              m.get("lemma").flatMap({
                case JsObject(m) => m.get("tag")
                case _ => throw new WNJsonException("Lemma must be an object")
              }) match {
                case Some(JsArray(v)) => v.map(tagFormat.read)
                case None => Nil
                case _ => throw new WNJsonException("Tag must be an array")
              }
            ),
            forms=m.getOrElse("form", JsArray()) match {
              case JsArray(x) => x.map(formFormat.read)
              case _ => throw new WNJsonException("Form must be a list of objects")
            },
            senses=m.getOrElse("sense", JsArray()) match {
              case JsArray(x) => x.map(metaSenseFormat.read)
              case _ => throw new WNJsonException("Sense must be a list of objects")
            },
            syntacticBehaviours=m.getOrElse("synBehavior", JsArray()) match {
              case JsArray(x) => x.map(syntacticBehaviourFormat.read)
              case _ => throw new WNJsonException("Syntactic behaviour must be a list of objects")
            },
            id=stringOrFail(m.getOrElse("@id", throw new WNJsonException("Lexical entry must have an id"))))
        case _ =>
          throw new WNJsonException("Lexical entry must be an object")
      }
    }
    implicit val metaLexicalEntryFormat = new MetaFormat(lexicalEntryFormat)

    object synsetFormat extends JsonFormat[Synset] {
      def write(s : Synset) = JsObject(Map(
        "@id" -> JsString(s.id)) ++
        (s.definitions.map(metaDefinitionFormat.write).toList match {
          case Nil => Map()
          case vals => Map("definition" -> JsArray(vals:_*))
        }) ++
        (s.synsetRelations.map(metaSynsetRelationFormat.write).toList match {
          case Nil => Map()
          case vals => Map("relations" -> JsArray(vals:_*))
        }) ++
        (s.ili.map(x => Map("ili" -> JsString("ili:" + x)))).getOrElse(Map()) ++
        (s.synsetExamples.map(metaExampleFormat.write).toList match {
          case Nil => Map()
          case vals => Map("example" -> JsArray(vals:_*))
        }) ++
        (s.iliDefinition.map(x => Map("iliDefinition" -> metaILIDefinitionFormat.write(x))).getOrElse(Map())))
      def read(v : JsValue) = v match {
        case JsObject(m) =>
          Synset(
            definitions=m.getOrElse("definition", JsArray()) match {
              case JsArray(x) => x.map(metaDefinitionFormat.read)
              case _ => throw new WNJsonException("Definition must be list of objects")
            },
            iliDefinition=m.get("iliDefinition").map(metaILIDefinitionFormat.read),
            synsetRelations=m.getOrElse("relations", JsArray()) match {
              case JsArray(x) => x.map(metaSynsetRelationFormat.read)
              case _ => throw new WNJsonException("Synset link must be list of objects")
            },
            id=stringOrFail(m.getOrElse("@id", throw new WNJsonException("Synset must have an ID"))),
            ili=m.get("ili").map(x => checkDrop("ili:", stringOrFail(x))),
            synsetExamples=m.getOrElse("example", JsArray()) match {
              case JsArray(x) => x.map(metaExampleFormat.read)
              case _ => throw new WNJsonException("Synset exampels must be list of objects")
            })
        case _ =>
          throw new WNJsonException("Synset must be an object")
      }
    }
    implicit val metaSynsetFormat = new MetaFormat(synsetFormat)

    object lexiconFormat extends JsonFormat[Lexicon] {
      def write(l : Lexicon) = JsObject(Map(
        "@context" -> JsObject("@language" -> JsString(l.language.toString())),
        "@id" -> JsString(l.id),
        "@type" -> JsString("ontolex:Lexicon"),
        "label" -> JsString(l.label),
        "language" -> JsString(l.language.toString()),
        "email" -> JsString(l.email),
        "license" -> JsString(l.license),
        "version" -> JsString(l.version)) ++
        (l.entries.map(metaLexicalEntryFormat.write).toList match {
          case Nil => Map()
          case vals => Map("entry" -> JsArray(vals:_*))
        }) ++
        (l.synsets.map(metaSynsetFormat.write).toList match {
          case Nil => Map()
          case vals => Map("synset" -> JsArray(vals:_*))
        }) ++
        l.url.map(u => Map("url" -> JsString(u))).getOrElse(Map()) ++
        l.citation.map(u => Map("citation" -> JsString(u))).getOrElse(Map()))
      def read(v : JsValue) = v match {
        case JsObject(m) =>
          Lexicon(
            entries=m.getOrElse("entry", JsArray()) match {
              case JsArray(x) => x.map(metaLexicalEntryFormat.read)
              case _ => throw new IllegalArgumentException("Entries should be a list of objects")
            },
            synsets=m.getOrElse("synset", JsArray()) match {
              case JsArray(x) => x.map(metaSynsetFormat.read)
              case _ => throw new IllegalArgumentException("Synsets should be a list of objects")
            },
            id=stringOrFail(m.getOrElse("@id", throw new WNJsonException("ID is required on a lexicon"))),
            label=stringOrFail(m.getOrElse("label", throw new WNJsonException("Label is required on a lexicon"))),
            language=Language.get(stringOrFail(m.getOrElse("language", throw new WNJsonException("Language is required on a lexicon")))),
            email=stringOrFail(m.getOrElse("email", throw new WNJsonException("Email is required on a lexicon"))),
            license=stringOrFail(m.getOrElse("license", throw new WNJsonException("License is required on a lexicon"))),
            version=stringOrFail(m.getOrElse("version", throw new WNJsonException("Version is required on a lexicon"))),
            url=m.get("url").map(stringOrFail),
            citation=m.get("citation").map(stringOrFail))
        case _ =>
          throw new WNJsonException("Lexicon must be an object")
      }
    }
    implicit val metaLexiconFormat = new MetaFormat(lexiconFormat)
    implicit object lexicalResourceFormat extends JsonFormat[LexicalResource] {
      def write(lr : LexicalResource) = JsObject(
        "@context" -> JsString("http://globalwordnet.github.io/schemas/wn-json-context-1.0.json"),
        "@graph" -> JsArray(lr.lexicons.map(metaLexiconFormat.write):_*))
      def read(v : JsValue) = v match {
        case v : JsObject =>
          LexicalResource(
            v.fields.get("@graph") match {
              case Some(JsArray(vs)) => vs.map(metaLexiconFormat.read)
              case Some(x) => throw new WNJsonException("@graph is not a list of objects")
              case None => throw new WNJsonException("No @graph")
            })
        case _ =>
          throw new WNJsonException("Lexical resource is not an object")
      }
    }
  }
  def read(file : java.io.File) : LexicalResource = read(new java.io.FileReader(file))

  def read(input : Reader) : LexicalResource = {
    import WNJSONFormat._
    JsonParser(new ReaderAsSprayParserInput(input)).convertTo[LexicalResource]
  }

  private def prettyWrite(out : PrintWriter, v : JsValue, indent : Int) {
    v match {
      case JsObject(m) =>
        out.println("{")
        var first = true
        for((key, value) <- m) {
          if(first) {
            first = false
          } else {
            out.println(",")
          } 
          out.print("  " * (indent + 1))
          out.print("\"")
          out.print(key.replaceAll("\\\"","\\\\\\\""))
          out.print("\": ")
          prettyWrite(out, value, indent + 1)
        }
        out.println()
        out.print("  " * indent)
        out.print("}")
      case JsArray(value) =>
        out.print("[")
        var first = true
        for(v <- value) {
          if(first) {
            first = false
          } else {
            out.print(", ")
          }
          prettyWrite(out, v, indent)
        }
        out.print("]")
      case JsString(s) =>
        out.print("\"%s\"" format(s.replaceAll("\\\"", "\\\\\\\"")))
      case JsFalse =>
        out.print("false")
      case JsTrue =>
        out.print("true")
      case JsNull =>
        out.print("null")
      case JsNumber(n) =>
        out.print(n.toString)
    }
  }

  def write(resource : LexicalResource, file : java.io.File) = 
    write(resource, new java.io.FileWriter(file))

  def write(resource : LexicalResource, output : Writer) = {
    import WNJSONFormat._
    prettyWrite(new PrintWriter(output), resource.toJson, 0)
    output.flush
  }
}
