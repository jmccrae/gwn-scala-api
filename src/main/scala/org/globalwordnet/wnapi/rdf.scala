package org.globalwordnet.api.serialize

import eu.monnetproject.lang.{Language, Script}
import java.io.{File, FileReader, Reader, Writer, FileWriter}
import org.globalwordnet.api._
import org.globalwordnet.api.wn._
import org.apache.jena.rdf.model.{Model, ModelFactory, Resource, Property, RDFNode, Literal}
import org.apache.jena.vocabulary.{RDF, RDFS, DC_11, SKOS, OWL, XSD}
//import scala.collection.JavaConverters._
import scala.jdk.CollectionConverters._
import scala.language.dynamics
import scala.language.reflectiveCalls

case class WNRDFException(msg : String = "", cause : Throwable = null) extends RuntimeException(msg, cause)

class WNRDF(shortRelations : Boolean = false) extends Format {
  // To make log4j shut up :)
  org.apache.log4j.BasicConfigurator.configure()

  class NameSpace(val prefix : String) extends Dynamic {
    def apply(suffix : String)(implicit model : Model) = model.createResource(prefix + suffix)
    def selectDynamic(suffix : String)(implicit model : Model) = model.createResource(prefix + suffix)
  }

  private implicit class PimpedResource(resource : Resource) {
    def \(p : Property)(implicit model : Model) : Iterator[RDFNode] = {
      model.listObjectsOfProperty(resource, p).asScala.iterator
    }
    def \(r : Resource)(implicit model : Model) : Iterator[RDFNode] = {
      \(model.createProperty(r.getURI()))
    }
    def \*(r : Property)(implicit model : Model) : Iterator[Resource] = {
      model.listObjectsOfProperty(resource, r).asScala.flatMap({
        case r : Resource => Some(r)
        case _ => None
      })
    }
    def \*(r : Resource)(implicit model : Model) : Iterator[Resource] = \*(model.createProperty(r.getURI()))(model)
    def /(r : Resource)(implicit model : Model) : Iterator[Resource] = /(model.createProperty(r.getURI()))(model)
    def /(p : Property)(implicit model : Model) : Iterator[Resource] = {
      model.listSubjectsWithProperty(p, resource).asScala.iterator
    }
    def lit(r : Resource)(implicit model : Model) : Iterator[Literal] = lit(model.createProperty(r.getURI()))(model)
    def lit(p : Property)(implicit model : Model) : Iterator[Literal] = {
      model.listObjectsOfProperty(resource, p).asScala.flatMap({
        case l : Literal => Some(l)
        case _ => None
      })
    }
    def +(p : Property)(implicit model : Model) = new {
      def ++(vs : Seq[RDFNode]) = vs.foreach({v =>
        resource.addProperty(p, v)
      })
      def +(v : RDFNode) : Unit = {
        resource.addProperty(p ,v)
      }
    }
    def +(r : Resource)(implicit model : Model) = new {
      def ++(vs : Seq[RDFNode]) = vs.foreach({v =>
        resource.addProperty(model.createProperty(r.getURI()), v)
      })
      def +(v : RDFNode) : Unit = {
        resource.addProperty(model.createProperty(r.getURI()) ,v)
      }
    }
    def -(p : Property)(implicit model : Model) = new {
      def ++(vs : Seq[Resource]) = vs.foreach({v =>
        v.addProperty(p, resource)
      })
      def +(v : Resource) : Unit = {
        v.addProperty(p ,resource)
      }
    }
    def -(r : Resource)(implicit model : Model) = new {
      def ++(vs : Seq[Resource]) = vs.foreach({v =>
        v.addProperty(model.createProperty(r.getURI()), resource)
      })
      def +(v : Resource) : Unit = {
        v.addProperty(model.createProperty(r.getURI()), resource)
      }
    }


  }

  private implicit class PimpedRDFNodeIterator[A <: RDFNode](iter : Iterator[A]) {
    def headOrElse(foo : => A) = {
      if(iter.hasNext) {
        iter.next()
      } else {
        foo
      }
    }

    def headOption = {
      if(iter.hasNext) {
        Some(iter.next())
      } else {
        None
      }
    }
  }

  val WN = new NameSpace("https://globalwordnet.github.io/schemas/wn#")
  val ONTOLEX = new NameSpace("http://www.w3.org/ns/lemon/ontolex#")
  val SYNSEM = new NameSpace("http://www.w3.org/ns/lemon/synsem#")
  val VARTRANS = new NameSpace("http://www.w3.org/ns/lemon/vartrans#")
  val LIME = new NameSpace("http://www.w3.org/ns/lemon/lime#")
  val SCHEMA = new NameSpace("http://schema.org/")
  val CC = new NameSpace("http://creativecommons.org/ns#")
  val ILI = new NameSpace("http://ili.globalwordnet.org/ili/")
  val namespaces = Map("wn" -> WN, "ontolex" -> ONTOLEX, "synsem" -> SYNSEM,
    "vartrans" -> VARTRANS, "lime" -> LIME, "schema" -> SCHEMA, 
    "cc" -> CC, "ili" -> ILI, "skos" -> new NameSpace(SKOS.getURI()))

  def read(file : File) : LexicalResource = {
    read(new FileReader(file), WNRDF.guessLang(file), file.toURI().toString() + "#")
  }

  def read(file : File, lang : String) : LexicalResource = {
    read(new FileReader(file), lang, file.toURI().toString() + "#")
  }

  def read(file : File, lang : String, baseUrl : String) : LexicalResource = {
    read(new FileReader(file), lang, baseUrl)
  }

  def read(input : Reader, lang : String, baseUrl : String) : LexicalResource = {
    implicit val model = ModelFactory.createDefaultModel()
    model.read(input, baseUrl, lang)
    readLexicalResource
  }

  private def toId(r : Resource) : String = {
    if(r.isURIResource()) {
      r.getLocalName()
    } else {
      r.getId().getLabelString()
    }
  }

  private def toOptionalId(r : Resource) : Option[String] = {
    if(r.isURIResource()) {
      Some(r.getLocalName())
    } else {
      None
    }
  }

  private def readLexicalResource(implicit model : Model) : LexicalResource = {
    LexicalResource(model.listSubjectsWithProperty(RDF.`type`, LIME.Lexicon).asScala.map(readLexicon _).toSeq)
  }

  private def readLexicon(r : Resource)(implicit model : Model) : Lexicon = {
    readMeta(Lexicon(
      toId(r),
      (r lit RDFS.label).headOrElse(throw new WNRDFException("Label is required")).getLexicalForm(),
      Language.get((r lit DC_11.language).headOrElse(throw new WNRDFException("Language is required")).getLexicalForm()),
      (r lit SCHEMA.email).headOrElse(throw new WNRDFException("Email is required")).getLexicalForm(),
      (r \* CC.license).headOrElse(throw new WNRDFException("License is required")).getURI(),
      (r lit OWL.versionInfo).headOrElse(throw new WNRDFException("Version is required")).getLexicalForm(),
      (r lit SCHEMA.url).headOption.map(_.getLexicalForm()),
      (r lit SCHEMA.citation).headOption.map(_.getLexicalForm()),
      Nil, // (r \* WN.requires).map(readRequires).toSeq,
      (r \* LIME.entry).map(readLexicalEntry).toSeq,
      (r / SKOS.inScheme).map(readSynset).toSeq), r)
  }

  private def readMeta[A <: Meta](a : A, r : Resource)(implicit model : Model) : A = {
    (r lit DC_11.contributor).to(LazyList).headOption match {
      case Some(l) => 
        a.contributor = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_11.coverage).to(LazyList).headOption match {
      case Some(l) => 
        a.coverage = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_11.creator).to(LazyList).headOption match {
      case Some(l) => 
        a.creator = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_11.date).to(LazyList).headOption match {
      case Some(l) => 
        a.date = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_11.description).to(LazyList).headOption match {
      case Some(l) => 
        a.description = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_11.format).to(LazyList).headOption match {
      case Some(l) => 
        a.format = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_11.identifier).to(LazyList).headOption match {
      case Some(l) => 
        a.identifier = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_11.publisher).to(LazyList).headOption match {
      case Some(l) => 
        a.publisher = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_11.relation).to(LazyList).headOption match {
      case Some(l) => 
        a.relation = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_11.rights).to(LazyList).headOption match {
      case Some(l) => 
        a.rights = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_11.source).to(LazyList).headOption match {
      case Some(l) => 
        a.source = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_11.subject).to(LazyList).headOption match {
      case Some(l) => 
        a.subject = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_11.title).to(LazyList).headOption match {
      case Some(l) => 
        a.title = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_11.`type`).to(LazyList).headOption match {
      case Some(l) => 
        a.`type` = Some(l.getLexicalForm())
      case None =>
    }
    (r lit WN.status).to(LazyList).headOption match {
      case Some(l) => 
        a.status = Some(l.getLexicalForm())
      case None =>
    }
    (r lit WN.note).to(LazyList).headOption match {
      case Some(l) => 
        a.note = Some(l.getLexicalForm())
      case None =>
    }
    (r lit WN.confidenceScore).to(LazyList).headOption match {
      case Some(l) => 
        a.confidenceScore = Some(l.getDouble())
      case None =>
    }
    a
  }

  private def readLexicalEntry(r : Resource)(implicit model : Model) : LexicalEntry = {
    readMeta(LexicalEntry(
      toId(r),
      readLemma(
        (r \* ONTOLEX.canonicalForm).headOrElse(throw new WNRDFException("No canonical form for " + r)),
        (r \* WN.partOfSpeech).headOrElse(throw new WNRDFException("No part of speech for " + r))),
      (r \* ONTOLEX.otherForm).map(readForm).toSeq,
      (r \* ONTOLEX.sense).map(readSense).toSeq,
      (r \* SYNSEM.synBehavior).map(readSynBehavior).toSeq), r)
  }

  private def readLemma(canForm : Resource, pos : Resource)(implicit model : Model) : Lemma = {
    Lemma(
      (canForm lit ONTOLEX.writtenRep).headOrElse(throw new WNRDFException("No written representation for " + canForm)).getLexicalForm(),
      if(pos.getURI().startsWith(WN.prefix)) {
        PartOfSpeech.fromName(pos.getURI().drop(WN.prefix.length))
      } else {
        throw new WNRDFException("Non-standard part of speech " + pos)
      },
      (canForm lit WN.script).headOption.map(l => Script.getByAlpha4Code(l.getLexicalForm())),
      (canForm \* WN.tag).map(readTag).toSeq,
      (canForm \* WN.pronunciation).map(readPronunciation).toSeq)
  }

  private def readForm(r : Resource)(implicit model : Model) : Form = {
    Form(
      (r lit ONTOLEX.writtenRep).headOrElse(throw new WNRDFException("No written representation for " + r)).getLexicalForm(),
      toOptionalId(r),
      (r \* WN.tag).map(readTag).toSeq,
      (r lit WN.script).headOption.map(l => Script.getByAlpha4Code(l.getLexicalForm())),
      (r \* WN.pronunciation).map(readPronunciation).toSeq)
  }

  private def readPronunciation(r : Resource)(implicit model : Model) : Pronunciation = {
    Pronunciation(
      (r lit RDF.value).headOrElse(throw new WNRDFException("Pronunciation without value")).getLexicalForm(),
      (r lit VARTRANS.variety).headOption.map(_.getLexicalForm()),
      (r lit VARTRANS.notation).headOption.map(_.getLexicalForm()),
      (r lit VARTRANS.phonemic).headOption.map(_.getBoolean()).getOrElse(true),
      (r lit VARTRANS.audio).headOption.map(_.getLexicalForm()))
  }

  private def readTag(r : Resource)(implicit model : Model) : Tag = {
    Tag(
      (r lit WN.category).headOrElse(throw new WNRDFException("No category for " + r)).getLexicalForm(),
      (r lit RDF.value).headOrElse(throw new WNRDFException("No value for " + r)).getLexicalForm())
  }

  private def readSense(r : Resource)(implicit model : Model) : Sense = {
    readMeta(Sense(
      toId(r),
      toId(((r \* ONTOLEX.reference) 
        ++ (r \* ONTOLEX.isLexicalizedSenseOf)).headOrElse(
          throw new WNRDFException("Sense without synset"))),
      (r / VARTRANS.source).map(readSenseRelation).toSeq ++
        readShortSenseRelation(r),
      (r \* WN.example).map(readExample).toSeq,
      (r \* WN.count).map(readCount).toSeq), r)
  }

  private def readSenseRelation(r : Resource)(implicit model : Model) : SenseRelation = {
    val relType = (r \* VARTRANS.category).headOption match {
      case Some(r) if r.getURI().startsWith(WN.prefix) => 
        SenseRelType.fromString(r.getURI().drop(WN.prefix.length), None)
      case _ => (r \* DC_11.`type`).headOption match {
        case Some(t) => other(t.getURI())
        case None => (r lit DC_11.`type`).headOption match {
          case Some(l) => other(l.getLexicalForm())
          case None => throw new WNRDFException("Relation without category or type")
        }
      }
    }
    readMeta(SenseRelation(
      toId((r \* VARTRANS.target).headOrElse(throw new WNRDFException("Relation without target"))),
      relType), r)
  }

  private def readShortSenseRelation(r : Resource)(implicit model : Model) : Seq[SenseRelation] = {
    SenseRelType.values.flatMap(relType => {
      (r \* WN(relType.name)).map(target => {
        SenseRelation(toId(target), relType)
      })
    })
  }



  private def readExample(r : Resource)(implicit model : Model) : Example = {
    readMeta(Example(
      (r lit RDF.value).headOrElse(throw new WNRDFException("Example without value")).getLexicalForm(),
      (r lit DC_11.language).headOption.map(l => Language.get(l.getLexicalForm()))), r)
  }

  private def readCount(r : Resource)(implicit model : Model) : Count = {
    readMeta(Count(
      (r lit RDF.value).headOrElse(throw new WNRDFException("Count without value")).getInt()), r)
  }

  private def readSynBehavior(r : Resource)(implicit model : Model) : SyntacticBehaviour = {
    SyntacticBehaviour(
      toOptionalId(r),
      (r lit RDFS.label).headOrElse(throw new WNRDFException("Syntactic behaviour without label")).getLexicalForm(),
      (r \* WN.senses).map(toId).toSeq)
  }

  private def readSynset(r : Resource)(implicit model : Model) : Synset = {
    readMeta(Synset(
      toId(r),
      (r \* WN.ili).headOption match {
        case Some(u) if u.getURI() startsWith ILI.prefix =>
          Some(u.getURI().drop(ILI.prefix.length))
        case None =>
          None
        case u =>
          throw new WNRDFException("ILI not in ILI namespace " + u)
      },
      (r \* WN.definition).map(readDefinition).toSeq,
      (r \* WN.iliDefinition).headOption.map(readILIDefinition),
      (r / VARTRANS.source).map(readSynsetRelation).toSeq ++ 
        readShortSynsetRelation(r),
      (r \* WN.example).map(readExample).toSeq,
      (r \* WN.partOfSpeech).headOption.map(pos => PartOfSpeech.fromName(pos.getURI().drop(WN.prefix.length)))), r)
  }

  private def readDefinition(r : Resource)(implicit model : Model) : Definition = {
    readMeta(Definition(
      (r lit RDF.value).headOrElse(throw new WNRDFException("Definition without value")).getLexicalForm(),
      (r lit DC_11.language).headOption.map(l => Language.get(l.getLexicalForm())),
      (r \* WN.sourceSense).headOption.map(toId)
    ), r)
  }

  private def readILIDefinition(r : Resource)(implicit model : Model) : ILIDefinition = {
    readMeta(ILIDefinition(
      (r lit RDF.value).headOrElse(throw new WNRDFException("ILIDefinition without value")).getLexicalForm()), r)
  }
   
  private def readSynsetRelation(r : Resource)(implicit model : Model) : SynsetRelation = {
    val relType = (r \* VARTRANS.category).headOption match {
      case Some(r) if r.getURI().startsWith(WN.prefix) => 
        SynsetRelType.fromString(r.getURI().drop(WN.prefix.length), None)
      case _ => (r \* DC_11.`type`).headOption match {
        case Some(t) => other(t.getURI())
        case None => (r lit DC_11.`type`).headOption match {
          case Some(l) => other(l.getLexicalForm())
          case None => throw new WNRDFException("Relation without category or type")
        }
      }
    }
    readMeta(SynsetRelation(
      toId((r \* VARTRANS.target).headOrElse(throw new WNRDFException("Relation without target"))),
      relType), r)
  }

  private def readShortSynsetRelation(r : Resource)(implicit model : Model) : Seq[SynsetRelation] = {
    SynsetRelType.values.flatMap(relType => {
      (r \* WN(relType.name)).map(target => {
        SynsetRelation(toId(target), relType)
      })
    })
  }

  def write(lr : LexicalResource, output : File) : Unit = {
    write(lr, new FileWriter(output),
          output.toURI().toString() + "#",
          WNRDF.guessLang(output), true)
  } 

  def write(lr : LexicalResource, output : File, lang : String, bnodes : Boolean) : Unit = {
    write(lr, new FileWriter(output),
      output.toURI().toString() + "#",
      lang, bnodes)
  }

  def write(lr : LexicalResource, output : File, baseUrl : String, lang : String, bnodes : Boolean) : Unit = {
    write(lr, new FileWriter(output),
      baseUrl, lang, bnodes)
  }

  def write(lr : LexicalResource, output : Writer, baseUrl : String, lang : String, bnodes : Boolean) : Unit = {
    var model = writeLexicalResource(lr)(baseUrl)
    for((prefix, ns) <- namespaces) {
      model.setNsPrefix(prefix, ns.prefix)
    }
    if(!bnodes) {
      model = removeBlanks(model, baseUrl)
    }
    model.write(output, lang)
    output.flush()
  }

  private def writeMeta(a : Meta, r : Resource)(implicit model : Model) : Unit = {
    a.contributor match {
      case Some(l) => 
        r + DC_11.contributor + model.createLiteral(l)
      case None =>
    }
    a.coverage match {
      case Some(l) => 
        r + DC_11.coverage + model.createLiteral(l)
      case None =>
    }
    a.creator match {
      case Some(l) => 
        r + DC_11.creator + model.createLiteral(l)
      case None =>
    }
    a.date match {
      case Some(l) => 
        r + DC_11.date + model.createLiteral(l)
      case None =>
    }
    a.description match {
      case Some(l) => 
        r + DC_11.description + model.createLiteral(l)
      case None =>
    }
    a.format match {
      case Some(l) => 
        r + DC_11.format + model.createLiteral(l)
      case None =>
    }
    a.identifier match {
      case Some(l) => 
        r + DC_11.identifier + model.createLiteral(l)
      case None =>
    }
    a.publisher match {
      case Some(l) => 
        r + DC_11.publisher + model.createLiteral(l)
      case None =>
    }
    a.relation match {
      case Some(l) => 
        r + DC_11.relation + model.createLiteral(l)
      case None =>
    }
    a.rights match {
      case Some(l) => 
        r + DC_11.rights + model.createLiteral(l)
      case None =>
    }
    a.source match {
      case Some(l) => 
        r + DC_11.source + model.createLiteral(l)
      case None =>
    }
    a.subject match {
      case Some(l) => 
        r + DC_11.subject + model.createLiteral(l)
      case None =>
    }
    a.title match {
      case Some(l) => 
        r + DC_11.title + model.createLiteral(l)
      case None =>
    }
    a.`type` match {
      case Some(l) => 
        r + DC_11.`type` + model.createLiteral(l)
      case None =>
    }
    a.status match {
      case Some(l) => 
        r + WN.status + model.createLiteral(l)
      case None =>
    }
    a.note match {
      case Some(l) => 
        r + WN.note + model.createLiteral(l)
      case None =>
    }
    a.confidenceScore match {
      case Some(l) => 
        r + WN.confidenceScore + model.createTypedLiteral(l : Any)
      case None =>
    }
  }

  private def writeLexicalResource(lr : LexicalResource)(implicit baseUrl : String) : Model = {
    implicit val model = ModelFactory.createDefaultModel()
    for(l <- lr.lexicons) {
      writeLexicon(l)(model, baseUrl, l)
    }
    model
  }

  private def writeLexicon(l : Lexicon)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource(baseUrl + l.id)
    writeMeta(l, r)
    r + RDF.`type` + LIME.Lexicon
    r + LIME.entry ++ l.entries.map(writeLexicalEntry)
    r - SKOS.inScheme ++ l.synsets.map(writeSynset)
    r + RDFS.label + model.createLiteral(l.label)
    r + DC_11.language + model.createLiteral(l.language.toString())
    r + SCHEMA.email + model.createLiteral(l.email)
    r + CC.license + model.createResource(l.license)
    r + OWL.versionInfo + model.createLiteral(l.version)
    l.url match {
      case Some(u) =>
        r + SCHEMA.url + model.createLiteral(u)
      case None =>
    }
    l.citation match {
      case Some(c) =>
        r + SCHEMA.citation + model.createLiteral(c)
      case None =>
    }
    r
  }

  private def writeLexicalEntry(e : LexicalEntry)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource(baseUrl + e.id)
    writeMeta(e, r)
    r + RDF.`type` + ONTOLEX.LexicalEntry
    r + ONTOLEX.canonicalForm + writeLemma(e.lemma)
    r + WN.partOfSpeech + writePartOfSpeech(e.lemma.partOfSpeech)
    r + ONTOLEX.otherForm ++ e.forms.map(writeForm)
    r + ONTOLEX.sense ++ e.senses.map(writeSense)
    r + SYNSEM.synBehavior ++ e.syntacticBehaviours.map(writeSyntacticBehavior)
    r + RDFS.label + model.createLiteral(e.lemma.writtenForm, lexicon.language.toString())
    r
  }

  private def writeLemma(e : Lemma)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource()
    r + RDF.`type` + ONTOLEX.Form
    r + ONTOLEX.writtenRep + model.createLiteral(e.writtenForm, lexicon.language.toString())
    e.script match {
      case Some(s) =>
        r + WN.script + model.createLiteral(s.toString())
      case None =>
    }
    e.tag foreach { v =>
        val t = model.createResource()
        r + WN.tag + t
        t + WN.category + model.createLiteral(v.category)
        t + RDF.value + model.createLiteral(v.value)
    }
    e.pronunciation foreach { v =>
      r + WN.pronunciation + writePronunciation(v)
    }
    r
  }

  private def writePartOfSpeech(p : PartOfSpeech)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
     model.createResource(WN.prefix + p.name)
  }

  private def writeForm(f : Form)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = f.id match {
      case Some(id) => model.createResource(baseUrl + id)
      case None => model.createResource()
    }
    r + RDF.`type` + ONTOLEX.Form
    r + ONTOLEX.writtenRep + model.createLiteral(f.writtenForm, lexicon.language.toString())
    f.script match {
      case Some(s) =>
        r + WN.script + model.createLiteral(s.toString())
      case None =>
    }
    f.tag foreach { v =>
        val t = model.createResource()
        r + WN.tag + t
        t + WN.category + model.createLiteral(v.category)
        t + RDF.value + model.createLiteral(v.value)
    }
    f.pronunciation foreach { v =>
      r + WN.pronunciation + writePronunciation(v)
    }
    r
  }

  private def writePronunciation(p : Pronunciation)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource()
    r + RDF.`type` + VARTRANS.Pronunciation
    r + RDF.value + model.createLiteral(p.pronunciation, lexicon.language.toString())
    p.variety match {
      case Some(s) =>
        r + VARTRANS.variety + model.createLiteral(s)
      case None =>
    }
    p.notation match {
      case Some(s) =>
        r + VARTRANS.notation + model.createLiteral(s)
      case None =>
    }
    r + VARTRANS.phonemic + model.createTypedLiteral(p.phonemic: java.lang.Boolean)
    p.audio match {
      case Some(a) =>
        r + VARTRANS.audio + model.createLiteral(a)
      case None =>
    }
    r
  }

  private def writeSense(s : Sense)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource(baseUrl + s.id)
    writeMeta(s, r)
    r + RDF.`type` + ONTOLEX.LexicalSense
    if(shortRelations) {
      for(sr <- s.senseRelations) {
        r + WN(sr.relType.name) + model.createResource(baseUrl + sr.target)
      }
    } else {
      r - VARTRANS.source ++ s.senseRelations.map(writeSenseRelation)
    }
    r + WN.example ++ s.senseExamples.map(writeExample)
    r + ONTOLEX.isLexicalizedSenseOf + model.createResource(baseUrl + s.synsetRef)
    r + WN.count ++ s.counts.map(writeCount)
    r
  }

  private def writeSyntacticBehavior(s : SyntacticBehaviour)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = s.id match {
      case Some(id) => model.createResource(baseUrl + id)
      case None => model.createResource()
    }
    r + RDF.`type` + SYNSEM.SyntacticFrame
    r + RDFS.label + model.createLiteral(s.subcategorizationFrame)
    r + WN.senses ++ s.senses.map(x => model.createResource(baseUrl + x))
    r
  }

  private def writeExample(e : Example)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource()
    writeMeta(e, r)
    r + RDF.`type` + WN.Example
    r + RDF.value + model.createLiteral(e.content, lexicon.language.toString())
    e.language match {
      case Some(s) =>
        r + DC_11.language + model.createLiteral(s.toString())
      case None =>
    }
    r
  }

  private def writeSenseRelation(s : SenseRelation)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource()
    writeMeta(s, r)
    r + RDF.`type` + VARTRANS.SenseRelation
    s.relType match {
      case other(x) =>
        r + DC_11.`type` + (if(x.startsWith("http")) {
          model.createResource(java.net.URI.create(x).toString)
        } else {
          model.createLiteral(x)
        })
      case _ =>
        r + VARTRANS.category + WN(s.relType.name)
    }
    r + VARTRANS.target + model.createResource(baseUrl + s.target)
    r
  }

  private def writeCount(c : Count)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource()
    r + RDF.`type` + WN.Count
    r + RDF.value + model.createTypedLiteral(c.value : Any)
    r
  }

  private def writeSynset(s : Synset)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource(baseUrl + s.id)
    writeMeta(s, r)
    r + RDF.`type` + ONTOLEX.LexicalConcept
    r + WN.definition ++ s.definitions.map(writeDefinition)
    s.iliDefinition match {
      case Some(i) =>
        r + WN.iliDefinition + writeILIDefinition(i)
      case None =>
    }
    if(shortRelations) {
      for(sr <- s.synsetRelations) {
        r + WN(sr.relType.name) + model.createResource(baseUrl + sr.target)
      }
    } else {
      r - VARTRANS.source ++ s.synsetRelations.map(writeSynsetRelation)
    }
    s.ili match {
      case Some(i) =>
        r + WN.ili + model.createResource("http://ili.globalwordnet.org/ili/" + i)
      case None =>
    }
    s.partOfSpeech.foreach({ pos =>
      r + WN.partOfSpeech + writePartOfSpeech(pos)
    })
    r + WN.example ++ s.synsetExamples.map(writeExample)
    r
  }

  private def writeDefinition(d : Definition)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
     val r = model.createResource()
    writeMeta(d, r)
     r + RDF.`type` + WN.Definition
     r + RDF.value + model.createLiteral(d.content, d.language.getOrElse(lexicon.language).toString())
     d.language match {
       case Some(l) => 
         r + DC_11.language + model.createLiteral(l.toString())
       case None =>
     }
     d.sourceSense match {
       case Some(l) => 
         r + WN.sourceSense + model.createResource(baseUrl + l.toString())
       case None =>
     }
     r
  }

  private def writeILIDefinition(d : ILIDefinition)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
     val r = model.createResource()
    writeMeta(d, r)
     r + RDF.`type` + WN.ILIDefinition
     r + RDF.value + model.createLiteral(d.content, "en")
     r
  }

  private def writeSynsetRelation(s : SynsetRelation)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource()
    writeMeta(s, r)
    r + RDF.`type` + VARTRANS.ConceptualRelation
    s.relType match {
      case other(x) =>
        r + DC_11.`type` + (if(x.startsWith("http")) {
          model.createResource(java.net.URI.create(x).toString)
        } else {
          model.createLiteral(x)
        })
      case _ =>
        r + VARTRANS.category + WN(s.relType.name)
    }
    r + VARTRANS.target + model.createResource(baseUrl + s.target)
    r
  }

  private def removeBlanks(model : Model, baseUrl : String) : Model = {
    val random = new scala.util.Random()
    var remap = collection.mutable.HashMap[Resource, Resource]()
    for(stat <- model.listStatements.asScala) {
       if(stat.getSubject().isAnon() && !remap.contains(stat.getSubject())) {
         val bytes = new Array[Byte](32)
         random.nextBytes(bytes)
         val newRes = model.createResource(baseUrl + bytes.map(x => "%X" format x).mkString(""))
         remap.put(stat.getSubject(), newRes)
       }
       if(stat.getObject().isAnon() && !remap.contains(stat.getObject().asInstanceOf[Resource])) {
         val bytes = new Array[Byte](32)
         random.nextBytes(bytes)
         val newRes = model.createResource(baseUrl + bytes.map(x => "%X" format x).mkString(""))
         remap.put(stat.getObject().asResource(), newRes)
       }
    }
    if(!remap.isEmpty) {
      val newModel = ModelFactory.createDefaultModel()
      for(stat <- model.listStatements.asScala) {
        newModel.add(newModel.createStatement(
          remap.getOrElse(stat.getSubject(), stat.getSubject()),
          stat.getPredicate(),
          if(stat.getObject().isResource()) {
            remap.getOrElse(stat.getObject().asResource(), stat.getObject().asResource())
          } else {
            stat.getObject()
          }))
      }
      newModel
    } else {
      model
    }
  }
}

object WNRDF {
  def guessLang(file : File) = {
    if(file.getName().endsWith(".rdf") || file.getName().endsWith(".xml")) {
      "RDF/XML"
    } else if(file.getName().endsWith(".ttl")) {
      "TURTLE"
    } else if(file.getName().endsWith(".nt")) {
      "N-TRIPLE"
    } else if(file.getName().endsWith(".n3")) {
      "N3"
    } else {
      "RDF/XML"
    }
  }
}

