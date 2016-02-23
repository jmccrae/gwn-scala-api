package org.globalwordnet.api.serialize

import java.io.{File, FileReader, Reader, Writer}
import org.globalwordnet.api.wn._
import org.apache.jena.rdf.model.{Model, ModelFactory, Resource, Property, RDFNode, Literal}
import org.apache.jena.vocabulary.{RDF, RDFS, DC_10, SKOS, OWL, XSD}
import scala.collection.JavaConversions._
import scala.language.dynamics
import scala.language.reflectiveCalls

case class WNRDFException(msg : String = "", cause : Throwable = null) extends RuntimeException(msg, cause)

object WNRDF {
  class NameSpace(val prefix : String) extends Dynamic {
    def apply(suffix : String)(implicit model : Model) = model.createResource(prefix + suffix)
    def selectDynamic(suffix : String)(implicit model : Model) = model.createResource(prefix + suffix)
  }

  private implicit class PimpedResource(resource : Resource) {
    def \(p : Property)(implicit model : Model) : Iterator[RDFNode] = {
      model.listObjectsOfProperty(resource, p)
    }
    def \(r : Resource)(implicit model : Model) : Iterator[RDFNode] = {
      model.listObjectsOfProperty(resource, model.createProperty(r.getURI()))
    }
    def \*(r : Property)(implicit model : Model) : Iterator[Resource] = {
      model.listObjectsOfProperty(resource, r).flatMap({
        case r : Resource => Some(r)
        case _ => None
      })
    }
    def \*(r : Resource)(implicit model : Model) : Iterator[Resource] = \*(model.createProperty(r.getURI()))(model)
    def /(r : Resource)(implicit model : Model) : Iterator[Resource] = /(model.createProperty(r.getURI()))(model)
    def /(p : Property)(implicit model : Model) : Iterator[Resource] = {
      model.listSubjectsWithProperty(p, resource)
    }
    def lit(r : Resource)(implicit model : Model) : Iterator[Literal] = lit(model.createProperty(r.getURI()))(model)
    def lit(p : Property)(implicit model : Model) : Iterator[Literal] = {
      model.listObjectsOfProperty(resource, p).flatMap({
        case l : Literal => Some(l)
        case _ => None
      })
    }
    def +(p : Property)(implicit model : Model) = new {
      def ++(vs : Seq[RDFNode]) = vs.foreach({v =>
        resource.addProperty(p, v)
      })
      def +(v : RDFNode) {
        resource.addProperty(p ,v)
      }
    }
    def +(r : Resource)(implicit model : Model) = new {
      def ++(vs : Seq[RDFNode]) = vs.foreach({v =>
        resource.addProperty(model.createProperty(r.getURI()), v)
      })
      def +(v : RDFNode) {
        resource.addProperty(model.createProperty(r.getURI()) ,v)
      }
    }
    def -(p : Property)(implicit model : Model) = new {
      def ++(vs : Seq[Resource]) = vs.foreach({v =>
        v.addProperty(p, resource)
      })
      def +(v : Resource) {
        v.addProperty(p ,resource)
      }
    }
    def -(r : Resource)(implicit model : Model) = new {
      def ++(vs : Seq[Resource]) = vs.foreach({v =>
        v.addProperty(model.createProperty(r.getURI()), resource)
      })
      def +(v : Resource) {
        v.addProperty(model.createProperty(r.getURI()), resource)
      }
    }


  }

  private implicit class PimpedRDFNodeIterator[A <: RDFNode](iter : Iterator[A]) {
    def headOrElse(foo : => A) = {
      if(iter.hasNext) {
        iter.next
      } else {
        foo
      }
    }

    def headOption = {
      if(iter.hasNext) {
        Some(iter.next)
      } else {
        None
      }
    }
  }

  val WN = new NameSpace("http://globalwordnet.github.com/schemas/wn#")
  val ONTOLEX = new NameSpace("http://www.w3.org/ns/lemon/ontolex#")
  val SYNSEM = new NameSpace("http://www.w3.org/ns/lemon/synsem#")
  val VARTRANS = new NameSpace("http://www.w3.org/ns/lemon/vartrans#")
  val LIME = new NameSpace("http://www.w3.org/ns/lemon/lime#")
  val SCHEMA = new NameSpace("http://schema.org/")
  val CC = new NameSpace("http://creativecommons.org/ns#")

  def read(file : File) : LexicalResource = {
    if(file.getName().endsWith(".rdf") || file.getName().endsWith(".xml")) {
      read(new FileReader(file), "RDF/XML", "file://" + file.getAbsolutePath())
    } else if(file.getName().endsWith(".ttl")) {
      read(new FileReader(file), "TURTLE", "file://" + file.getAbsolutePath())
    } else if(file.getName().endsWith(".nt")) {
      read(new FileReader(file), "N-TRIPLE", "file://" + file.getAbsolutePath())
    } else if(file.getName().endsWith(".n3")) {
      read(new FileReader(file), "N3", "file://" + file.getAbsolutePath())
    } else {
      read(new FileReader(file), "RDF/XML", "file://" + file.getAbsolutePath())
    }
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

  def readLexicalResource(implicit model : Model) : LexicalResource = {
    LexicalResource(model.listSubjectsWithProperty(RDF.`type`, ONTOLEX.Lexicon).map(readLexicon).toSeq)
  }

  def readLexicon(r : Resource)(implicit model : Model) : Lexicon = {
    readMeta(Lexicon(
      (r \* LIME.entry).map(readLexicalEntry).toSeq,
      (r / SKOS.inScheme).map(readSynset).toSeq,
      toId(r),
      (r lit RDFS.label).next().getLexicalForm(),
      (r lit DC_10.language).next().getLexicalForm(),
      (r lit SCHEMA.email).next().getLexicalForm(),
      (r lit CC.license).next().getLexicalForm(),
      (r lit OWL.versionInfo).next().getLexicalForm(),
      (r lit SCHEMA.url).toStream.headOption.map(_.getLexicalForm()),
      (r lit SCHEMA.citation).toStream.headOption.map(_.getLexicalForm())), r)
  }

  def readMeta[A <: Meta](a : A, r : Resource)(implicit model : Model) : A = {
    (r lit DC_10.contributor).toStream.headOption match {
      case Some(l) => 
        a.contributor = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_10.coverage).toStream.headOption match {
      case Some(l) => 
        a.coverage = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_10.creator).toStream.headOption match {
      case Some(l) => 
        a.creator = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_10.date).toStream.headOption match {
      case Some(l) => 
        a.date = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_10.description).toStream.headOption match {
      case Some(l) => 
        a.description = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_10.format).toStream.headOption match {
      case Some(l) => 
        a.format = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_10.identifier).toStream.headOption match {
      case Some(l) => 
        a.identifier = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_10.publisher).toStream.headOption match {
      case Some(l) => 
        a.publisher = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_10.relation).toStream.headOption match {
      case Some(l) => 
        a.relation = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_10.rights).toStream.headOption match {
      case Some(l) => 
        a.rights = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_10.source).toStream.headOption match {
      case Some(l) => 
        a.source = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_10.subject).toStream.headOption match {
      case Some(l) => 
        a.subject = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_10.title).toStream.headOption match {
      case Some(l) => 
        a.title = Some(l.getLexicalForm())
      case None =>
    }
    (r lit DC_10.`type`).toStream.headOption match {
      case Some(l) => 
        a.`type` = Some(l.getLexicalForm())
      case None =>
    }
    (r lit WN.status).toStream.headOption match {
      case Some(l) => 
        a.status = Some(l.getLexicalForm())
      case None =>
    }
    (r lit WN.confidenceScore).toStream.headOption match {
      case Some(l) => 
        a.confidenceScore = Some(l.getDouble())
      case None =>
    }
    a
  }

  def readLexicalEntry(r : Resource)(implicit model : Model) : LexicalEntry = {
    readMeta(LexicalEntry(
      readLemma(
        (r \* ONTOLEX.canonicalForm).headOrElse(throw new WNRDFException("No canonical form for " + r)),
        (r \* WN.partOfSpeech).headOrElse(throw new WNRDFException("No part of speech for " + r))),
      (r \* ONTOLEX.otherForm).map(readForm).toSeq,
      (r \* ONTOLEX.sense).map(readSense).toSeq,
      (r \* SYNSEM.synBehavior).map(readSynBehavior).toSeq,
      toId(r)), r)
  }

  def readLemma(canForm : Resource, pos : Resource)(implicit model : Model) : Lemma = {
    Lemma(
      (canForm lit ONTOLEX.writtenRep).headOrElse(throw new WNRDFException("No written representation for " + canForm)).getLexicalForm(),
      if(pos.getURI().startsWith(WN.prefix)) {
        PartOfSpeech.fromName(pos.getURI().drop(WN.prefix.length))
      } else {
        throw new WNRDFException("Non-standard part of speech " + pos)
      })
  }

  def readForm(r : Resource)(implicit model : Model) : Form = {
    Form(
      (r lit ONTOLEX.writtenRep).headOrElse(throw new WNRDFException("No written representation for " + r)).getLexicalForm(),
      (r lit WN.tag).headOption.map(_.getLexicalForm()))
  }

  def readSense(r : Resource)(implicit model : Model) : Sense = {
    readMeta(Sense(
      (r / VARTRANS.source).map(readSenseRelation).toSeq,
      (r \* WN.example).map(readExample).toSeq,
      toId(r),
      toId((r \* ONTOLEX.reference).headOrElse(throw new WNRDFException("Sense without synset"))),
      (r \* WN.count).map(readCount).toSeq), r)
  }

  def readSenseRelation(r : Resource)(implicit model : Model) : SenseRelation = {
    val relType = (r \* VARTRANS.category).headOrElse(throw new WNRDFException("Relation without category"))
    readMeta(SenseRelation(
      toId((r \* VARTRANS.target).headOrElse(throw new WNRDFException("Relation without target"))),
      if(relType.getURI().startsWith(WN.prefix)) {
        SenseRelType.fromString(relType.getURI().drop(WN.prefix.length))
      } else {
        throw new WNRDFException("Nonstandard relation type")
      }), r)
  }

  def readExample(r : Resource)(implicit model : Model) : Example = {
    readMeta(Example(
      (r lit RDF.value).headOrElse(throw new WNRDFException("Example without value")).getLexicalForm()), r)
  }

  def readCount(r : Resource)(implicit model : Model) : Count = {
    readMeta(Count(
      (r lit RDF.value).headOrElse(throw new WNRDFException("Count without value")).getInt()), r)
  }

  def readSynBehavior(r : Resource)(implicit model : Model) : SyntacticBehaviour = {
    SyntacticBehaviour(
      (r lit RDFS.label).headOrElse(throw new WNRDFException("Syntactic behaviour without label")).getLexicalForm())
  }

  def readSynset(r : Resource)(implicit model : Model) : Synset = {
    readMeta(Synset(
      (r \* WN.definition).map(readDefinition).toSeq,
      (r \* WN.iliDefinition).headOption.map(readILIDefinition),
      (r \* VARTRANS.source).map(readSynsetRelation).toSeq,
      toId(r),
      (r lit WN.ili).headOption.map(_.getLexicalForm()),
      (r \* WN.example).map(readExample).toSeq), r)
  }

  def readDefinition(r : Resource)(implicit model : Model) : Definition = {
    readMeta(Definition(
      (r lit RDF.value).headOrElse(throw new WNRDFException("Definition without value")).getLexicalForm(),
      (r lit DC_10.language).headOption.map(_.getLexicalForm())), r)
  }

  def readILIDefinition(r : Resource)(implicit model : Model) : ILIDefinition = {
    readMeta(ILIDefinition(
      (r lit RDF.value).headOrElse(throw new WNRDFException("ILIDefinition without value")).getLexicalForm()), r)
  }
   
  def readSynsetRelation(r : Resource)(implicit model : Model) : SynsetRelation = {
    val relType = (r \* VARTRANS.category).headOrElse(throw new WNRDFException("Relation without category"))
    readMeta(SynsetRelation(
      toId((r \* VARTRANS.target).headOrElse(throw new WNRDFException("Relation without target"))),
      if(relType.getURI().startsWith(WN.prefix)) {
        SynsetRelType.fromString(relType.getURI().drop(WN.prefix.length))
      } else {
        throw new WNRDFException("Nonstandard relation type")
      }), r)
  }

  def write(lr : LexicalResource, output : Writer, baseUrl : String, lang : String) = {
    val model = writeLexicalResource(lr)(baseUrl)
    model.write(output, lang)
    output.flush()
  }

  def writeLexicalResource(lr : LexicalResource)(implicit baseUrl : String) : Model = {
    implicit val model = ModelFactory.createDefaultModel()
    for(l <- lr.lexicons) {
      writeLexicon(l)(model, baseUrl, l)
    }
    model
  }

  def writeLexicon(l : Lexicon)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource(baseUrl + l.id)
    r + RDF.`type` + ONTOLEX.Lexicon
    r + ONTOLEX.entry ++ l.entries.map(writeLexicalEntry)
    r - SKOS.inScheme ++ l.synsets.map(writeSynset)
    r
  }

  def writeLexicalEntry(e : LexicalEntry)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource(baseUrl + e.id)
    r + RDF.`type` + ONTOLEX.LexicalEntry
    r + ONTOLEX.canonicalForm + writeLemma(e.lemma)
    r + WN.partOfSpeech + writePartOfSpeech(e.lemma.partOfSpeech)
    r + ONTOLEX.otherForm ++ e.forms.map(writeForm)
    r + ONTOLEX.sense ++ e.senses.map(writeSense)
    r + SYNSEM.synBehavior ++ e.syntacticBehaviours.map(writeSyntacticBehavior)
    r
  }

  def writeLemma(e : Lemma)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource()
    r + RDF.`type` + ONTOLEX.Form
    r + ONTOLEX.writtenRep + model.createLiteral(e.writtenForm, lexicon.language)
    r
  }

  def writePartOfSpeech(p : PartOfSpeech)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
     model.createResource(WN.prefix + p.name)
  }

  def writeForm(f : Form)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource()
    r + RDF.`type` + ONTOLEX.Form
    r + ONTOLEX.writtenRep + model.createLiteral(f.writtenForm, lexicon.language)
    r
  }

  def writeSense(s : Sense)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource(baseUrl + s.id)
    r + RDF.`type` + ONTOLEX.LexicalSense
    r - SYNSEM.source ++ s.senseRelations.map(writeSenseRelation)
    r + WN.example ++ s.senseExamples.map(writeExample)
    r + ONTOLEX.reference + model.createResource(baseUrl + s.synsetRef)
    r + WN.count ++ s.counts.map(writeCount)
    r
  }

  def writeSyntacticBehavior(s : SyntacticBehaviour)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource()
    r + RDF.`type` + SYNSEM.SyntacticFrame
    r + RDFS.label + model.createLiteral(s.subcategorizationFrame)
    r
  }

  def writeExample(e : Example)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource()
    r + RDF.`type` + WN.Example
    r + RDF.value + model.createLiteral(e.content, lexicon.language)
    r
  }

  def writeSenseRelation(s : SenseRelation)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource()
    r + RDF.`type` + VARTRANS.SenseRelation
    r + VARTRANS.category + WN(s.relType.name)
    r + VARTRANS.target + model.createResource(baseUrl + s.target)
    r
  }

  def writeCount(c : Count)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource()
    r + RDF.`type` + WN.Count
    r + RDF.value + model.createTypedLiteral(c.value : Any)
    r
  }

  def writeSynset(s : Synset)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource(baseUrl + s.id)
    r + RDF.`type` + ONTOLEX.LexicalConcept
    r + WN.definition ++ s.definitions.map(writeDefinition)
    s.iliDefinition match {
      case Some(i) =>
        r + WN.iliDefinition + writeILIDefinition(i)
      case None =>
    }
    r + VARTRANS.target ++ s.synsetRelations.map(writeSynsetRelation)
    s.ili match {
      case Some(i) =>
        r + WN.ili + model.createResource("http://ili.globalwordnet.org/ili/" + i)
      case None =>
    }
    r + WN.example ++ s.synsetExamples.map(writeExample)
    r
  }

  def writeDefinition(d : Definition)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
     val r = model.createResource()
     r + RDF.`type` + WN.Definition
     r + RDF.value + model.createLiteral(d.content, d.language.getOrElse(lexicon.language))
     r
  }

  def writeILIDefinition(d : ILIDefinition)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
     val r = model.createResource()
     r + RDF.`type` + WN.ILIDefinition
     r + RDF.value + model.createLiteral(d.content, "en")
     r
  }

  def writeSynsetRelation(s : SynsetRelation)(implicit model : Model, baseUrl : String, lexicon : Lexicon) : Resource = {
    val r = model.createResource()
    r + RDF.`type` + VARTRANS.SynsetRelation
    r + VARTRANS.category + WN(s.relType.name)
    r + VARTRANS.target + model.createResource(baseUrl + s.target)
    r
  }


}
