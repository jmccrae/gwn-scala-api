package org.globalwordnet.api.serialize

import eu.monnetproject.lang.Language
import org.globalwordnet.api._
import org.globalwordnet.api.wn._
import java.io.{File, FileReader}
import org.apache.jena.rdf.model.{ModelFactory, Model, Resource, Statement}
import org.apache.jena.vocabulary.{RDF, RDFS}
import scala.collection.JavaConverters._
import MoreStringEscapeUtils._

class W3C(id : String, label : String, language : Language,
  email : String, license : String, version : String, url : Option[String],
  citation : Option[String]) extends Format {
  import WNRDF.{guessLang}
  // To make log4j shut up :)
  org.apache.log4j.BasicConfigurator.configure()

  val wn20instances = "http://www.w3.org/2006/03/wn/wn20/instances/"
  val wn20schema = "http://www.w3.org/2006/03/wn/wn20/schema/"
  val dnschema = "http://www.wordnet.dk/owl/instance/2009/03/schema/"

  def read(file : File) : LexicalResource = {
    read(new FileReader(file), guessLang(file), file.toURI().toString() + "#")
  }

  def read(input : FileReader, rdfLang : String, baseUrl : String) : LexicalResource = {
    val model = ModelFactory.createDefaultModel()
    model.read(input, baseUrl, rdfLang)
    readLexicalResource(model)
  }

  def readLexicalResource(model : Model) : LexicalResource = {
    LexicalResource(Seq(readLexicon(model)))
  }

  def readLexicon(model : Model) : Lexicon = {
    Lexicon(id, label, language, email, license, version, url, citation,
      readLexicalEntries(model), readSynsets(model))
  }

  def readLexicalEntries(model : Model) : Seq[LexicalEntry] = {
    var senses = collection.mutable.ListBuffer[(Sense,LexicalEntry)]()
    for(sense <- model.listSubjectsWithProperty(RDF.`type`,
         model.createResource(wn20schema + "NounWordSense")).asScala) {
           senses += readSense(sense, noun, model)
    }
    for(sense <- model.listSubjectsWithProperty(RDF.`type`,
         model.createResource(wn20schema + "AdjectiveWordSense")).asScala) {
           senses += readSense(sense, adjective, model)
    }
    for(sense <- model.listSubjectsWithProperty(RDF.`type`,
         model.createResource(wn20schema + "AdjectiveSatelliteWordSense")).asScala) {
           senses += readSense(sense, adjective_satellite, model)
    }
    for(sense <- model.listSubjectsWithProperty(RDF.`type`,
         model.createResource(wn20schema + "VerbWordSense")).asScala) {
           senses += readSense(sense, verb, model)
    }
    for(sense <- model.listSubjectsWithProperty(RDF.`type`,
         model.createResource(wn20schema + "AdverbWordSense")).asScala) {
           senses += readSense(sense, adverb, model)
    }
    senses2entries(senses)
  }

  def senses2entries(senses : Seq[(Sense,LexicalEntry)]) : Seq[LexicalEntry] = {
    senses.groupBy(_._2.lemma).values.map( ss => {
      LexicalEntry(ss.head._2.id,
        ss.head._2.lemma,
        ss.head._2.forms,
        ss.map(_._1),
        ss.head._2.syntacticBehaviours)
    }).toSeq
  }

  def readSense(sense : Resource, partOfSpeech : PartOfSpeech, model : Model) : (Sense, LexicalEntry) = {
    val id = url2id(sense)
    val synset = getSynsetId(model.listSubjectsWithProperty(
      model.createProperty(wn20schema, "containsWordSense"), sense).asScala.toStream.
        headOption.getOrElse(throw new W3CFormatException("Sense not in any synsets:" + sense)), model)
    val word = model.listObjectsOfProperty(sense,
      model.createProperty(wn20schema, "word")).asScala.toStream.headOption.
        getOrElse(throw new W3CFormatException("Sense without a word:" + sense))
    val frames = model.listObjectsOfProperty(sense,
      model.createProperty(wn20schema, "frame")).asScala.map(_.asLiteral().getString()).toSeq
    (
      Sense(id=url2id(sense), synsetRef=synset, 
        senseRelations=readSenseRelations(sense, model)),
      readLexicalEntry(word.asResource(), partOfSpeech, frames, model)
    )
  }

  def readLexicalEntry(word : Resource, partOfSpeech : PartOfSpeech, 
      frames : Seq[String], model : Model) : LexicalEntry = {
    val id = url2id(word)
    val lemma = model.listObjectsOfProperty(word,
      model.createProperty(wn20schema, "lexicalForm")).asScala.toStream.
        headOption.getOrElse(throw new W3CFormatException("Word without lemma" 
          + word)).asLiteral().getString()
    LexicalEntry(id=id, lemma=Lemma(lemma, partOfSpeech),
      syntacticBehaviours=frames.map(b => SyntacticBehaviour(b.replaceAll("----", "%"))))
  }

  def readSynsets(model : Model) : Seq[Synset] = {
    var synsets = collection.mutable.ListBuffer[Synset]()
    for(synset <- model.listSubjectsWithProperty(RDF.`type`,
         model.createResource(wn20schema + "NounSynset")).asScala) {
           synsets += readSynset(synset, noun, model)
    }
    for(synset <- model.listSubjectsWithProperty(RDF.`type`,
         model.createResource(wn20schema + "AdjectiveSynset")).asScala) {
           synsets += readSynset(synset, adjective, model)
    }
    for(synset <- model.listSubjectsWithProperty(RDF.`type`,
         model.createResource(wn20schema + "AdjectiveSatelliteSynset")).asScala) {
           synsets += readSynset(synset, adjective_satellite, model)
    }
    for(synset <- model.listSubjectsWithProperty(RDF.`type`,
         model.createResource(wn20schema + "VerbSynset")).asScala) {
           synsets += readSynset(synset, verb, model)
    }
    for(synset <- model.listSubjectsWithProperty(RDF.`type`,
         model.createResource(wn20schema + "AdverbSynset")).asScala) {
           synsets += readSynset(synset, adverb, model)
    }
    synsets.toSeq     
  }

  def getSynsetId(synset : Resource, model : Model) : String = {
    id + "-" + model.listObjectsOfProperty(synset,
      model.createProperty(wn20schema, "synsetId")).asScala.toStream.headOption.
        getOrElse(throw new W3CFormatException("Synset without an ID: " + 
          synset)).asLiteral().getString()
  }

  def readSynset(synset : Resource, partOfSpeech : PartOfSpeech, model : Model) : Synset = {
    val synsetId = getSynsetId(synset, model)
    val gloss = model.listObjectsOfProperty(synset,
      model.createProperty(wn20schema, "gloss")).asScala.toSeq.map(
        g => Definition(g.asLiteral().getString().replaceAll("^\\(|\\)$", "")))
    Synset(id=synsetId, definitions=gloss,
      synsetRelations=readSynsetRelations(synset, model),
      partOfSpeech=Some(partOfSpeech))
  }

  def readSynsetRelations(synset : Resource, model : Model) : Seq[SynsetRelation] = {
    model.listStatements(synset, null, null).asScala.flatMap(statement => {
      if(statement.getPredicate() == 
        model.createProperty(wn20schema, "attribute")) {
          Some(mkSynsetRel(statement, attribute, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "causedBy")) {
          Some(mkSynsetRel(statement, is_caused_by, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "causes")) {
          Some(mkSynsetRel(statement, causes, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "classifiedByRegion")) {
          Some(mkSynsetRel(statement, domain_region, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "classifiedByTopic")) {
          Some(mkSynsetRel(statement, domain_topic, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "classifiedByUsage")) {
          Some(mkSynsetRel(statement, exemplifies, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "entailedBy")) {
          Some(mkSynsetRel(statement, is_entailed_by, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "entails")) {
          Some(mkSynsetRel(statement, entails, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "memberHolonymOf")) {
          Some(mkSynsetRel(statement, holo_member, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "partHolonymOf")) {
          Some(mkSynsetRel(statement, holo_part, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "substanceHolonymOf")) {
          Some(mkSynsetRel(statement, holo_substance, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "hypernymOf")) {
          Some(mkSynsetRel(statement, hypernym, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "hyponymOf")) {
          Some(mkSynsetRel(statement, hyponym, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "memberMeronymOf")) {
          Some(mkSynsetRel(statement, mero_member, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "partMeronymOf")) {
          Some(mkSynsetRel(statement, mero_part, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "substanceHolonymOf")) {
          Some(mkSynsetRel(statement, mero_substance, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "sameVerbGroupAs")) {
          Some(mkSynsetRel(statement, similar, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "similarTo")) {
          Some(mkSynsetRel(statement, similar, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "seeAlso")) {
          Some(mkSynsetRel(statement, also, model))
      } else if(statement.getPredicate().getURI().startsWith(dnschema)) {
          Some(SynsetRelation(url2id(statement.getObject().asResource()),
            other(statement.getPredicate.getURI().substring(dnschema.length))))
      } else {
        None
      }
    }).toSeq
  }

  def mkSynsetRel(statement : Statement, relType : SynsetRelType, model : Model) : SynsetRelation = {
    val targetId = getSynsetId(statement.getObject().asResource(), model)
    SynsetRelation(targetId, relType)
  }

  def readSenseRelations(sense : Resource, model : Model) : Seq[SenseRelation] = {
    model.listStatements(sense, null, null).asScala.flatMap(statement => {
      println(statement)
      if(statement.getPredicate() == 
        model.createProperty(wn20schema, "adjectivePertainsTo")) {
          Some(mkSenseRel(statement, pertainym, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "adverbPertainsTo")) {
          Some(mkSenseRel(statement, pertainym, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "antonymOf")) {
          Some(mkSenseRel(statement, antonym, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "derivationallyRelated")) {
          Some(mkSenseRel(statement, derivation, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "participleOf")) {
          Some(mkSenseRel(statement, participle, model))
      } else if(statement.getPredicate() ==
        model.createProperty(wn20schema, "seeAlso")) {
          Some(mkSenseRel(statement, also, model))
      } else if(statement.getPredicate().getURI().startsWith(dnschema)) {
          Some(SenseRelation(url2id(statement.getObject().asResource()),
            other(statement.getPredicate.getURI().substring(dnschema.length))))
      } else {
        None
      }
    }).toSeq
  }

  def mkSenseRel(statement : Statement, relType : SenseRelType, model : Model) : SenseRelation = {
    val targetId = url2id(statement.getObject().asResource())
    SenseRelation(targetId, relType)
  }



  def url2id(resource : Resource) : String = {
    escapeXmlId((resource.getURI().lastIndexOf('/'), resource.getURI().lastIndexOf('#')) match {
      case (x,y) if x > y => resource.getURI().substring(x + 1)
      case (y,x) if x > y => resource.getURI().substring(x + 1)
      case _ => resource.getURI()
    })
  }


  def write(resource : LexicalResource, file : File) : Unit = {
    throw new UnsupportedOperationException("Cannot write to W3C format yet")
  }
}

case class W3CFormatException(msg : String = "", cause : Throwable = null) extends RuntimeException(msg, cause)
