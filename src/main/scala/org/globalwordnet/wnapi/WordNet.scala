package org.globalwordnet.api.wn

import eu.monnetproject.lang.{Language, Script}

trait Meta {
  var contributor : Option[String] = None
  def withContributor(s : String) : this.type = { contributor = Some(s) ; this }
  var coverage : Option[String] = None
  def withCoverage(s : String) : this.type = { coverage = Some(s) ; this }
  var creator : Option[String] = None
  def withCreator(s : String) : this.type = { creator = Some(s) ; this }
  var date : Option[String] = None
  def withDate(s : String) : this.type = { date = Some(s) ; this }
  var description : Option[String] = None
  def withDescription(s : String) : this.type = { description = Some(s) ; this }
  var format : Option[String] = None
  def withFormat(s : String) : this.type = { format = Some(s) ; this }
  var identifier : Option[String] = None
  def withIdentifier(s : String) : this.type = { identifier = Some(s) ; this }
  var publisher : Option[String] = None
  def withPublisher(s : String) : this.type = { publisher = Some(s) ; this }
  var relation : Option[String] = None
  def withRelation(s : String) : this.type = { relation = Some(s) ; this }
  var rights : Option[String] = None
  def withRights(s : String) : this.type = { rights = Some(s) ; this }
  var source : Option[String] = None
  def withSource(s : String) : this.type = { source = Some(s) ; this }
  var subject : Option[String] = None
  def withSubject(s : String) : this.type = { subject = Some(s) ; this }
  var title : Option[String] = None
  def withTitle(s : String) : this.type = { title = Some(s) ; this }
  var `type` : Option[String] = None
  def withType(s : String) : this.type = { `type` = Some(s) ; this }
  var status : Option[String] = None
  def withStatus(s : String) : this.type = { status = Some(s) ; this }
  var note : Option[String] = None
  def withNote(s : String) : this.type = { note = Some(s) ; this }
  var confidenceScore : Option[Double] = None
  def withConfidenceScore(s : Double) : this.type = { confidenceScore = Some(s) ; this }
}

case class LexicalResource(lexicons : Seq[Lexicon]) {
  import org.globalwordnet.api.MultiMap._
  lazy val synsetLookup : Map[String, Synset] = lexicons.flatMap({ 
    lexicon =>
      lexicon.synsets.map({
        synset =>
          synset.id -> synset
      })
  }).toMap

  lazy val senseLookup : Map[String, (LexicalEntry, Sense)] = lexicons.flatMap({
    lexicon =>
      lexicon.entries.flatMap({
        entry => 
          entry.senses.map({
            sense =>
              sense.id -> (entry, sense)
          })
      })
  }).toMap
  lazy val entriesForSynset : Map[String, Seq[String]] = {
    lexicons.flatMap(lexicon => lexicon.entries.flatMap({ entry =>
      entry.senses.map({ sense =>
        sense.synsetRef -> entry.lemma.writtenForm
      })
    })).toMultiMap
  }
}

case class Lexicon(id : String, 
  label : String, language : Language, email : String,
  license : String, version : String, url : Option[String] = None, 
  citation : Option[String] = None,
  entries : Seq[LexicalEntry] = Nil, 
  synsets : Seq[Synset] = Nil) extends Meta {
  if(!synsets.forall(synset => synset.ili != Some("in") || synset.id.startsWith(id + "-"))) {
    throw new WordNetFormatException("Synset identifiers do not start with %s-" format id)
  }

  lazy val synsetsById : Map[String, Synset] = synsets.groupBy(_.id).mapValues(_.head)

  override def toString = s"""Lexicon(id=$id label=$label language=$language email=$email license=$license version=$version ${url.map("url=" + _).getOrElse("")}${citation.map("citation" + _).getOrElse("")}
ENTRIES
${entries.mkString("\n")}
SYNSETS
${synsets.mkString("\n")}"""

  def metadata : Lexicon = this.copy(entries = Nil, synsets = Nil)
}
 
case class LexicalEntry(id : String, lemma : Lemma, forms : Seq[Form] = Nil, senses : Seq[Sense] = Nil,
   syntacticBehaviours : Seq[SyntacticBehaviour] = Nil) extends Meta {
  override def toString = s"""LexicalEntry[$id](${(Seq(lemma.toString) ++ forms.map(_.toString) ++ senses.map(_.toString) ++ syntacticBehaviours.map(_.toString)).mkString(", ")})"""
}

case class Lemma(writtenForm : String, partOfSpeech : PartOfSpeech, script : Option[Script] = None, tag : Seq[Tag] = Nil) {
  override def toString = s"""Lemma(${(Seq(writtenForm, partOfSpeech.toString) ++ script.map(_.toString) ++ tag.map(_.toString)).mkString(", ")})"""
}

case class Form(writtenForm : String, tag : Seq[Tag] = Nil, script : Option[Script] = None) {
  override def toString = s"""Form(${(Seq(writtenForm) ++ script.map(_.toString) ++ tag.map(_.toString)).mkString(", ")})"""
}

case class Tag(category : String, value : String) {
  override def toString = s"""$category=$value"""
}

case class Sense(id : String, synsetRef : String,
  senseRelations : Seq[SenseRelation] = Nil, senseExamples : Seq[Example] = Nil,
  counts : Seq[Count] = Nil) extends Meta {
  override def toString = s"""Sense[$id](${(Seq(synsetRef) ++ senseRelations.map(_.toString) ++ senseExamples.map(_.toString) ++ counts.map(_.toString)).mkString(", ")})"""
}

case class Synset(id : String, ili : Option[String] = None,
  definitions : Seq[Definition] = Nil, iliDefinition : Option[ILIDefinition] = None,
  synsetRelations : Seq[SynsetRelation] = Nil, 
  synsetExamples : Seq[Example] = Nil,
  partOfSpeech : Option[PartOfSpeech] = None) extends Meta {
  ili match {
    case Some("in") if iliDefinition == None =>
      throw new WordNetFormatException("If the ILI is set to \"in\" there must be an ILI Definition [" + id + "]")
    case _ =>
  }

  override def toString = s"""Synset[$id](${(ili.toSeq ++ definitions.map(_.toString) ++
    iliDefinition.map(_.toString) ++ synsetRelations.map(_.toString) ++ synsetExamples.map(_.toString)).mkString(", ")})"""

  def pos(lexicon : Lexicon) = partOfSpeech.getOrElse({
    lexicon.entries.filter(_.senses.exists(_.synsetRef == id)).headOption match {
      case Some(entry) =>
        entry.lemma.partOfSpeech
      case None =>
        throw new WordNetFormatException("Empty synset without part-of-speech")
    }
  })
}

case class Definition(content : String, language : Option[Language] = None,
  sourceSense : Option[String] = None) extends Meta {
  override def toString = s"""Definition(${content}${language.map("@" + _).getOrElse("")}${sourceSense.map(" [" + _ + "]").getOrElse("")})"""
}

case class ILIDefinition(content : String) extends Meta

case class Example(content : String, language : Option[Language] = None) extends Meta

case class SynsetRelation(target : String, relType : SynsetRelType) extends Meta {
  override def toString = s"""$relType -> $target"""
}

case class SenseRelation(target : String, relType : SenseRelType) extends Meta {
  override def toString = s"""$relType -> $target"""
}

case class SyntacticBehaviour(subcategorizationFrame : String, senses : Seq[String])

case class Count(value : Int) extends Meta

trait RelType {
  def name = this.getClass.getSimpleName().dropRight(1)
  override def toString = name
}

sealed trait SynsetRelType extends RelType

object SynsetRelType {
  def fromStringOrOther(name : String) : SynsetRelType = name match {
    case "agent" => agent
    case "also" => also
    case "attribute" => attribute
    case "be_in_state" => be_in_state
    case "causes" => causes
    case "classified_by" => classified_by
    case "classifies" => classifies
    case "co_agent_instrument" => co_agent_instrument
    case "co_agent_patient" => co_agent_patient
    case "co_agent_result" => co_agent_result
    case "co_instrument_agent" => co_instrument_agent
    case "co_instrument_patient" => co_instrument_patient
    case "co_instrument_result" => co_instrument_result
    case "co_patient_agent" => co_patient_agent
    case "co_patient_instrument" => co_patient_instrument
    case "co_result_agent" => co_result_agent
    case "co_result_instrument" => co_result_instrument
    case "co_role" => co_role
    case "direction" => direction
    case "domain_region" => domain_region
    case "domain_topic" => domain_topic
    case "exemplifies" => exemplifies
    case "entails" => entails
    case "eq_synonym" => eq_synonym
    case "has_domain_region" => has_domain_region
    case "has_domain_topic" => has_domain_topic
    case "is_exemplified_by" => is_exemplified_by
    case "holo_location" => holo_location
    case "holo_member" => holo_member
    case "holo_part" => holo_part
    case "holo_portion" => holo_portion
    case "holo_substance" => holo_substance
    case "holonym" => holonym
    case "hypernym" => hypernym
    case "hyponym" => hyponym
    case "in_manner" => in_manner
    case "instance_hypernym" => instance_hypernym
    case "instance_hyponym" => instance_hyponym
    case "instrument" => instrument
    case "involved" => involved
    case "involved_agent" => involved_agent
    case "involved_direction" => involved_direction
    case "involved_instrument" => involved_instrument
    case "involved_location" => involved_location
    case "involved_patient" => involved_patient
    case "involved_result" => involved_result
    case "involved_source_direction" => involved_source_direction
    case "involved_target_direction" => involved_target_direction
    case "is_caused_by" => is_caused_by
    case "is_entailed_by" => is_entailed_by
    case "location" => location
    case "manner_of" => manner_of
    case "mero_location" => mero_location
    case "mero_member" => mero_member
    case "mero_part" => mero_part
    case "mero_portion" => mero_portion
    case "mero_substance" => mero_substance
    case "meronym" => meronym
    case "similar" => similar
    case "patient" => patient
    case "restricted_by" => restricted_by
    case "restricts" => restricts
    case "result" => result
    case "role" => role
    case "source_direction" => source_direction
    case "state_of" => state_of
    case "target_direction" => target_direction
    case "subevent" => subevent
    case "is_subevent_of" => is_subevent_of
    case "antonym" => antonym
    case _other => other(_other)
  }
 
  def fromString(name : String, dcType : Option[String]) = name match {
    case "agent" => agent
    case "also" => also
    case "attribute" => attribute
    case "be_in_state" => be_in_state
    case "causes" => causes
    case "classified_by" => classified_by
    case "classifies" => classifies
    case "co_agent_instrument" => co_agent_instrument
    case "co_agent_patient" => co_agent_patient
    case "co_agent_result" => co_agent_result
    case "co_instrument_agent" => co_instrument_agent
    case "co_instrument_patient" => co_instrument_patient
    case "co_instrument_result" => co_instrument_result
    case "co_patient_agent" => co_patient_agent
    case "co_patient_instrument" => co_patient_instrument
    case "co_result_agent" => co_result_agent
    case "co_result_instrument" => co_result_instrument
    case "co_role" => co_role
    case "direction" => direction
    case "domain_region" => domain_region
    case "domain_topic" => domain_topic
    case "exemplifies" => exemplifies
    case "entails" => entails
    case "eq_synonym" => eq_synonym
    case "has_domain_region" => has_domain_region
    case "has_domain_topic" => has_domain_topic
    case "is_exemplified_by" => is_exemplified_by
    case "holo_location" => holo_location
    case "holo_member" => holo_member
    case "holo_part" => holo_part
    case "holo_portion" => holo_portion
    case "holo_substance" => holo_substance
    case "holonym" => holonym
    case "hypernym" => hypernym
    case "hyponym" => hyponym
    case "in_manner" => in_manner
    case "instance_hypernym" => instance_hypernym
    case "instance_hyponym" => instance_hyponym
    case "instrument" => instrument
    case "involved" => involved
    case "involved_agent" => involved_agent
    case "involved_direction" => involved_direction
    case "involved_instrument" => involved_instrument
    case "involved_location" => involved_location
    case "involved_patient" => involved_patient
    case "involved_result" => involved_result
    case "involved_source_direction" => involved_source_direction
    case "involved_target_direction" => involved_target_direction
    case "is_caused_by" => is_caused_by
    case "is_entailed_by" => is_entailed_by
    case "location" => location
    case "manner_of" => manner_of
    case "mero_location" => mero_location
    case "mero_member" => mero_member
    case "mero_part" => mero_part
    case "mero_portion" => mero_portion
    case "mero_substance" => mero_substance
    case "meronym" => meronym
    case "similar" => similar
    case "patient" => patient
    case "restricted_by" => restricted_by
    case "restricts" => restricts
    case "result" => result
    case "role" => role
    case "source_direction" => source_direction
    case "state_of" => state_of
    case "target_direction" => target_direction
    case "subevent" => subevent
    case "is_subevent_of" => is_subevent_of
    case "antonym" => antonym
    case "other" => other(dcType.getOrElse(throw new WordNetFormatException("Other requires a dc:type")))
    case other => throw new WordNetFormatException("Unsupported relation type: " + other)
  }
}
object agent extends SynsetRelType
object also extends SynsetRelType with SenseRelType
object attribute extends SynsetRelType
object be_in_state extends SynsetRelType
object causes extends SynsetRelType
object classified_by extends SynsetRelType
object classifies extends SynsetRelType
object co_agent_instrument extends SynsetRelType
object co_agent_patient extends SynsetRelType
object co_agent_result extends SynsetRelType
object co_instrument_agent extends SynsetRelType
object co_instrument_patient extends SynsetRelType
object co_instrument_result extends SynsetRelType
object co_patient_agent extends SynsetRelType
object co_patient_instrument extends SynsetRelType
object co_result_agent extends SynsetRelType
object co_result_instrument extends SynsetRelType
object co_role extends SynsetRelType
object direction extends SynsetRelType
object domain_region extends SynsetRelType with SenseRelType
object domain_topic extends SynsetRelType with SenseRelType
object exemplifies extends SynsetRelType with SenseRelType
object entails extends SynsetRelType
object eq_synonym extends SynsetRelType
object has_domain_region extends SynsetRelType with SenseRelType
object has_domain_topic extends SynsetRelType with SenseRelType
object is_exemplified_by extends SynsetRelType with SenseRelType
object holo_location extends SynsetRelType
object holo_member extends SynsetRelType
object holo_part extends SynsetRelType
object holo_portion extends SynsetRelType
object holo_substance extends SynsetRelType
object holonym extends SynsetRelType
object hypernym extends SynsetRelType
object hyponym extends SynsetRelType
object in_manner extends SynsetRelType
object instance_hypernym extends SynsetRelType
object instance_hyponym extends SynsetRelType
object instrument extends SynsetRelType
object involved extends SynsetRelType
object involved_agent extends SynsetRelType
object involved_direction extends SynsetRelType
object involved_instrument extends SynsetRelType
object involved_location extends SynsetRelType
object involved_patient extends SynsetRelType
object involved_result extends SynsetRelType
object involved_source_direction extends SynsetRelType
object involved_target_direction extends SynsetRelType
object is_caused_by extends SynsetRelType
object is_entailed_by extends SynsetRelType
object location extends SynsetRelType
object manner_of extends SynsetRelType
object mero_location extends SynsetRelType
object mero_member extends SynsetRelType
object mero_part extends SynsetRelType
object mero_portion extends SynsetRelType
object mero_substance extends SynsetRelType
object meronym extends SynsetRelType
object similar extends SynsetRelType with SenseRelType
case class other(`type` : String) extends SynsetRelType with SenseRelType {
  override def name = "other"
}
object patient extends SynsetRelType
object restricted_by extends SynsetRelType
object restricts extends SynsetRelType
object result extends SynsetRelType
object role extends SynsetRelType
object source_direction extends SynsetRelType
object state_of extends SynsetRelType
object target_direction extends SynsetRelType
object subevent extends SynsetRelType
object is_subevent_of extends SynsetRelType


sealed trait SenseRelType extends RelType

object SenseRelType {
  def fromString(name : String, dcType : Option[String]) = name match {
    case "antonym" => antonym
    case "also" => also
    case "participle" => participle
    case "pertainym" => pertainym
    case "derivation" => derivation
    case "domain_topic" => domain_topic
    case "has_domain_topic" => has_domain_topic
    case "domain_region" => domain_region
    case "has_domain_region" => has_domain_region
    case "exemplifies" => exemplifies
    case "is_exemplified_by" => is_exemplified_by
    case "similar" => similar
    case "other" => other(dcType.getOrElse(throw new RuntimeException("Other requires a dc:type")))
  }
}
object antonym extends SenseRelType with SynsetRelType
object participle extends SenseRelType
object pertainym extends SenseRelType
object derivation extends SenseRelType

sealed class PartOfSpeech(val shortForm : String, val name : String) {
  override def toString = this.getClass.getSimpleName().dropRight(1)
  def shortFormNoSatellite = if(shortForm == "s") { "a" } else { shortForm }
}
object noun extends PartOfSpeech("n", "noun")
object verb extends PartOfSpeech("v", "verb")
object adjective extends PartOfSpeech("a", "adjective")
object adverb extends PartOfSpeech("r", "adverb")
object adjective_satellite extends PartOfSpeech("s", "adjective_satellite")
object named_entity extends PartOfSpeech("t", "named_entity")
object conjunction extends PartOfSpeech("c", "conjunction")
object adposition extends PartOfSpeech("p", "adposition")
object other_pos extends PartOfSpeech("x", "other_pos")
object unknown_pos extends PartOfSpeech("u", "unknown_pos")

object PartOfSpeech {
  def fromString(code : String) = code match {
    case "n" => noun
    case "v" => verb
    case "a" => adjective
    case "r" => adverb
    case "s" => adjective_satellite
    case "t" => named_entity
    case "c" => conjunction
    case "p" => adposition
    case "x" => other_pos
    case "u" => unknown_pos
    case _ => throw new IllegalArgumentException(code + " is not a valid pos code")
  }
  def fromName(name : String) = name match {
    case "noun" => noun
    case "verb" => verb
    case "adjective" => adjective
    case "adverb" => adverb
    case "adjective_satellite" => adjective_satellite
    case "named_entity" => named_entity
    case "conjunction" => conjunction
    case "adposition" => adposition
    case "other_pos" => other_pos
    case "unknown_pos" => unknown_pos
    case _ => throw new IllegalArgumentException(name + " is not a valid pos name")
  }
}

case class WordNetFormatException(msg : String = null, cause : Throwable = null) extends RuntimeException(msg, cause)
