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
  def removeDefaultConfidence : this.type = {
    if (this.confidenceScore == Some(1.0)) {
      confidenceScore = None
    }
    this
  }
}

case class LexicalResource(lexicons : Seq[Lexicon], lexiconExtensions: Seq[LexiconExtension] = Nil) {
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
    })).iterator.toMultiMap
  }
  lazy val entryObjectsForSynset : Map[String, Seq[LexicalEntry]] = {
    lexicons.flatMap(lexicon => lexicon.entries.flatMap({ entry =>
      entry.senses.map({ sense =>
        sense.synsetRef -> entry
      })
    })).iterator.toMultiMap
  }
 }

case class Lexicon(id : String, 
  label : String, language : Language, email : String,
  license : String, version : String, url : Option[String] = None, 
  citation : Option[String] = None,
  logo : Option[String] = None,
  requires : Seq[Requires] = Nil,
  entries : Seq[LexicalEntry] = Nil, 
  synsets : Seq[Synset] = Nil,
  frames : Seq[SyntacticBehaviour] = Nil) extends Meta {
  if(!synsets.forall(synset => synset.ili != Some("in") || synset.id.startsWith(id + "-"))) {
    throw new WordNetFormatException("Synset identifiers do not start with %s-" format id)
  }

  lazy val synsetsById : Map[String, Synset] = synsets.groupBy(_.id).view.mapValues(_.head).toMap

  override def toString = s"""Lexicon(id=$id label=$label language=$language email=$email license=$license version=$version ${url.map("url=" + _).getOrElse("")}${citation.map("citation" + _).getOrElse("")}
ENTRIES
${entries.mkString("\n")}
SYNSETS
${synsets.mkString("\n")}"""

  def metadata : Lexicon = this.copy(entries = Nil, synsets = Nil)

  lazy val framesById : Map[String, SyntacticBehaviour] = frames.groupBy(_.id.getOrElse("")).view.mapValues(_.head).toMap
}
 
case class LexicalEntry(id : String, lemma : Lemma, forms : Seq[Form] = Nil, senses : Seq[Sense] = Nil,
   syntacticBehaviours : Seq[SyntacticBehaviour] = Nil, index : Option[String] = None) extends ExternalEntries with Meta {
  override def toString = s"""LexicalEntry[$id](${(Seq(lemma.toString) ++ forms.map(_.toString) ++ senses.map(_.toString) ++ syntacticBehaviours.map(_.toString)).mkString(", ")})"""
}

case class Lemma(writtenForm : String, partOfSpeech : PartOfSpeech, script : Option[Script] = None, tag : Seq[Tag] = Nil, pronunciation : Seq[Pronunciation] = Nil) {
  override def toString = s"""Lemma(${(Seq(writtenForm, partOfSpeech.toString) ++ script.map(_.toString) ++ tag.map(_.toString)).mkString(", ")})"""
}

case class Form(writtenForm : String, id : Option[String] = None,
  tag : Seq[Tag] = Nil, script : Option[Script] = None, pronunciation : Seq[Pronunciation] = Nil) extends ExternalForms{
  override def toString = s"""Form(${(Seq(writtenForm) ++ script.map(_.toString) ++ tag.map(_.toString)).mkString(", ")})"""
}

case class Pronunciation(pronunciation : String, variety : Option[String] = None,
  notation : Option[String] = None, phonemic : Boolean = true, audio : Option[String] = None) {
  override def toString = s"""Pronunciation(${pronunciation}${variety.map(" [" + _ + "]").getOrElse("")}${notation.map(" {" + _ + "}").getOrElse("")}${if(phonemic) " (phonemic)" else ""}${audio.map(" audio=" + _).getOrElse("")})"""
}

case class Tag(category : String, value : String) {
  override def toString = s"""$category=$value"""
}

case class Sense(id : String, synsetRef : String,
  senseRelations : Seq[SenseRelation] = Nil, senseExamples : Seq[Example] = Nil,
  counts : Seq[Count] = Nil, adjposition : Option[AdjPosition] = None,
  subcats : Seq[String] = Nil,
  n : Option[Int] = None, lexicalized : Boolean =  true) extends ExternalSenses with Meta {
  override def toString = s"""Sense[$id](${(Seq(synsetRef) ++ senseRelations.map(_.toString) ++ senseExamples.map(_.toString) ++ counts.map(_.toString)).mkString(", ")})"""
}

case class Synset(id : String, ili : Option[String] = None,
  definitions : Seq[Definition] = Nil, iliDefinition : Option[ILIDefinition] = None,
  synsetRelations : Seq[SynsetRelation] = Nil, 
  synsetExamples : Seq[Example] = Nil,
  partOfSpeech : Option[PartOfSpeech] = None,
  members : Seq[String] = Nil,
  lexicalized : Boolean = true,
  lexfile : Option[String] = None) extends ExternalSynsets with Meta {
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

case class SyntacticBehaviour(id : Option[String], 
    subcategorizationFrame : String, senses : Seq[String])

case class Count(value : Int) extends Meta

case class LexiconExtension(id : String, 
  label : String, language : Language, email : String,
  license : String, version : String, 
  url : Option[String] = None, 
  citation : Option[String] = None,
  `extends` : Extends,
  requires : Seq[Requires] = Nil,
  entries : Seq[ExternalEntries] = Nil, 
  synsets : Seq[ExternalSynsets] = Nil) extends Meta 

case class Requires(ref : String, version: String, url : Option[String] = None) 

case class Extends(ref : String, version: String, url : Option[String] = None) 

sealed trait ExternalEntries

case class ExternalLexicalEntry(
  id : String,
  lemma : Option[ExternalLemma] = None,
  forms : Seq[ExternalForms] = Nil,
  senses : Seq[ExternalSenses] = Nil,
  syntacticBehaviours : Seq[SyntacticBehaviour] = Nil) extends ExternalEntries

case class ExternalLemma(
  pronunciation : Seq[Pronunciation] = Nil,
  tag : Seq[Tag] = Nil) 

sealed trait ExternalForms

case class ExternalForm(
  id : String,
  pronunciation : Seq[Pronunciation] = Nil,
  tag : Seq[Tag] = Nil) extends ExternalForms

sealed trait ExternalSenses

case class ExternalSense(
  id : String,
  senseRelations : Seq[SenseRelation] = Nil,
  examples : Seq[Example] = Nil,
  counts : Seq[Count] = Nil
) extends ExternalSenses

sealed trait ExternalSynsets

case class ExternalSynset(
  id : String,
  definitions : Seq[Definition] = Nil,
  synsetRelations : Seq[SynsetRelation] = Nil,
  examples : Seq[Example] = Nil) extends ExternalSynsets
  

trait RelType {
  def name = this.getClass.getSimpleName().dropRight(1)
  override def toString = name
}

sealed trait SynsetRelType extends RelType

object SynsetRelType {
  def fromStringOpt(name : String) : Option[SynsetRelType] = name match {
    case "antonym" => Some(antonym)
    case "agent" => Some(agent)
    case "also" => Some(also)
    case "attribute" => Some(attribute)
    case "be_in_state" => Some(be_in_state)
    case "causes" => Some(causes)
    case "classified_by" => Some(classified_by)
    case "classifies" => Some(classifies)
    case "co_agent_instrument" => Some(co_agent_instrument)
    case "co_agent_patient" => Some(co_agent_patient)
    case "co_agent_result" => Some(co_agent_result)
    case "co_instrument_agent" => Some(co_instrument_agent)
    case "co_instrument_patient" => Some(co_instrument_patient)
    case "co_instrument_result" => Some(co_instrument_result)
    case "co_patient_agent" => Some(co_patient_agent)
    case "co_patient_instrument" => Some(co_patient_instrument)
    case "co_result_agent" => Some(co_result_agent)
    case "co_result_instrument" => Some(co_result_instrument)
    case "co_role" => Some(co_role)
    case "direction" => Some(direction)
    case "domain_region" => Some(domain_region)
    case "domain_topic" => Some(domain_topic)
    case "exemplifies" => Some(exemplifies)
    case "entails" => Some(entails)
    case "eq_synonym" => Some(eq_synonym)
    case "has_domain_region" => Some(has_domain_region)
    case "has_domain_topic" => Some(has_domain_topic)
    case "is_exemplified_by" => Some(is_exemplified_by)
    case "holo_location" => Some(holo_location)
    case "holo_member" => Some(holo_member)
    case "holo_part" => Some(holo_part)
    case "holo_portion" => Some(holo_portion)
    case "holo_substance" => Some(holo_substance)
    case "holonym" => Some(holonym)
    case "hypernym" => Some(hypernym)
    case "hyponym" => Some(hyponym)
    case "in_manner" => Some(in_manner)
    case "instance_hypernym" => Some(instance_hypernym)
    case "instance_hyponym" => Some(instance_hyponym)
    case "instrument" => Some(instrument)
    case "involved" => Some(involved)
    case "involved_agent" => Some(involved_agent)
    case "involved_direction" => Some(involved_direction)
    case "involved_instrument" => Some(involved_instrument)
    case "involved_location" => Some(involved_location)
    case "involved_patient" => Some(involved_patient)
    case "involved_result" => Some(involved_result)
    case "involved_source_direction" => Some(involved_source_direction)
    case "involved_target_direction" => Some(involved_target_direction)
    case "is_caused_by" => Some(is_caused_by)
    case "is_entailed_by" => Some(is_entailed_by)
    case "location" => Some(location)
    case "manner_of" => Some(manner_of)
    case "mero_location" => Some(mero_location)
    case "mero_member" => Some(mero_member)
    case "mero_part" => Some(mero_part)
    case "mero_portion" => Some(mero_portion)
    case "mero_substance" => Some(mero_substance)
    case "meronym" => Some(meronym)
    case "similar" => Some(similar)
    case "patient" => Some(patient)
    case "restricted_by" => Some(restricted_by)
    case "restricts" => Some(restricts)
    case "result" => Some(result)
    case "role" => Some(role)
    case "source_direction" => Some(source_direction)
    case "state_of" => Some(state_of)
    case "target_direction" => Some(target_direction)
    case "subevent" => Some(subevent)
    case "is_subevent_of" => Some(is_subevent_of)
    case "feminine" => Some(feminine)
    case "has_feminine" => Some(has_feminine)
    case "masculine" => Some(masculine)
    case "has_masculine" => Some(has_masculine)
    case "young" => Some(young)
    case "has_young" => Some(has_young)
    case "diminutive" => Some(diminutive)
    case "has_diminutive" => Some(has_diminutive)
    case "augmentative" => Some(augmentative)
    case "has_augmentative" => Some(has_augmentative)
    case "anto_gradable" => Some(anto_gradable)
    case "anto_simple" => Some(anto_simple)
    case "anto_converse" => Some(anto_converse)
    case "ir_synonym" => Some(ir_synonym)
    case _ => None
    }

  def fromStringOrOther(name : String) : SynsetRelType = fromStringOpt(name) match {
    case Some(relType) => relType
    case None => other(name)
  }

  def fromString(name : String, dcType : Option[String] = None) : SynsetRelType = name match {
    case "other" => other(dcType.getOrElse(throw new WordNetFormatException("Other requires a dc:type")))
    case other => fromStringOpt(other) match {
      case Some(relType) => relType
      case None => throw new WordNetFormatException("Unsupported relation type: " + other)
    }
  }

  def fromOMWString(name : String) = name match {
    case "also" => also
    case "at" => other("at")
    case "ants" => antonym
    case "attr" => attribute
    case "caus" => causes
    case "dmnc" => exemplifies
    case "dmnr" => domain_region
    case "dmnu" => other("dmnu")
    case "dmtc" => is_exemplified_by
    case "dmtr" => has_domain_region
    case "dmtu" => other("dmtu")
    case "enta" => entails
    case "eq_synonym" => eq_synonym
    case "hasi" => instance_hyponym
    case "hmem" => holo_member
    case "hprt" => holo_part
    case "hsub" => holo_substance
    case "hype" => hypernym
    case "hypo" => hyponym
    case "inst" => instance_hypernym
    case "mmem" => mero_member
    case "mprt" => mero_part
    case "msub" => mero_substance
    case "self" => other("self")
    case "sim" => similar
    case other => throw new WordNetFormatException("Unsupported relation type: " + other)
  }

  val values = Seq(agent, also, attribute, be_in_state, causes, classified_by,
classifies, co_agent_instrument, co_agent_patient, co_agent_result,
co_instrument_agent, co_instrument_patient, co_instrument_result, co_patient_agent,
co_patient_instrument, co_result_agent, co_result_instrument, co_role, direction,
domain_region, domain_topic, exemplifies, entails, eq_synonym, has_domain_region,
has_domain_topic, is_exemplified_by, holo_location, holo_member, holo_part, holo_portion,
holo_substance, holonym, hypernym, hyponym, in_manner, instance_hypernym,
instance_hyponym, instrument, involved, involved_agent, involved_direction, involved_instrument,
involved_location, involved_patient, involved_result, involved_source_direction,
involved_target_direction, is_caused_by, is_entailed_by, location, manner_of,
mero_location, mero_member, mero_part, mero_portion, mero_substance, meronym,
similar, patient, restricted_by, restricts, result, role, source_direction,
state_of, target_direction, subevent, is_subevent_of,
feminine, has_feminine, masculine, has_masculine, young, has_young,
diminutive, has_diminutive, augmentative, has_augmentative,
anto_gradable, anto_simple, anto_converse, ir_synonym)
}
object agent extends SynsetRelType with SenseRelType
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
object instrument extends SynsetRelType with SenseRelType
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
object location extends SynsetRelType with SenseRelType
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
object result extends SynsetRelType with SenseRelType
object role extends SynsetRelType
object source_direction extends SynsetRelType
object state_of extends SynsetRelType
object target_direction extends SynsetRelType
object subevent extends SynsetRelType
object is_subevent_of extends SynsetRelType
object feminine extends SynsetRelType with SenseRelType
object has_feminine extends SynsetRelType with SenseRelType
object masculine extends SynsetRelType with SenseRelType
object has_masculine extends SynsetRelType with SenseRelType
object young extends SynsetRelType with SenseRelType
object has_young extends SynsetRelType with SenseRelType
object diminutive extends SynsetRelType with SenseRelType
object has_diminutive extends SynsetRelType with SenseRelType
object augmentative extends SynsetRelType with SenseRelType
object has_augmentative extends SynsetRelType with SenseRelType
object anto_gradable extends SynsetRelType with SenseRelType
object anto_simple extends SynsetRelType with SenseRelType
object anto_converse extends SynsetRelType with SenseRelType
object ir_synonym extends SynsetRelType

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
    case "simple_aspect_ip" => simple_aspect_ip
    case "secondary_aspect_ip" => secondary_aspect_ip
    case "simple_aspect_pi" => simple_aspect_pi
    case "secondary_aspect_pi" => secondary_aspect_pi
    case "metonym" => metonym
    case "has_metonym" => has_metonym
    case "material" => material
    case "event" => event
    case "by_means_of" => by_means_of
    case "undergoer" => undergoer
    case "property" => property
    case "state" => state
    case "uses" => uses
    case "destination" => destination
    case "body_part" => body_part
    case "vehicle" => vehicle
    case "feminine" => feminine
    case "has_feminine" => has_feminine
    case "masculine" => masculine
    case "has_masculine" => has_masculine
    case "young" => young
    case "has_young" => has_young
    case "diminutive" => diminutive
    case "has_diminutive" => has_diminutive
    case "augmentative" => augmentative
    case "has_augmentative" => has_augmentative
    case "anto_gradable" => anto_gradable
    case "anto_simple" => anto_simple
    case "anto_converse" => anto_converse
    case "agent" => agent
    case "instrument" => instrument
    case "location" => location
    case "result" => result
  }

  val values = Seq( also, domain_region, domain_topic, exemplifies, has_domain_region,
has_domain_topic, is_exemplified_by, similar, antonym, participle, pertainym, derivation,
simple_aspect_ip, secondary_aspect_ip, simple_aspect_pi, secondary_aspect_pi,
metonym, has_metonym, material, event, by_means_of, undergoer,
property, state, uses, destination, body_part, vehicle,
feminine, has_feminine, masculine, has_masculine, young,
has_young, diminutive, has_diminutive, augmentative, has_augmentative,
anto_gradable, anto_simple, anto_converse, agent, instrument,
location, result)
}
object antonym extends SenseRelType with SynsetRelType
object participle extends SenseRelType
object pertainym extends SenseRelType
object derivation extends SenseRelType
// simple_aspect_ip|secondary_aspect_ip|simple_aspect_pi|secondary_aspect_pi|feminine|has_feminine|masculine|has_masculine|young|has_young|diminutive|has_diminutive|augmentative|has_augmentative|anto_gradable|anto_simple|anto_converse|metaphor|has_metaphor|metonym|has_metonym|agent|material|event|instrument|location|by_means_of|undergoer|property|result|state|uses|destination|body_part|vehicle
object simple_aspect_ip extends SenseRelType
object secondary_aspect_ip extends SenseRelType
object simple_aspect_pi extends SenseRelType
object secondary_aspect_pi extends SenseRelType
object metonym extends SenseRelType
object has_metonym extends SenseRelType
object material extends SenseRelType
object event extends SenseRelType
object by_means_of extends SenseRelType
object undergoer extends SenseRelType
object property extends SenseRelType
object state extends SenseRelType
object uses extends SenseRelType
object destination extends SenseRelType
object body_part extends SenseRelType
object vehicle extends SenseRelType

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

sealed class AdjPosition(val shortForm : String)

object AdjPosition {
  def fromString(code : String) = code match {
    case "a" => attributive
    case "p" => predicative
    case "ip" => postpositive
    case _ => throw new IllegalArgumentException(code + " is not a valid adj position code")
  }
}

object attributive extends AdjPosition("a")
object predicative extends AdjPosition("p")
object postpositive extends AdjPosition("ip")

case class WordNetFormatException(msg : String = null, cause : Throwable = null) extends RuntimeException(msg, cause)
