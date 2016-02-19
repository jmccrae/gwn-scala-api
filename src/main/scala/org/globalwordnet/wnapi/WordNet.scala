package org.globalwordnet.api.wn

trait Meta {
  var contributor : Option[String] = None
  var coverage : Option[String] = None
  var creator : Option[String] = None
  var date : Option[String] = None
  var description : Option[String] = None
  var format : Option[String] = None
  var identifier : Option[String] = None
  var publisher : Option[String] = None
  var relation : Option[String] = None
  var rights : Option[String] = None
  var source : Option[String] = None
  var subject : Option[String] = None
  var title : Option[String] = None
  var `type` : Option[String] = None
  var status : Option[String] = None
  var confidenceScore : Option[Double] = None
}

case class LexicalResource(lexicons : Seq[Lexicon]) {
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
}

case class Lexicon(entries : Seq[LexicalEntry], synsets : Seq[Synset],
  id : String, label : String, language : String, email : String,
  license : String, version : String, url : Option[String], 
  citation : Option[String]) extends Meta {
    assert(!entries.isEmpty)
}
 
case class LexicalEntry(lemma : Lemma, forms : Seq[Form], senses : Seq[Sense],
   syntacticBehaviours : Seq[SyntacticBehaviour], id : String) extends Meta

case class Lemma(writtenForm : String, partOfSpeech : PartOfSpeech)

sealed class PartOfSpeech(val shortForm : String)
object noun extends PartOfSpeech("n")
object verb extends PartOfSpeech("v")
object adjective extends PartOfSpeech("a")
object adverb extends PartOfSpeech("r")
object adjective_satellite extends PartOfSpeech("s")
object multiword_expression extends PartOfSpeech("z")
object conjunction extends PartOfSpeech("c")
object preposition extends PartOfSpeech("p")
object other extends PartOfSpeech("x")
object unknown extends PartOfSpeech("u")

object PartOfSpeech {
  def fromString(code : String) = code match {
    case "n" => noun
    case "v" => verb
    case "a" => adjective
    case "r" => adverb
    case "s" => adjective_satellite
    case "z" => multiword_expression
    case "c" => conjunction
    case "p" => preposition
    case "x" => other
    case "u" => unknown
  }
}

case class Form(writtenForm : String, tag : Option[String] = None)

case class Sense(senseRelations : Seq[SenseRelation], senseExamples : Seq[SenseExample],
  id : String, synsetRef : String) extends Meta

case class Synset(definitions : Seq[Definition], iliDefinition : Option[ILIDefinition],
  synsetRelations : Seq[SynsetRelation], id : String, ili : String, count : Option[String] = None) extends Meta

case class Definition(content : String, language : Option[String] = None) extends Meta

case class ILIDefinition(content : String) extends Meta

case class SenseExample(content : String) extends Meta

case class SynsetRelation(target : String, relType : SynsetRelType) extends Meta

trait RelType {
  def name = this.getClass.getSimpleName().dropRight(1)
}

sealed trait SynsetRelType extends RelType

object SynsetRelType {
  def fromString(name : String) = name match {
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
    case "domain_usage" => domain_usage
    case "entails" => entails
    case "eq_synonym" => eq_synonym
    case "fuzzynym" => fuzzynym
    case "has_domain_region" => has_domain_region
    case "has_domain_topic" => has_domain_topic
    case "has_domain_usage" => has_domain_usage
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
    case "near_synonym" => near_synonym
    case "patient" => patient
    case "restricted_by" => restricted_by
    case "restricts" => restricts
    case "result" => result
    case "role" => role
    case "source_direction" => source_direction
    case "state_of" => state_of
    case "synonym" => synonym
    case "target_direction" => target_direction
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
object domain_usage extends SynsetRelType with SenseRelType
object entails extends SynsetRelType
object eq_synonym extends SynsetRelType
object fuzzynym extends SynsetRelType
object has_domain_region extends SynsetRelType with SenseRelType
object has_domain_topic extends SynsetRelType with SenseRelType
object has_domain_usage extends SynsetRelType with SenseRelType
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
object near_synonym extends SynsetRelType
case class other(`type` : String) extends SynsetRelType with SenseRelType
object patient extends SynsetRelType
object restricted_by extends SynsetRelType
object restricts extends SynsetRelType
object result extends SynsetRelType
object role extends SynsetRelType
object source_direction extends SynsetRelType
object state_of extends SynsetRelType
object synonym extends SynsetRelType
object target_direction extends SynsetRelType

case class SenseRelation(target : String, relType : SenseRelType) extends Meta

sealed trait SenseRelType extends RelType

object SenseRelType {
  def fromString(name : String) = name match {
    case "antonym" => antonym
    case "near_antonym" => near_antonym
    case "also" => also
    case "participle" => participle
    case "pertainym" => pertainym
    case "derivation" => derivation
    case "domain_topic" => domain_topic
    case "has_domain_category" => has_domain_topic
    case "domain_region" => domain_region
    case "has_domain_region" => has_domain_region
    case "domain_usage" => domain_usage
    case "has_domain_usage" => has_domain_usage
  }
}
object antonym extends SenseRelType
object near_antonym extends SenseRelType
object participle extends SenseRelType
object pertainym extends SenseRelType
object derivation extends SenseRelType

case class SyntacticBehaviour(subcategorizationFrame : String)

