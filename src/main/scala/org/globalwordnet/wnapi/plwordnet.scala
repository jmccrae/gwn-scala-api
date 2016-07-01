package org.globalwordnet.api

import eu.monnetproject.lang.Language
import java.io.File
import org.apache.commons.lang3.StringEscapeUtils._
import org.globalwordnet.api.wn._
import scala.xml._

package plwn {
    sealed trait plWNDescription

    case class SimpleDefinition(text : String) extends plWNDescription
    case class Category(value : String) extends plWNDescription
    case class OtherDefinition(source : String, text : String) extends plWNDescription
    case class Link(to : String) extends plWNDescription
    case class Ref(to : String) extends plWNDescription

    case class Emotion(annoNumber : Int,
      primary : Seq[EmotionValue], universal : Seq[EmotionValue],
      sentiment : Sentiment, example : Seq[String]) extends plWNDescription

    sealed trait Sentiment
    object NoSent extends Sentiment
    object WeakPos extends Sentiment
    object StrongPos extends Sentiment
    object WeakNeg extends Sentiment
    object StrongNeg extends Sentiment
    object Ambivalent extends Sentiment

    sealed trait EmotionValue
    sealed class PlutchikEmotion(val wnId : String) extends EmotionValue
    sealed trait PuzyninaEmotion extends EmotionValue
    object anticipation extends PlutchikEmotion("wn31-07526319-n") {
      override def toString = "anticipation" }
    object joy extends PlutchikEmotion("wn31-07542591-n") {
      override def toString = "joy" }
    object trust extends PlutchikEmotion("wn31-13952885-n") {
      override def toString = "trust" }
    object fear extends PlutchikEmotion("wn31-07534492-n") {
      override def toString = "fear" }
    object surprise extends PlutchikEmotion("wn31-07525587-n") {
      override def toString = "surprise" }
    object sadness extends PlutchikEmotion("wn31-07547828-n") {
      override def toString = "sadness" }
    object disgust extends PlutchikEmotion("wn31-07518499-n") {
      override def toString = "disgust" }
    object anger extends PlutchikEmotion("wn31-07531593-n") {
      override def toString = "anger" }
    object beauty extends PuzyninaEmotion {
      override def toString = "beauty" }
    object ugliness extends PuzyninaEmotion {
      override def toString = "ugliness" }
    object utility extends PuzyninaEmotion {
      override def toString = "utility" }
    object futility extends PuzyninaEmotion {
      override def toString = "futility" }
    object error extends PuzyninaEmotion {
      override def toString = "error" }
    object truth extends PuzyninaEmotion {
      override def toString = "truth" }
    object harm extends PuzyninaEmotion {
      override def toString = "harm" }
    object anothers_good extends PuzyninaEmotion {
      override def toString = "anothers_good" }
    object ignorance extends PuzyninaEmotion {
      override def toString = "ignorance" }
    object knowledge extends PuzyninaEmotion {
      override def toString = "knowledge" }
    object happiness extends PuzyninaEmotion {
      override def toString = "happiness" }
    object misfortune extends PuzyninaEmotion {
      override def toString = "misfortune" }
}

package serialize {

  case class PLWordNetConfig(
    val email : String = "plwordnet.pwr.wroc.pl@gmail.com",
    val license : String = "http://nlp.pwr.wroc.pl/plwordnet/license",
    val version : String = "2.3",
    val url : Option[String] = Some("http://nlp.pwr.wroc.pl/plwordnet"),
    val citation : Option[String] = None
    )

  object plWordNetReader {
    import org.globalwordnet.api.plwn._

    def load_plwordnet(en : Boolean, plWordNetFile : File) = {
      val root = if(plWordNetFile.getName().endsWith(".gz")) {
        XML.load(new java.util.zip.GZIPInputStream(new java.io.FileInputStream(plWordNetFile)))
      } else {
        XML.loadFile(plWordNetFile)
      }
      val pwn_entries = (root \\ "lexical-unit").filter(x => en == (x \ "@pos").text.endsWith(" pwn")).map(
        x => (x \ "@id").text -> ((x \ "@name").text, (x \ "@pos").text)).toMap
      val lexicalunits = pwn_entries.keys.toSet
      val descriptions = (root \\ "lexical-unit").filter(x => en == (x \ "@pos").text.endsWith(" pwn")).map(
        x => (x \ "@id").text -> (x \ "@desc").text).toMap
      val synsets = (root \\ "synset").filter(
        x => (x \ "unit-id").exists(n => pwn_entries contains n.text)).map(
        x => (x \ "@id").text -> ((x \ "unit-id").map(_.text), (x \ "@definition").text)).toMap
      val lexrelations = (root \\ "lexicalrelations").filter(
        x => (lexicalunits contains (x \ "@child").text)).map(
        x => ((x \ "@child").text, (x \ "@parent").text, (x \ "@relation").text)).
        groupBy(_._1).mapValues(_.map(x => (x._2, x._3)))
      val synrelations = (root \\ "synsetrelations").filter(
        x => (synsets contains (x \ "@child").text)).map(
        x => ((x \ "@child").text, (x \ "@parent").text, (x \ "@relation").text)).
        groupBy(_._1).mapValues(_.map(x => (x._2, x._3)))
       (pwn_entries, synsets, lexrelations, synrelations, descriptions)
    }

    def build_senses(synsets : Map[String, (Seq[String], String)]) : Map[String, Seq[String]] = {
      synsets.flatMap({
        case (sid, (lids, _)) =>
          lids.map(_ -> sid)
      }).groupBy(_._1).mapValues(_.map(_._2).toSeq)
    }

    def polishToPos(polish : String) = polish match {
      case "przymiotnik pwn" => adjective
      case "przymiotnik" => adjective
      case "rzeczownik pwn"  => noun
      case "rzeczownik"  => noun
      case "czasownik pwn"   => verb
      case "czasownik"   => verb
      case "przysłówek pwn"  => adverb
      case "przysłówek"  => adverb
    }

      val coreSynRels = Set("agent", "also", "attribute", "be_in_state", "causes", "classified_by", "classifies", "co_agent_instrument", "co_agent_patient", "co_agent_result", "co_instrument_agent", "co_instrument_patient", "co_instrument_result", "co_patient_agent", "co_patient_instrument", "co_result_agent", "co_result_instrument", "co_role", "derivation", "direction", "domain_region", "domain_topic", "domain_usage", "entails", "eq_synonym", "fuzzynym", "has_domain_region", "has_domain_topic", "has_domain_usage", "holo_location", "holo_member", "holo_part", "holo_portion", "holo_substance", "holonym", "hypernym", "hyponym", "in_manner", "instance_hypernym", "instance_hyponym", "instrument", "involved", "involved_agent", "involved_direction", "involved_instrument", "involved_location", "involved_patient", "involved_result", "involved_source_direction", "involved_target_direction", "is_caused_by", "is_entailed_by", "location", "manner_of", "mero_location", "mero_member", "mero_part", "mero_portion", "mero_substance", "meronym", "near_synonym", "other", "patient", "restricted_by", "restricts", "result", "role", "source_direction", "state_of", "target_direction")
      val coreSenseRels = Set("antonym", "near_antonym", "also", "participle", "pertainym", "derivation", "domain_topic", "has_domain_topic", "domain_region", "has_domain_region", "domain_usage", "has_domain_usage", "other")

    def mapRelType(relType : String) = relType match {
      // Relations only in plWordNet
      case "10" => hyponym
      case "11" => hypernym
      case "13" => other("conversion")
      case "19" => also
      case "20" => mero_part
      case "21" => mero_portion
      case "22" => mero_location
      case "23" => mero_member
      case "24" => mero_substance
      case "25" => holo_part
      case "26" => holo_portion
      case "27" => holo_location
      case "28" => holo_member
      case "29" => holo_substance
      case "34" => agent
      case "35" => patient
      case "36" => instrument
      case "37" => location
      case "38" => result
      case "39" => other("time")
      case "40" => role
      case "41" => involved_agent
      case "42" => involved_patient
      case "43" => other("involved_time")
      case "44" => involved_location
      case "45" => involved_instrument
      case "46" => involved_result
      case "47" => involved
      case "48" => other("agent_hidden")
      case "49" => other("location_hidden")
      case "50" => other("result_hidden")
      case "51" => other("be_in_state")
      case "52" => other("state")
      case "53" => other("feminine")
      case "55" => other("young")
      case "56" => other("diminutive")
      case "57" => other("augmentative")
      case "58" => other("inhabitant")
      case "59" => other("derivation")
      case "60" => other("inter_register_synonym")
      case "62" => other("noun_verb_cross_category_synonym")
      case "63" => other("noun_adjective_cross_category_synonym")
      case "64" => other("taxonomic_meronym")
      case "65" => other("taxonomic_holonym")
      case "74" => other("pure_perfective_imperfective")
      case "75" => other("pure_imperfective_perfective")
      case "89" => other("imperfective_verb_adjective_processuality")
      case "90" => other("imperfective_verb_noun_processuality")
      case "92" => other("verb_adjective_state")
      case "93" => other("verb_noun_state")
      case "101" => other("complementary_antonym")
      case "104" => other("proper_antonym")
      case "106" => other("type")
      case "107" => other("instance_hyponym")
      case "108" => other("synset_fuzzynym")
      case "110" => other("secondary_aspect_perfective_imperfective")
      case "111" => other("secondary_aspect_imperfective_perfective")
      case "113" => other("meronym_of_substitution")
      case "114" => other("meronym_of_accompanying_situation")
      case "116" => other("holonym_of_substitution")
      case "117" => other("holonym_of_accompanying_situation")
      case "118" => other("verb_perfective_adjective_processuality")
      case "119" => other("verb_perfective_noun_processuality")
      case "120" => other("cause_imperfective_imperfective")
      case "121" => other("cause_perfective_perfective")
      case "122" => other("perfective_imperfective_cause_of_state")
      case "124" => other("perfective_adjective_cause_of_process")
      case "125" => other("perfective_noun_cause_of_process")
      case "126" => other("imperfective_adjective_cause_of_process")
      case "127" => other("imperfective_noun_cause_of_process")
      case "129" => other("perfective_imperfective")
      case "130" => other("imperfective_imperfective")
      case "131" => other("reflexive_meaning")
      case "134" => other("iterative_imperfective_imperfective")
      case "136" => other("distributive")
      case "137" => other("presuppositional")
      case "138" => other("preceding")
      case "140" => other("iterative_imperfective_perfective")
      case "141" => other("verb_noun_cross_category_synonym")
      case "142" => other("adjective_noun_cross_category_synonym")
      case "145" => other("value_of_attribute")
      case "146" => other("modifier")
      case "147" => other("gradation")
      case "148" => other("similititudinal_meaning")
      case "149" => other("characterizing")
      case "151" => other("comparative")
      case "152" => other("superlative")
      case "154" => other("agent")
      case "155" => other("patient")
      case "156" => other("instrument")
      case "157" => other("location")
      case "158" => other("time")
      case "160" => other("result")
      case "161" => other("cause")
      case "163" => other("potential")
      case "164" => other("habitual")
      case "165" => other("quantitative")
      case "166" => other("evaluation")
      case "168" => other("markedness_of_intensity")
      case "169" => other("cross_category_synonym_for_relational_adjectives")
      case "194" => other("participle_of_verb")
      case "197" => eq_synonym
      case "198" => eq_synonym
      case "201" => holo_part
      case "202" => holo_member
      case "203" => holo_substance
      case "205" => mero_part
      case "206" => mero_member
      case "207" => mero_substance
      case "211" => hypernym
      case "212" => hyponym
      case "214" => holo_part
      case "215" => holo_member
      case "216" => holo_substance
      case "217" => mero_part
      case "218" => mero_member
      case "219" => mero_substance
      case "225" => other("near_synonym")
      case "226" => other("near_synonym")
      case "228" => other("inter_register_synonym")
      case "229" => other("inter_register_synonym")
      case "230" => instance_hypernym // To Sumo
      case "235" => other("")
      case "238" => other("")
      case "239" => other("")
      case "242" => other("")
      case "244" => other("")
      case "3000" => other("") // ???
      case "3001" => other("") // ???
      case "3002" => other("") // ???
      case "3003" => other("") // ???
      case "3004" => other("") // ???
      case "3005" => other("") // ???
      case "3006" => other("") // ???
      // Relations in both resources
      case "170" => antonym
      case "171" => hyponym
      case "172" => instance_hyponym
      case "173" => hypernym
      case "174" => instance_hypernym
      case "175" => mero_member
      case "176" => mero_substance
      case "177" => mero_part
      case "178" => holo_member
      case "179" => holo_substance
      case "180" => holo_part
      case "181" => attribute
      case "182" => derivation
      case "183" => has_domain_topic
      case "184" => domain_topic
      case "185" => has_domain_region
      case "186" => domain_region
      case "187" => has_domain_usage
      case "188" => domain_usage
      case "189" => entails
      case "190" => causes
      case "191" => also
      case "192" => similar
      case "193" => similar
      case "195" => pertainym
      case "208" => eq_synonym // eq_synonym2
      case "209" => eq_synonym // eq_synonym3
      case "210" => hypernym // hypernym
      case "213" => hyponym // hyponym
      case "222" => other("") // automatic_prompt2
      case "223" => other("") // automatic_prompt3
    }

    def load_pwn(wn : LexicalResource) : (Map[String, Seq[String]], Map[String, Seq[String]], Map[String, String]) = {
      val wordnet = wn.lexicons.find(_.language == Language.ENGLISH).get

      val lemma_synsets = wordnet.entries.map(x => (x.lemma.writtenForm, x.senses.map(_.synsetRef)))

      val synset_definitions = wordnet.synsets.map(x => (x.id, x.definitions.head.content, x.ili.get))

      (lemma_synsets.groupBy(_._1).mapValues(_.flatMap(_._2)),
        synset_definitions.groupBy(_._1).mapValues(_.map(_._2).toSeq),
        synset_definitions.map(x => x._1 -> x._3).toMap)
    }

    def map_plwordnet_to_pwn(entries : Map[String, (String, String)],
                             senses : Map[String, Seq[String]],
                             synsets : Map[String, (Seq[String], String)],
                             pwn_entries : Map[String, Seq[String]],
                             pwn_defns : Map[String, Seq[String]]) :
      (Map[String, String], Map[String, String]) = {
      val entryMapping = entries.map({
        case (plwn_eid, (lemma, _)) =>
           val plwn_defns = senses.getOrElse(plwn_eid, Nil).map(plwn_sid =>
               synsets(plwn_sid)._2.trim())
          val pwn_sids = pwn_entries.getOrElse(lemma, Nil).flatMap({
            pwn_sid => 
              if(pwn_defns.getOrElse(pwn_sid, Nil).exists({ pwn_defn =>
                plwn_defns contains pwn_defn
              })) {
                Seq(pwn_sid)
              } else {
                Seq()
              }
          })
          pwn_sids match {
            case Seq() =>
              plwn_eid -> "in"
            case Seq(pwn_sid) =>
              plwn_eid -> pwn_sid
            case _ =>
              System.err.println("Ambiguous mapping for " + plwn_eid)
              plwn_eid -> pwn_sids.head
          }
      })
      val synsetMapping = entryMapping.flatMap({
        case (plwn_eid, pwn_sid) =>
          senses.getOrElse(plwn_eid, Nil).map({
            plwn_sid =>
              plwn_sid -> pwn_sid
          })
      }).toMap
      (entryMapping.filter(_._2 != "in"), synsetMapping.filter(_._2 != "in"))
    }

    def build_lexicon(en : Boolean,
                 entries : Map[String, (String, String)],
                 synsets : Map[String, (Seq[String], String)],
                 senses  : Map[String, Seq[String]],
                 lexrelations : Map[String, Seq[(String, String)]],
                 synrelations : Map[String, Seq[(String, String)]],
                 entry_mapping : Map[String, String],
                 synset_mapping : Map[String, String],
                 ili : Map[String, String],
                 config : PLWordNetConfig,
                 descriptions : Map[String, Seq[plWNDescription]]) : Lexicon = {
      val entries_grouped : Map[(String, String), Map[String, (String, String)]] = 
          entries.groupBy(x => x._2)
      Lexicon(id=if(en){ "enWordNet" } else { "plWordNet" },
             label=if(en) { "enWordNet" } else { "plWordNet" },
             language=if(en) { Language.ENGLISH } else { Language.POLISH },
             email=config.email,
             license=config.license,
             version=config.version,
             url=config.url,
             citation=config.citation,
             entries={
              for(((name,pos), group) <- entries_grouped) yield {
                LexicalEntry(id=name,
                  lemma=Lemma(writtenForm=name, partOfSpeech=polishToPos(pos)),
                  senses=Seq[Sense]() ++({
                    for(entryid <- group.keys;
                        sense <- senses.getOrElse(entryid, Nil)) yield {
                          Sense(id=entryid,
                            synsetRef=entry_mapping.getOrElse(entryid, "pl-" + sense),
                            senseRelations={
                              for((targ, relType) <- lexrelations.getOrElse(entryid, Nil)) yield {
                                val tid = targ
                                mapRelType(relType) match {
                                  case other("") =>
                                    None
                                  case t : SenseRelType =>
                                    Some(SenseRelation(target=tid, relType=t))
                                  case _ =>
                                    throw new RuntimeException("Synset relation as sense relation")
                                }
                              }
                            }.flatten)
                        }
                  }))
              }
            }.toSeq,
            synsets={
              for((synsetid, (_, defn)) <- synsets) yield {
                val sid = synset_mapping.getOrElse(synsetid, "pl-" + synsetid)
                val iliid = if(en) { Some(ili.getOrElse(sid, "in")) } else { None }
                Synset(id=sid,ili=iliid,
                  definitions=if(defn matches "\\s*") { Seq() } else { Seq(Definition(defn)) },
                  synsetRelations={
                    for((targ, rel) <- synrelations.getOrElse(synsetid, Nil)) yield {
                      val tid = synset_mapping.getOrElse(targ, "pl-" + targ)
                      mapRelType(rel) match {
                        case other("") => None
                        case t : SynsetRelType =>
                          Some(SynsetRelation(target=tid, relType=t))
                        case _ =>
                          throw new RuntimeException("Sense relation as synset relation")
                      }
                    }
                  }.flatten)
              }
            }.toSeq)
    }

    private final val simpleDefinition = "##D:? ?(.*)\\.?\\s*".r

    private final val category = "##K:? ?(.*)\\.?\\s*".r

    private final val otherSource = "##([\\p{IsAlphabetic}-]+): (.*)\\.?\\s*".r

    private final val link = "##L:?\\s*(.*)\\.?\\s*".r

    private final val ref = "<##REF\\d?:? ?(.*)>".r

    private final val emotion  ="##A([123]):? ?(0|\\{\\s*([\\p{IsAlphabetic}, \\-]*)\\s*(;\\s*([\\p{IsAlphabetic}, \\-]*))?\\s*\\}\\s*(amb|0|\\+\\s*m|\\-\\s*m|\\+\\s*s|\\-\\s*s)?\\s*((\\[.*\\]\\s*)*)\\s*)\\.?\\s*".r

    object Sentiment {
      def fromString(s : String) = s match {
        case "0" => NoSent
        case "amb" => Ambivalent
        case "+m" => StrongPos
        case "+ m" => StrongPos
        case "-m" => StrongNeg
        case "- m" => StrongNeg
        case "-  m" => StrongNeg
        case "+s" => WeakPos
        case "+ s" => WeakPos
        case "-s" => WeakNeg
        case "- s" => WeakNeg
        case x =>
          System.err.println("Sentiment" + x)
          NoSent
      }
    }
    object EmotionValue {
      def fromPolish(s : String) : Seq[EmotionValue] = s.trim() match {
        // Universal
        case "bezużyteczność" => Seq(futility)
        case "bląd" => Seq(error)
        case "brzydota" => Seq(ugliness)
        case "brzygota" => Seq(ugliness)
        case "brzytoda" => Seq(ugliness)
        case "bład" => Seq(error)
        case "błąd" => Seq(error)
        case "dobro drugiego człowieka" => Seq(anothers_good)
        case "dobro drugiego" => Seq(anothers_good)
        case "dobro" => Seq(anothers_good)
        case "krzwda" => Seq(harm)
        case "krzyada" => Seq(harm)
        case "krzywda błąd" => Seq(harm, error)
        case "krzywda" => Seq(harm)
        case "niedzczęście" => Seq(sadness)
        case "nieiwedza" => Seq(ignorance)
        case "nieszczeście" => Seq(misfortune)
        case "nieszczęscie" => Seq(misfortune)
        case "nieszczęście" => Seq(misfortune)
        case "nieszczęśćie" => Seq(misfortune)
        case "nieurzyteczność" => Seq(futility)
        case "nieuzyteczność" => Seq(futility)
        case "nieużytecznosć" => Seq(futility)
        case "nieużytecznośc" => Seq(futility)
        case "nieużyteczność" => Seq(futility)
        case "nieużyteczność krzywda" => Seq(futility, harm)
        case "niewiedza" => Seq(ignorance)
        case "nieżuyteczność" => Seq(futility)
        case "nieżyteczność" => Seq(futility)
        case "niużyteczność" => Seq(futility)
        case "piekno" => Seq(beauty)
        case "piękno" => Seq(beauty)
        case "prawda" => Seq(truth)
        case "sz częście" => Seq(happiness)
        case "szczeście" => Seq(happiness)
        case "szczęści" => Seq(happiness)
        case "szczęście użyteczność" => Seq(happiness, utility)
        case "szczęśćie" => Seq(happiness)
        case "szczęscie" => Seq(happiness)
        case "uzyteczność" => Seq(utility)
        case "użuyteczność" => Seq(utility)
        case "użytecznosć" => Seq(utility)
        case "użytecznośc" => Seq(utility)
        case "użyteczność dobro" => Seq(anothers_good, utility)
        case "użyteczność szczęście" => Seq(utility, happiness)
        case "użyteczność wiedza" => Seq(utility, knowledge)
        case "użyteczność" => Seq(utility)
        case "wiedza" => Seq(knowledge)
        // Primary
        case "cieszenie sie na" => Seq(anticipation)
        case "cieszenie sie" => Seq(anticipation)
        case "cieszenie się na" => Seq(anticipation)
        case "cieszenie się" => Seq(anticipation)
        case "ciesznie się na" => Seq(anticipation)
        case "gniew" => Seq(anger)
        case "oczekiwanie na" => Seq(anticipation)
        case "radosć" => Seq(joy)
        case "radoć" => Seq(joy)
        case "radośc" => Seq(joy)
        case "radość" => Seq(joy)
        case "s mutek" => Seq(sadness)
        case "smitek" => Seq(sadness)
        case "smute" => Seq(sadness)
        case "smutek" => Seq(sadness)
        case "strach wstręt" => Seq(fear, disgust)
        case "strach" => Seq(fear)
        case "szczęście" => Seq(happiness)
        case "wstret" => Seq(disgust)
        case "wstrę" => Seq(disgust)
        case "wstręt" => Seq(disgust)
        case "wstęt" => Seq(disgust)
        case "zaskoczenie" => Seq(surprise)
        case "zaufanie złość" => Seq(trust, anger)
        case "zaufanie" => Seq(trust)
        case "zlość" => Seq(anger)
        case "zufanie" => Seq(trust)
        case "złosć" => Seq(anger)
        case "złośc" => Seq(anger)
        case "złość" => Seq(anger)
        case "złóść" => Seq(anger)
        case "złość wstręt" => Seq(anger, disgust)
        case "-" => Nil
        case "" => Nil
        case _ =>
          System.err.println("Unrecognized emotion: " + s)
          Nil
      }
    }



    val recognitionErrors = new java.io.PrintWriter("desc-errors.txt")
    var errorCount = 0

    def parseDescription(desc : String) : Seq[plWNDescription] = {
      def ri(i : Int) = if(i < 0) { Int.MaxValue -100} else { i }
      def elems(d : String) : List[String] = {
        val i1 = ri(d.indexOf("##"))
        val i2 = ri(d.indexOf("[##"))
        val i3 = ri(d.indexOf("{##"))
        val i4 = ri(d.indexOf("<##"))
        if(i1 < i2 && i1 < i3 && i1 < i4 && i1 != Int.MaxValue) {
          if(i1 == 0) {
            val j1 = ri(d.indexOf("##", 2))
            val j = math.min(math.min(j1, i2), math.min(i3, i4))
            d.take(j) :: elems(d.drop(j))
          } else {
            d.take(i1) :: elems(d.drop(i1))
          }
        } else if(i2 < i1 && i2 < i3 && i2 < i4 && i2 != Int.MaxValue) {
          val i = ri(d.indexOf("]", i2))
          if(i2 != 0) {
            d.take(i2) :: d.slice(i2+1,i) :: elems(d.drop(i+1))
          } else {
            d.slice(i2+1,i) :: elems(d.drop(i+1))
          }
        } else if(i3 < i1 && i3 < i2 && i3 < i4 && i3 != Int.MaxValue) {
          val i = ri(d.indexOf("}", i3))
          assert(i > i3)
          if(i3 != 0) {
            d.take(i3) :: d.slice(i3+1,i) :: elems(d.drop(i+1))
          } else {
            d.slice(i3+1,i) :: elems(d.drop(i+1))
          }
        } else if(i4 < i1 && i4 < i2 && i4 < i3 && i4 != Int.MaxValue) {
          val i = ri(d.indexOf(">", i4))
          if(i4 != 0) {
            d.take(i4) :: d.slice(i4,i+1) :: elems(d.drop(i+1))
          } else {
            d.slice(i4,i+1) :: elems(d.drop(i+1))
          }
        } else if(d matches "\\s*") {
          Nil
        } else {
          d :: Nil
        }
      }
      if(desc contains "##") {
        var hasError = false
        (for(section <- elems(desc)) yield {
          section match {
            case simpleDefinition(text) =>
              Some(SimpleDefinition(text))
            case category(value) =>
              Some(Category(value))
            case link(to) =>
              Some(Link(to))
            case emotion(an, v, pes, _, ues, sent, ex, _) =>
              if(v == "0") {
                None
              } else {
                Some(Emotion(an.toInt, 
                  pes.split(",\\s*").flatMap(EmotionValue.fromPolish), 
                  Option(ues).getOrElse("").split(",\\s*").
                    flatMap(EmotionValue.fromPolish), 

                  Option(sent).map(Sentiment.fromString).getOrElse(NoSent), 
                  ex.trim().drop(1).dropRight(1).split("\\] \\[").map(_.trim())))
              }
            case ref(to) =>
              Some(Ref(to))
            case otherSource(source, text) =>
              Some(OtherDefinition(source, text))
            case "<##s>" => None //- compositional word combination
            case "<##aDD>" => None // - multi-word lexical unit -- convrt to <##DD>
            case "<##as1DD>" => None // - multi-word lexical unit exhibiting syntactic nonseparability
            case "<##as2DD>" => None // - multi-word lexical unit exhibiting fixed word order
            case "<##nDD>" => None // - compositional word combination
            case "<##DD>" => None //  ??
            case "<##DS>" => None //  ??
            case "<##sDD>" => None //  ??
            case "brak danych" => None // no data!
            case x => 
              if(!(x contains "##") && !x.matches("\\s*")) {
                Some(SimpleDefinition(x))
              } else {
                if(!x.matches("\\s*")) {
                  if(!hasError) {
                    recognitionErrors.println(x.replaceAll("\n","\\\\n").replaceAll("\r", "\\\\r"))
                    hasError = true
                    errorCount += 1
                  }
                }
                None
              }
          }
        }).flatten
      } else {
        Seq(SimpleDefinition(desc))
      }
    }

    def read(plWordNetFile : File, config : PLWordNetConfig, wn31 : LexicalResource) :
      LexicalResource = {
      var entry_mapping : Map[String, String] = null
      var synset_mapping : Map[String, String] = null
      val enLexicon = {
        val (entries, synsets, lexrelations, synrelations, descriptions) = load_plwordnet(true, plWordNetFile)

        val enD = descriptions.mapValues(parseDescription)
        val (pwn_entries, pwn_defns, ili) = load_pwn(wn31)

        val senses = build_senses(synsets)

        val (em, sm) = map_plwordnet_to_pwn(entries, senses, synsets, pwn_entries, pwn_defns)
        entry_mapping = em
        synset_mapping = sm

        build_lexicon(true, entries, synsets, senses, lexrelations, synrelations, entry_mapping,
          synset_mapping, ili, config, enD)

      }
      val plLexicon = {
        val (entries, synsets, lexrelations, synrelations, descriptions) = load_plwordnet(false, plWordNetFile)

        val plD = descriptions.mapValues(parseDescription)
        System.err.println("Error Count: %d" format (errorCount))
        recognitionErrors.flush()

        val senses = build_senses(synsets)

        build_lexicon(false, entries, synsets, senses, lexrelations, synrelations, 
          entry_mapping, synset_mapping, Map(), config, plD)
      }
      LexicalResource(Seq(enLexicon, plLexicon))
    }
  }
}
