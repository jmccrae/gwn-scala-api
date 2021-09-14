package org.globalwordnet.api.serialize

import eu.monnetproject.lang.Language
import java.io.File
import org.globalwordnet.api.wn._
import org.globalwordnet.api.util.makeId

object OpenMultilingualWordNet {
  def read(file : File, wn30 : LexicalResource, defLang : String = "und", prefix : String = "wn30-") : LexicalResource = {
    buildRaw(readRaw(file, defLang, prefix), wn30)
  }

  private final val langRel = "(\\w{2,3}):(.*)".r

  private def buildRaw(input : Iterator[(String, (String, String, String))],
      baseResource : LexicalResource) = {
    val m : Map[String, Map[String, Seq[(String, String)]]] = input.toSeq.groupBy(_._1).mapValues({
      ss =>
        ss.map(_._2).groupBy(_._1).mapValues(_.map(x => (x._2, x._3)))
    })
    val languages = m.keys.map(Language.get).toSet ++ baseResource.lexicons.map(_.language) - Language.ENGLISH
    LexicalResource(
      Seq(buildEnLexicon(m, baseResource.lexicons.find(_.language == Language.ENGLISH).get)) ++
      languages.map(lang => buildLexicon(lang, 
        m.getOrElse(lang.getIso639_3(), Map()), 
        baseResource.lexicons.find(_.language == lang).getOrElse(
          Lexicon("omwn-" + lang, "Open Multilingual Wordnet " + lang,
            lang, "", "", "")
          )
        )))
  }

  private def buildEnLexicon(props : Map[String, Map[String, Seq[(String, String)]]],
    enLexicon : Lexicon) : Lexicon = {
    val elements : Map[String, Map[String, Seq[(Language, String)]]] = (
      for{
        (lang, byProp) <- props.toSeq;
        (prop, ses) <- byProp;
        (synset, value) <- ses
      } yield {
        synset -> (prop -> (Language.get(lang), value))
      }
    ).groupBy(_._1).mapValues(_.map(_._2).groupBy(_._1).mapValues(_.map(_._2)))
    enLexicon match {
      case Lexicon(id, label, language, email, license, version, url, citation, entries, synsets, Nil) =>
        Lexicon(id, label, language, email, license, version,
          url, citation, entries,
          synsets.map({
            case Synset(id, ili, definitions, iliDefinition, synsetRelations, synsetExamples, partOfSpeech, Nil) =>
              Synset(id, ili, definitions ++ elements.getOrElse(id, Map()).getOrElse("definition", Nil).map({
                case (lang, value) => Definition(value, Some(lang))
              }), iliDefinition, synsetRelations,
              synsetExamples ++ elements.getOrElse(id, Map()).getOrElse("example", Nil).map({
                case (lang, value) => Example(value, Some(lang))
              }), partOfSpeech)
          }))
    }
  }

  private def buildLexicon(lang : Language, props : Map[String, Seq[(String, String)]], 
      lexicon : Lexicon) : Lexicon = lexicon match {
    case Lexicon(id, label, language, email, license, version, url, citation, entries, synsets, Nil) =>
      val lemmas = props.getOrElse("lemma", Nil)
      Lexicon(
        id, label, language, email, license, version, url, citation,
        entries ++ lemmas.map({ 
          case (synset, lemma) =>
            LexicalEntry(
              id=makeId("entry-%s-%s" format (language, lemma)),
              lemma=Lemma(
                lemma, 
                PartOfSpeech.fromString(synset.takeRight(1))),
              senses=Seq(
                Sense(id=makeId("sense-%s-%s" format (language, lemma)), 
                  synsetRef=synset)))
        }), synsets)
  }

  private def readRaw(file : File, defLang : String, prefix : String) = {
    for(line <- io.Source.fromFile(file).getLines if !line.startsWith("#")) yield {
      val elems = line.split("\t")
      val synset = prefix + elems(0)
      val rel = elems(1)
      val target = elems.drop(2).mkString("\t")
      val (lang, relType) = rel match {
        case langRel(lang, rel) =>
          (lang, rel)
        case other =>
          (defLang, other)
      }
      relType match {
        case "lemma" =>
          lang -> ("lemma", synset, target)
        case "exe" =>
          lang -> ("example", synset, target)
        case "def" =>
          lang -> ("definition", synset, target)
        case "lemma:root" =>
          lang -> ("lemma:root", synset, target)
        case "lemma:brokenPlural" =>
          lang -> ("lemma:brokenPlural", synset, target)
      }
    }
  }
}
