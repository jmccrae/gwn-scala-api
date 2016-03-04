package org.globalwordnet.api.util

import org.globalwordnet.api.wn._

object Merge {
  def merge(lr1 : LexicalResource, lr2 : LexicalResource) : LexicalResource = {
    val lrByLang1 = lr1.lexicons.groupBy(_.language)
    val lrByLang2 = lr2.lexicons.groupBy(_.language)

    LexicalResource(
      (lrByLang1.keys.toSet ++ lrByLang2.keys).toSeq.map({ lang =>
        (lrByLang1.getOrElse(lang, Seq()) ++ 
          lrByLang2.getOrElse(lang, Seq())).reduce(mergeLexicon(_, _, lr1, lr2))
      }))
  }

  def mergeStringOpt(s1 : Option[String], s2 : Option[String]) = s1 match {
    case Some(s) => 
      s2 match {
        case Some(t) =>
          Some(s + "," + t)
        case None =>
          Some(s)
      }
    case None =>
      s2 match {
        case Some(t) =>
          Some(t)
        case None =>
          None
      }
  }



  def mergeLexicon(l1 : Lexicon, l2 : Lexicon, lr1 : LexicalResource, lr2 : LexicalResource) : Lexicon = {
    val entries1 = l1.entries.groupBy(e => (e.lemma.writtenForm, e.lemma.partOfSpeech))
    val entries2 = l2.entries.groupBy(e => (e.lemma.writtenForm, e.lemma.partOfSpeech))
    val synsets1 = l1.synsets.filter(_.ili != None).groupBy(s => s.ili.get)
    val synsets2 = l2.synsets.filter(_.ili != None).groupBy(s => s.ili.get)
    def lookupSynset(id : String) : String = {
      val s = lr1.synsetLookup.getOrElse(id, 
        lr2.synsetLookup.getOrElse(id,
          throw new RuntimeException("Reference to unknown synset")))
      s.ili match {
        case None =>
          s.id
        case Some(s) =>
          makeId(s)
      }
    }

    def lookupSense(lemma : Lemma, id : String) : String = {
      val s = lr1.senseLookup.getOrElse(id,
        lr2.senseLookup.getOrElse(id,
          throw new RuntimeException("Reference to unknown sense: " + id)))._2
      val ss = lr1.synsetLookup.getOrElse(s.synsetRef,
        lr2.synsetLookup.getOrElse(s.synsetRef,
          throw new RuntimeException("Reference to unknown synset: " + s.synsetRef)))
      ss.ili match {
        case None =>
          makeId(makeId(lemma.writtenForm + "-" + lemma.partOfSpeech.shortForm) + ss.id)
        case Some(s) =>
          makeId(makeId(lemma.writtenForm + "-" + lemma.partOfSpeech.shortForm) + makeId(s))
      }
    }

    Lexicon(
      id=l1.id + "+" + l2.id,
      label=l1.label + "+" + l2.label,
      language=l1.language,
      email=l1.email + "," + l2.email,
      license=l1.license + "," + l2.license,
      version="%s [%s], %s [%s]" format (l1.version, l1.id, l2.version, l2.id),
      url=mergeStringOpt(l1.url, l2.url),
      citation=mergeStringOpt(l1.citation, l2.citation),
      entries=(entries1.keys ++ entries2.keys).toSet.toSeq.map({ x : (String, PartOfSpeech) =>
        (entries1.getOrElse(x, Seq()) ++ entries2.getOrElse(x, Seq())).reduce(mergeEntry(_, _, lookupSense, lookupSynset))
      }),
      synsets=(synsets1.keys ++ synsets2.keys).toSet.toSeq.map({ x : String =>
        val ss = (synsets1.getOrElse(x, Seq()) ++ synsets2.getOrElse(x, Seq())).
          reduce(mergeSynset(_, _, lookupSynset))
        ss.copy(id=lookupSynset(ss.id))
      })
    )
  }

  def mergeEntry(e1 : LexicalEntry, e2 : LexicalEntry, lookup : (Lemma, String) => String,
    ssLookup : String => String) = {
    assert(e1.lemma == e2.lemma)
    val senses1 = e1.senses.groupBy(s => lookup(e1.lemma, s.id))
    val senses2 = e1.senses.groupBy(s => lookup(e2.lemma, s.id))
    
    LexicalEntry(
      id=e1.id + "+" + e2.id,
      lemma=e1.lemma,
      forms=(e1.forms ++ e2.forms).toSet.toSeq,
      senses=(senses1.keys ++ senses2.keys).toSet.toSeq.map({ x : String =>
        val s = (senses1.getOrElse(x, Seq()) ++ senses2.getOrElse(x, Seq())).
          reduce(mergeSense(_, _, lookup(e1.lemma, _), ssLookup))
        s.copy(id=lookup(e1.lemma, s.id),synsetRef=ssLookup(s.synsetRef))
      }),
      syntacticBehaviours=(e1.syntacticBehaviours ++ e2.syntacticBehaviours).toSet.toSeq)
  }

  def mergeSense(s1 : Sense, s2 : Sense, lookup : String => String,
    ssLookup : String => String) = Sense(
    id=s1.id,
    synsetRef=s1.synsetRef,
    senseRelations=(s1.senseRelations ++ s2.senseRelations).map(mapSenseRelation(_, lookup)).toSet.toSeq,
    senseExamples=(s1.senseExamples ++ s2.senseExamples).toSet.toSeq,
    counts=(s1.counts ++ s2.counts).toSet.toSeq)

  def mapSenseRelation(sr : SenseRelation, lookup : String => String) = {
    sr.copy(target=lookup(sr.target))
  }

  def mergeSynset(s1 : Synset, s2 : Synset, lookup : String => String) : Synset = {
    assert(s1.ili == s2.ili)
    println(s1)
    println(s2)
    Synset(
      id=s1.id,
      ili=s1.ili,
      definitions=(s1.definitions ++ s2.definitions).toSet.toSeq,
      synsetRelations=(s1.synsetRelations ++ s2.synsetRelations).map(mapSynsetRelation(_, lookup)).toSet.toSeq,
      synsetExamples=(s1.synsetExamples ++ s2.synsetExamples).toSet.toSeq)
  }

  def mapSynsetRelation(sr : SynsetRelation, lookup : String => String) = {
    sr.copy(target=lookup(sr.target))
  }
}
