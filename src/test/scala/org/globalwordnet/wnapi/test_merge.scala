package org.globalwordnet.api.util

import eu.monnetproject.lang.Language
import org.globalwordnet.api.wn._
import org.scalatest._

class MergeTest extends FlatSpec with Matchers {
  "merge" should "join two lexicons" in {
    val lr1 = LexicalResource(Seq(
      Lexicon("wn30", "label", Language.ENGLISH, "email",
        "license", "version", entries=Seq(LexicalEntry(
          "wn30-cat", Lemma("cat", noun), senses=Seq(
            Sense("wn30-cat-1", "wn30-02121620-n",
              senseRelations=Seq(SenseRelation("wn30-cat-1", derivation)))))),
        synsets=Seq(Synset(
          "wn30-02121620-n", ili=Some("i46593")))
      )))
    val lr2 = LexicalResource(Seq(
      Lexicon("wn31", "label", Language.ENGLISH, "email",
        "license", "version", entries=Seq(LexicalEntry(
          "wn31-cat", Lemma("cat", noun), senses=Seq(
            Sense("wn31-cat-1", "wn31-02124272-n")))),
        synsets=Seq(Synset(
          "wn31-02124272-n", ili=Some("i46593"),
          synsetRelations=Seq(SynsetRelation("wn31-02124272-n", hypernym))))
      )))
    val result = Merge.merge(lr1, lr2)
    result.lexicons should have size 1
    val lexicon = result.lexicons(0)
    lexicon.id should be ("wn30+wn31")
    lexicon.label should be ("label+label")
    lexicon.language should be (Language.ENGLISH)
    lexicon.email should be ("email,email")
    lexicon.license should be ("license,license")
    lexicon.version should be ("version [wn30], version [wn31]")
    lexicon.entries should have size 1
    val entry = lexicon.entries(0)
    entry.id should be ("wn30-cat+wn31-cat")
    entry.lemma should be (Lemma("cat", noun))
    entry.senses should have size 1
    val sense = entry.senses(0)
    sense.id should be (makeId(makeId("cat-n") + makeId("i46593")))
    sense.synsetRef should be ("wn30+wn31-" + makeId("i46593"))
    sense.senseRelations should have size 1
    val senseR = sense.senseRelations(0)
    senseR should be (SenseRelation(sense.id, derivation))
    lexicon.synsets should have size 1
    val synset = lexicon.synsets(0)
    synset.id should be ("wn30+wn31-"+makeId("i46593"))
    synset.ili should be (Some("i46593"))
    synset.synsetRelations should have size 1
    synset.synsetRelations(0) should be (SynsetRelation(synset.id, hypernym))
  }
}
