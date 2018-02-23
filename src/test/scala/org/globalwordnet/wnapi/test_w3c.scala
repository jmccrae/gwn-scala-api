package org.globalwordnet.api.serialize

import eu.monnetproject.lang.{Language, Script}
import org.globalwordnet.api.wn._
import org.scalatest._

class W3CSpec extends FlatSpec with Matchers {
  var resource : LexicalResource = null
  "RDF reader" should "successfully read an RDF file" in {
    val w3c = new W3C("example-en", "Example wordnet (English)",
      Language.ENGLISH, "john@mccr.ae", "https://creativecommons.org/publicdomain/zero/1.0/",
      "1.0", Some("http://globalwordnet.github.io/schemas/"), 
        Some("CILI: the Collaborative Interlingual Index. Francis Bond, Piek Vossen, John P. McCrae and Christiane Fellbaum, Proceedings of the Global WordNet Conference 2016, (2016)."))
    resource = w3c.read(new java.io.File("src/test/resources/example-w3c.xml"))
  }
  it should "produce 1 lexicons" in {
    resource.lexicons.size should be (1)
  }
  it should "get features of lexicon" in {
    val lexicon = resource.lexicons(0)
    lexicon.id should be ("example-en")
    lexicon.label should be ("Example wordnet (English)")
    lexicon.language should be (Language.ENGLISH)
    lexicon.email should be ("john@mccr.ae")
    lexicon.license should be ("https://creativecommons.org/publicdomain/zero/1.0/")
    lexicon.version should be ("1.0")
    lexicon.citation should be (Some("CILI: the Collaborative Interlingual Index. Francis Bond, Piek Vossen, John P. McCrae and Christiane Fellbaum, Proceedings of the Global WordNet Conference 2016, (2016)."))
    lexicon.url should be (Some("http://globalwordnet.github.io/schemas/"))
  }
  it should "produce 2 english entries" in {
    resource.lexicons.find(_.id == "example-en").get.entries.size should be (3)
  }
  it should "produce 2 synset" in {
    resource.lexicons.find(_.id == "example-en").get.synsets.size should be (3)
  }
  it should "produce an entry for grandfather" in {
    val e = resource.lexicons.find(_.id == "example-en").get.entries.find(_.id == "word-grandfather").get
    e.id should be ("word-grandfather")
    e.lemma.writtenForm should be ("grandfather")
    e.lemma.partOfSpeech should be (noun)
    e.senses(0).id should be ("wordsense-grandfather-noun-1")
    e.senses(0).synsetRef should be ("example-en-109490348")
  }
  it should "produce a sense" in {
    val s = resource.lexicons.find(_.id == "example-en").get.entries.find(_.id == "word-stretch").get.senses(0)
    s.id should be ("wordsense-stretch-verb-2")
    s.synsetRef should be ("example-en-200026695")
  }
  it should "produce a sense relation" in {
    val s = resource.lexicons.find(_.id == "example-en").get.entries.find(_.id == "word-stretching").get.senses(0)
    assert(s.senseRelations.size > 0)
    val r = s.senseRelations(0)
    r.relType should be (derivation)
    r.target should be ("wordsense-stretch-verb-2")
  }
  it should "produce syntactic behavior" in {
    resource.lexicons.find(_.id == "example-en").get.entries.find(_.id == "word-stretch").get.syntacticBehaviours.size should be (1)
    val s = resource.lexicons.find(_.id == "example-en").get.entries.find(_.id == "word-stretch").get.syntacticBehaviours.find(_.subcategorizationFrame == "Somebody %s")
    s should not be (None)
  }
  it should "produce a synset" in {
    val s = resource.lexicons.find(_.id == "example-en").get.synsets.find(_.id == "example-en-109490348").get
    s.id should be ("example-en-109490348")
    s.ili should be (None)
//    s.ili should be (Some("i90287"))
    s.definitions(0).content should be ("the father of your father or mother")
    assert(s.synsetRelations.size > 0)
    s.synsetRelations(0).relType should be (hypernym)
    s.synsetRelations(0).target should be ("example-en-200026695")
  }
}
