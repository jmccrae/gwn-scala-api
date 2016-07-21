package org.globalwordnet.api.serialize

import eu.monnetproject.lang.{Language, Script}
import org.globalwordnet.api.wn._
import org.scalatest._

class RDFSpec extends FlatSpec with Matchers {
  var resource : LexicalResource = null
  "RDF reader" should "successfully read an RDF file" in {
    resource = WNRDF.read(new java.io.File("src/test/resources/example.ttl"))
  }
  it should "produce 2 lexicons" in {
    resource.lexicons.size should be (2)
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
    lexicon.publisher should be (Some("Global Wordnet Association"))
  }
  it should "produce 3 english entries" in {
    resource.lexicons.find(_.id == "example-en").get.entries.size should be (3)
  }
  it should "produce 2 synset" in {
    resource.lexicons.find(_.id == "example-en").get.synsets.size should be (2)
  }
  it should "produce an entry for grandfather" in {
    val e = resource.lexicons.find(_.id == "example-en").get.entries.find(_.id == "w1").get
    e.id should be ("w1")
    e.lemma.writtenForm should be ("grandfather")
    e.lemma.partOfSpeech should be (noun)
    e.senses(0).id should be ("example-en-10161911-n-1")
    e.senses(0).synsetRef should be ("example-en-10161911-n")
  }
  it should "produce a sense" in {
    val s = resource.lexicons.find(_.id == "example-en").get.entries.find(_.id == "w2").get.senses(0)
    s.id should be ("example-en-1-n-1")
    s.synsetRef should be ("example-en-1-n")
  }
  it should "produce a sense relation" in {
    val s = resource.lexicons.find(_.id == "example-en").get.entries.find(_.id == "w2").get.senses(0)
    val r = s.senseRelations(0)
    r.relType should be (derivation)
    r.target should be ("example-en-10161911-n-1")
  }
  it should "produce syntactic behavior" in {
    resource.lexicons.find(_.id == "example-en").get.entries.find(_.id == "w3").get.syntacticBehaviours.size should be (3)
    val s = resource.lexicons.find(_.id == "example-en").get.entries.find(_.id == "w3").get.syntacticBehaviours.find(_.subcategorizationFrame == "Sam cannot %s Sue")
    s should not be (None)
  }
  it should "produce a synset" in {
    val s = resource.lexicons.find(_.id == "example-en").get.synsets.find(_.id == "example-en-10161911-n").get
    s.id should be ("example-en-10161911-n")
    s.ili should be (Some("i90287"))
    s.definitions(0).content should be ("the father of your father or mother")
    s.synsetRelations(0).relType should be (hypernym)
    s.synsetRelations(0).target should be ("example-en-10162692-n")
  }
  val f = java.io.File.createTempFile("lexicon", ".ttl")
  f.deleteOnExit()
  "RDF Writer" should "write a file" in {
    WNRDF.write(resource, f)
  }
  var resource2 : LexicalResource = null
  "roundtripping" should "work" in {
    resource2 = WNRDF.read(f)
  }
  it should "produce 2 lexicons" in {
    resource2.lexicons.size should be (2)
  }
  it should "get features of lexicon" in {
    val lexicon = resource2.lexicons.find(_.id == "example-en").get
    lexicon.id should be ("example-en")
    lexicon.label should be ("Example wordnet (English)")
    lexicon.language should be (Language.ENGLISH)
    lexicon.email should be ("john@mccr.ae")
    lexicon.license should be ("https://creativecommons.org/publicdomain/zero/1.0/")
    lexicon.version should be ("1.0")
    lexicon.citation should be (Some("CILI: the Collaborative Interlingual Index. Francis Bond, Piek Vossen, John P. McCrae and Christiane Fellbaum, Proceedings of the Global WordNet Conference 2016, (2016)."))
    lexicon.url should be (Some("http://globalwordnet.github.io/schemas/"))
    lexicon.publisher should be (Some("Global Wordnet Association"))
  }
  it should "produce 3 english entries" in {
    resource2.lexicons.find(_.id == "example-en").get.entries.size should be (3)
  }
  it should "produce 2 synset" in {
    resource2.lexicons.find(_.id == "example-en").get.synsets.size should be (2)
  }
  it should "produce an entry for grandfather" in {
    val e = resource2.lexicons.find(_.id == "example-en").get.entries.find(_.id == "w1").get
    e.id should be ("w1")
    e.lemma.writtenForm should be ("grandfather")
    e.lemma.partOfSpeech should be (noun)
    e.senses(0).id should be ("example-en-10161911-n-1")
    e.senses(0).synsetRef should be ("example-en-10161911-n")
  }
  it should "produce a sense" in {
    val s = resource2.lexicons.find(_.id == "example-en").get.entries.find(_.id == "w2").get.senses(0)
    s.id should be ("example-en-1-n-1")
    s.synsetRef should be ("example-en-1-n")
  }
  it should "produce a sense relation" in {
    val s = resource2.lexicons.find(_.id == "example-en").get.entries.find(_.id == "w2").get.senses(0)
    val r = s.senseRelations(0)
    r.relType should be (derivation)
    r.target should be ("example-en-10161911-n-1")
  }
  it should "produce syntactic behavior" in {
    resource2.lexicons.find(_.id == "example-en").get.entries.find(_.id == "w3").get.syntacticBehaviours.size should be (3)
    val s = resource2.lexicons.find(_.id == "example-en").get.entries.find(_.id == "w3").get.syntacticBehaviours.find(_.subcategorizationFrame == "Sam cannot %s Sue")
    s should not be (None)
  }
  it should "produce a synset" in {
    val s = resource2.lexicons.find(_.id == "example-en").get.synsets.find(_.id == "example-en-10161911-n").get
    s.id should be ("example-en-10161911-n")
    s.ili should be (Some("i90287"))
    s.definitions(0).content should be ("the father of your father or mother")
    s.synsetRelations(0).relType should be (hypernym)
    s.synsetRelations(0).target should be ("example-en-10162692-n")
  }
 }
