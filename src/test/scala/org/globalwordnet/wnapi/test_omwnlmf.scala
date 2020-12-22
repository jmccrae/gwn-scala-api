package org.globalwordnet.api.serialize

import eu.monnetproject.lang.{Language, Script}
import org.globalwordnet.api.wn._
import org.scalatest._

class OMWNLMFSpec extends FlatSpec with Matchers {
  var resource : LexicalResource = null
  "OMWN LMF reader" should "successfully read an XML file" in {
    resource = new OMWNLMF("test@example.com", "http://www.example.com").read(new java.io.File("src/test/resources/example-omwn.xml"))
  }
  it should "produce 2 lexicon" in {
    resource.lexicons.size should be (1)
  }
  it should "have 1 lexical entry" in {
    resource.lexicons(0).entries.size should be (1)
  }
  it should "have 5 synsets" in {
    resource.lexicons(0).synsets.size should be (5)
  }
  it should "have read a synset correctly" in {
    val ss = resource.lexicons(0).synsets.find(_.id == "nld-10-03234306-n").get
    ss.contributor should be (Some("test author"))
    ss.date should be (Some("2020-12-22"))
    ss.source should be (Some("test source"))
    ss.status should be (Some("ready"))
    ss.confidenceScore should be (Some(0.5))
    ss.definitions.size should be (1)
    ss.definitions(0).content should be ("This is an example definition")
    ss.synsetExamples.size should be (1)
    ss.synsetExamples(0).content should be ("This is an example example")
    ss.synsetRelations.size should be (1)
    ss.synsetRelations(0) should be (SynsetRelation("nld-10-03234306-n", hypernym))
  }
  it should "have read an entry correctly" in {
    val e = resource.lexicons(0).entries(0)
    e.lemma.writtenForm should be ("plan")
    e.lemma.partOfSpeech should be (noun)
    e.senses.size should be (3)
    e.senses(0).synsetRef should be ("nld-10-03954199-n")
  }
  it should "have an ID" in {
    resource.lexicons(0).id should be ("nld")
  }

}
