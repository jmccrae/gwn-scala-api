package org.globalwordnet.api.serialize

import eu.monnetproject.lang.{Language, Script}
import java.io.File
import org.globalwordnet.api.wn._
import org.scalatest.{Tag => _, _}

class CoverageTest extends FlatSpec with Matchers {
  var resource : LexicalResource = null
  def testMetaData(m : Meta) {
    m.contributor should be (Some("contributor"))
    m.coverage should be (Some("coverage"))
    m.creator should be (Some("creator"))
    m.date should be (Some("date"))
    m.description should be (Some("description"))
    m.format should be (Some("format"))
    m.identifier should be (Some("identifier"))
    m.publisher should be (Some("publisher"))
    m.relation should be (Some("relation"))
    m.rights should be (Some("rights"))
    m.source should be (Some("source"))
    m.subject should be (Some("subject"))
    m.title should be (Some("title"))
    m.`type` should be (Some("type"))
    m.status should be (Some("status"))
    m.note should be (Some("note"))
    m.confidenceScore should be (Some(1.0))
  }

  def testCoverage(r : LexicalResource) {
    r.lexicons should have size (1)
    val l = r.lexicons(0)
    l.id should be ("ex2")
    l.label should be ("Example")
    l.language should be (Language.GERMAN)
    l.email should be ("email")
    l.license should be ("http://www.license.com/")
    l.version should be ("1.0")
    l.citation should be (Some("citation"))
    l.url should be (Some("http://example.com/"))
    testMetaData(l)
    l.entries should have size (10)
    val e = l.entries.find(_.id == "w1").get
    testMetaData(e)
    e.lemma.writtenForm should be ("test")
    e.lemma.script should be (Some(Script.LATIN))
    e.lemma.partOfSpeech should be (noun)
    e.forms should have size (1)
    e.lemma.tag should be (Seq(Tag("penn", "NN")))
    val f = e.forms(0)
    f.writtenForm should be ("tests")
    f.script should be (Some(Script.CYRILLIC))
    f.tag should be (Seq(Tag("penn","NNS")))
    e.senses should have size (1)
    val ws = e.senses(0)
    ws.id should be ("ws1")
    ws.synsetRef should be ("ex2-s1")
    testMetaData(ws)
    ws.senseRelations should have size (14)
    for(r <- ws.senseRelations) {
      if(r.note != None) {
        testMetaData(r)
      }
      r.target should be ("ws1")
    }
    ws.senseRelations.exists(_.note != None) should be (true)
    ws.senseExamples should have size 1
    val x = ws.senseExamples(0)
    x.content should be ("example")
    x.language should be (Some(Language.FRENCH))
    testMetaData(x)
    ws.counts should have size 1
    val c = ws.counts(0)
    c.value should be (10)
    testMetaData(x)
    e.syntacticBehaviours should have size 1
    val sb = e.syntacticBehaviours(0)
    sb.subcategorizationFrame should be ("subcategorizationFrame")
    l.entries.find(_.id == "w2").get.lemma.partOfSpeech should be (org.globalwordnet.api.wn.verb)
    l.entries.find(_.id == "w3").get.lemma.partOfSpeech should be (adjective)
    l.entries.find(_.id == "w4").get.lemma.partOfSpeech should be (adverb)
    l.entries.find(_.id == "w5").get.lemma.partOfSpeech should be (adjective_satellite)
    l.entries.find(_.id == "w6").get.lemma.partOfSpeech should be (named_entity)
    l.entries.find(_.id == "w7").get.lemma.partOfSpeech should be (conjunction)
    l.entries.find(_.id == "w8").get.lemma.partOfSpeech should be (adposition)
    l.entries.find(_.id == "w9").get.lemma.partOfSpeech should be (other_pos)
    l.entries.find(_.id == "w0").get.lemma.partOfSpeech should be (unknown_pos)

    l.synsets should have size 1
    val s = l.synsets(0)
    s.id should be ("ex2-s1")
    s.ili should be (Some("i1"))
    testMetaData(s)
    
    s.definitions should have size 1
    val d = s.definitions(0)
    d.language should be (Some(Language.ENGLISH))
    testMetaData(d)
    d.content should be ("definition")
    d.sourceSense should be (Some("ws1"))

    s.iliDefinition should not be None
    testMetaData(s.iliDefinition.get)
    s.iliDefinition.get.content should be ("iliDefinition")

    s.synsetRelations should have size (71)
    for(r <- s.synsetRelations) {
      if(r.note != None) {
        testMetaData(r)
      }
      r.target should be ("ex2-s1")
    }
    s.synsetRelations.exists(_.note != None) should be (true)
 
    s.synsetExamples should have size (1)
    val x2 = s.synsetExamples(0)
    x2.content should be ("example")
    x2.language should be (Some(Language.FRENCH))
    testMetaData(x2)
  }

  "XML reader" should "read the coverage xml" in {
    resource = WNLMF.read(new java.io.File("src/test/resources/example2.xml"))
  }
  it should "provide coverage" in {
    testCoverage(resource)
  }
  val xmlFile = File.createTempFile("lexicon", ".xml")
  xmlFile.deleteOnExit()
  it should "write the coverage model" in {
    WNLMF.write(resource, xmlFile)
  }
  it should "successfully read the same data" in {
    val r2 = WNLMF.read(xmlFile)
    testCoverage(r2)
  }


  val jsonFile = File.createTempFile("lexicon", ".json")
  jsonFile.deleteOnExit()
  "JSON reader" should "write the coverage model" in {
    WNJSON.write(resource, jsonFile)
  }
  it should "successfully read the same data" in {
    val r2 = WNJSON.read(jsonFile)
    testCoverage(r2)
  }

  val rdfFile = File.createTempFile("lexicon", ".rdf")
  rdfFile.deleteOnExit()
  "RDF reader" should "write the coverage model" in {
    WNRDF.write(resource, rdfFile)
  }
  it should "successfully read the same data" in {
    val r2 = WNRDF.read(rdfFile)
    testCoverage(r2)
  }



}
