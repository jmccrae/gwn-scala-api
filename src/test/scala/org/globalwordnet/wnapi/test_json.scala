package org.globalwordnet.api.serialize

import eu.monnetproject.lang.{Language, Script}
import org.globalwordnet.api.wn._
import org.scalatest._

class JsonSpec extends FlatSpec with Matchers {
  var resource : LexicalResource = null
  "Json reader" should "successfully read a JSON file" in {
    resource = WNJSON.read(new java.io.FileReader("src/test/resources/example.json"))
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
    resource.lexicons(0).entries.size should be (3)
  }
  it should "produce 2 synset" in {
    resource.lexicons(0).synsets.size should be (2)
  }
  it should "produce an entry for grandfather" in {
    val e = resource.lexicons(0).entries(0)
    e.id should be ("w1")
    e.lemma.writtenForm should be ("grandfather")
    e.lemma.partOfSpeech should be (noun)
    e.senses(0).id should be ("example-10161911-n-1")
    e.senses(0).synsetRef should be ("example-10161911-n")
  }
  it should "produce a sense" in {
    val s = resource.lexicons(0).entries(1).senses(0)
    s.id should be ("example-1-n-1")
    s.synsetRef should be ("example-1-n")
  }
  it should "produce a sense relation" in {
    val r = resource.lexicons(0).entries(1).senses(0).senseRelations(0)
    r.relType should be (derivation)
    r.target should be ("example-10161911-n-1")
  }
  it should "produce syntactic behavior" in {
    resource.lexicons(0).entries(2).syntacticBehaviours.size should be (3)
    val s = resource.lexicons(0).entries(2).syntacticBehaviours(0)
    s.subcategorizationFrame should be ("Sam cannot %s Sue")
  }
  it should "produce a synset" in {
    val s = resource.lexicons(0).synsets(0)
    s.id should be ("example-10161911-n")
    s.ili should be (Some("i90287"))
    s.definitions(0).content should be ("the father of your father or mother")
    s.synsetRelations(0).relType should be (hypernym)
    s.synsetRelations(0).target should be ("example-10162692-n")
  }
  val f = java.io.File.createTempFile("lexicon", ".json")
  f.deleteOnExit()
  "JSON Writer" should "write a file" in {
    WNJSON.write(resource, f)
  }
  it should "roundtrip" in {
    val text1 = io.Source.fromFile("src/test/resources/example.json").getLines.mkString(" ").replaceAll("\\s+", "")
    val text2 = io.Source.fromFile(f).getLines.mkString("\n")
    text2.replaceAll("\\s+", "").length should be (text1.length - 40)
    //val f1 = text1.groupBy(x => x).mapValues(_.size)
    //val f2 = text2.groupBy(x => x).mapValues(_.size)
    //for(k <- f1.keys) {
    //  if(f1(k) - f2.getOrElse(k, 0) != 0) {
    //    println("%s\t%d" format(k, f1(k) - f2.getOrElse(k, 0)))
    //  }
    //}
  }

}
 
