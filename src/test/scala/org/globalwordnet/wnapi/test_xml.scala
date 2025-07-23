package org.globalwordnet.api.serialize

import eu.monnetproject.lang.{Language, Script}
import org.globalwordnet.api.wn._
import org.scalatest._

class XMLSpec extends FlatSpec with Matchers {
  var resource : LexicalResource = null
  "XML reader" should "successfully read an XML file" in {
    resource = new WNLMF(false).read(new java.io.FileReader("src/test/resources/example.xml"))
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
  it should "produce 3 synset" in {
    resource.lexicons(0).synsets.size should be (3)
  }
  it should "produce an entry for grandfather" in {
    val e = resource.lexicons(0).entries(0)
    e.id should be ("w1")
    e.lemma.writtenForm should be ("grandfather")
    e.lemma.partOfSpeech should be (noun)
    e.senses(0).id should be ("example-en-10161911-n-1")
    e.senses(0).synsetRef should be ("example-en-10161911-n")
  }
  it should "produce a sense" in {
    val s = resource.lexicons(0).entries(1).senses(0)
    s.id should be ("example-en-1-n-1")
    s.synsetRef should be ("example-en-1-n")
  }
  it should "produce a sense relation" in {
    val r = resource.lexicons(0).entries(1).senses(0).senseRelations(0)
    r.relType should be (derivation)
    r.target should be ("example-en-10161911-n-1")
  }
  it should "produce syntactic behavior" in {
    resource.lexicons(0).entries(2).syntacticBehaviours.size should be (3)
    val s = resource.lexicons(0).entries(2).syntacticBehaviours(0)
    s.subcategorizationFrame should be ("Sam cannot %s Sue ")
  }
  it should "produce a synset" in {
    val s = resource.lexicons(0).synsets(0)
    s.id should be ("example-en-10161911-n")
    s.ili should be (Some("i90287"))
    s.definitions(0).content should be ("the father of your father or mother")
    s.synsetRelations(0).relType should be (hypernym)
    s.synsetRelations(0).target should be ("example-en-10162692-n")
  }
  val f = java.io.File.createTempFile("lexicon", ".xml")
  f.deleteOnExit()
  "XML Writer" should "write a file" in {
    new WNLMF(false).write(resource, f)
  }
  it should "roundtrip" in {
    val text1 = io.Source.fromFile("src/test/resources/example.xml").getLines().mkString(" ").replaceAll("<!--[^!]*-->", "").replaceAll("\\s+", " ").replaceAll(" >", ">").replaceAll(" />", "/>")
    val text2 = io.Source.fromFile(f).getLines().mkString(" ").replaceAll("\\s+", " ").replaceAll(" >", ">").replaceAll(" />", "/>")
    text2 should be (text1)
  }
  lazy val resource2: LexicalResource = new WNLMF(false).read(new java.io.FileReader("src/test/resources/example-ext.xml"))
  "XML reader" should "successfully read an XML file with extensions" in {
    resource2 // Accessing the lazy val to ensure it is initialized
  }

  it should "have a non confidence score for the extension" in {
    resource2.lexiconExtensions(0).confidenceScore should be (None)
  }

  val f2 = java.io.File.createTempFile("lexicon-ext", ".xml")
  f2.deleteOnExit()
  "XML Writer with extensions" should "write a file" in {
    new WNLMF(false).write(resource2, f2)
  }
  it should "roundtrip with extensions" in {
    val text1 = io.Source.fromFile("src/test/resources/example-ext.xml").getLines().mkString(" ").replaceAll("<!--[^!]*-->", "").replaceAll("\\s+", " ").replaceAll(" >", ">").replaceAll(" />", "/>")
    val text2 = io.Source.fromFile(f2).getLines().mkString(" ").replaceAll("\\s+", " ").replaceAll(" >", ">").replaceAll(" />", "/>")
    text2 should be (text1)
  }

  it should "read the space example and preserve space" in {
    val spaceResource = new WNLMF(false).read(new java.io.FileReader("src/test/resources/example-space.xml"))
    spaceResource.lexicons.size should be (1)
    spaceResource.lexicons(0).entries.size should be (1)
    spaceResource.lexicons(0).entries(0).senses.size should be (1)
    spaceResource.lexicons(0).entries(0).definitions(0).content should be ("This is a definition where space will not be preserved")
    spaceResource.lexicons(0).synsets(0).definitions(1).content should be ("  This  is  a  definition  where  space  will  be  preserved  ")
  }

}
