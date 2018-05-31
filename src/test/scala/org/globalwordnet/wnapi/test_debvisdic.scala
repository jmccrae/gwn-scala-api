package org.globalwordnet.api.serialize

import eu.monnetproject.lang.{Language, Script}
import org.globalwordnet.api.wn._
import org.scalatest._
import java.io.File

class DebVisDicSpec extends FlatSpec with Matchers {
  var resource : LexicalResource = null
  val dvdReader = new DebVisDic("example-en", "label", Language.ENGLISH,
    "e@ma.il", "CC-Zero", "0.0", None, None, None) {
      override lazy val iliMap = Some(Map((1740,"n")->"i1"))
    }
  "DebVisDic reader" should "successfully read a DebVisDic file" in {
    resource = dvdReader.read(new java.io.FileReader("src/test/resources/wnven.xml"))
  }

  it should "produce 1 lexicon" in {
    resource.lexicons.size should be (1)
  }

  it should "contain entry poidza" in {
    resource.lexicons(0).entries.filter(_.lemma.writtenForm == "poidza") should be (
      Seq(
        LexicalEntry(
          id="example-en-poidza-n", 
          lemma=Lemma("poidza", noun),
          senses=Seq(Sense(
            id="example-en-ENG20-06455674-n-2-poidza",
            synsetRef="example-en-ENG20-06455674-n"))),
        LexicalEntry(
          id="example-en-poidza-v", 
          lemma=Lemma("poidza", org.globalwordnet.api.wn.verb),
          senses=Seq(Sense(
            id="example-en-ENG20-00008198-v-1-poidza",
            synsetRef="example-en-ENG20-00008198-v")))
       
      ))
  }

  it should "contain synset ENG20-00017381-a" in {
    resource.lexicons(0).synsets.filter(_.id == "example-en-ENG20-00017381-a") should be (
      Seq(
        Synset(
          id="example-en-ENG20-00017381-a",
          ili=Some("i1"),
          synsetRelations=Seq(SynsetRelation("example-en-ENG20-00018100-v", hypernym)),
          partOfSpeech=Some(adjective),
          definitions=Nil,
          synsetExamples=Seq(Example("u rwa nga mulaṱela"))) withNote "DOMAIN=factotum, SUMO=+Impelling"))
  }

  it should "roundtrip" in {
    val _model1 = new WNLMF(false).read(new java.io.FileReader("src/test/resources/example.xml"))
    val model1 = LexicalResource(Seq(_model1.lexicons(0)))
    val file1 = File.createTempFile("debvisdic", ".xml")
    file1.deleteOnExit()
    val file2 = File.createTempFile("debvisdic", ".xml")
    file2.deleteOnExit()
    dvdReader.write(model1, file1)
    val model2 = dvdReader.read(file1)
    dvdReader.write(model2, file2)
    val data1 = io.Source.fromFile(file1).getLines.mkString("\n")
    val data2 = io.Source.fromFile(file2).getLines.mkString("\n")
    data1 should be (data2)
  }




}
