package org.globalwordnet.api.serialize

import eu.monnetproject.lang.{Language, Script}
import org.globalwordnet.api.wn._
import org.scalatest._
import java.io.File

class DebVisDicSpec extends FlatSpec with Matchers {
  var resource : LexicalResource = null
  val dvdReader = new DebVisDic("id", "label", Language.ENGLISH,
    "e@ma.il", "CC-Zero", "0.0")
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
          id="id-poidza-n", 
          lemma=Lemma("poidza", noun),
          senses=Seq(Sense(
            id="ENG20-06455674-n-2",
            synsetRef="ENG20-06455674-n"))),
        LexicalEntry(
          id="id-poidza-v", 
          lemma=Lemma("poidza", org.globalwordnet.api.wn.verb),
          senses=Seq(Sense(
            id="ENG20-00008198-v-1",
            synsetRef="ENG20-00008198-v")))
       
      ))
  }

  it should "contain synset ENG20-00017381-a" in {
    resource.lexicons(0).synsets.filter(_.id == "ENG20-00017381-a") should be (
      Seq(
        Synset(
          id="ENG20-00017381-a",
          partOfSpeech=Some(adjective),
          definitions=Nil,
          synsetExamples=Seq(Example("u rwa nga mulaṱela"))) withNote "DOMAIN=factotum, SUMO=+Impelling"))
  }

  it should "roundtrip" in {
    val _model1 = WNLMF.read(new java.io.FileReader("src/test/resources/example.xml"))
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
