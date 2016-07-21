package org.globalwordnet.api.serialize

import org.scalatest._
import eu.monnetproject.lang.Language
import org.globalwordnet.api.wn._

class PLWNTest extends FlatSpec with Matchers {
  "plWordNet Reader" should "read the test file" in {
    val config = PLWordNetConfig()
    val resource = plWordNetReader.read(new java.io.File("src/test/resources/plwn-test.xml"),
      config, LexicalResource(Seq(Lexicon(
        id="pwn",
        label="Princeton WordNet",
        language=Language.ENGLISH,
        email="",
        license="",
        version=""))))
    val expectedValue = LexicalResource(Seq(
      Lexicon(
        id="enWordNet",
        label="enWordNet",
        language=Language.ENGLISH,
        email=config.email,
        license=config.license,
        version=config.version,
        url=config.url,
        citation=config.citation,
        entries=Seq(
          LexicalEntry(
            id=".22",
            lemma=Lemma(
              writtenForm=".22",
              partOfSpeech=noun),
            senses=Seq(
              Sense(
                id="478387",
                synsetRef="pl-309949",
                senseRelations=Seq())))),
        synsets=Seq(
          Synset(
            id="pl-309949",
            ili=Some("in"),
            iliDefinition=Some(ILIDefinition("a .22 caliber firearm (pistol or rifle)  ")),
            definitions=Seq(Definition("a .22 caliber firearm (pistol or rifle)  ")),
            synsetRelations=Seq(
              SynsetRelation(
                target="pl-302999",
                relType=hypernym)
            )))
        ), Lexicon(
          id="plWordNet",
          label="plWordNet",
          language=Language.POLISH,
          email=config.email,
          license=config.license,
          version=config.version,
          url=config.url,
          citation=config.citation,
          entries=Seq(
            LexicalEntry(
              id="dehermetyzacja",
              lemma=Lemma(
                writtenForm="dehermetyzacja",
                partOfSpeech=noun),
              senses=Seq(
                Sense(
                  id="107360",
                  synsetRef="pl-77760",
                  senseRelations=Seq(
                    SenseRelation(
                      target="61999",
                      relType=other("proper_antonym")
                    )
                  )
                )
              )
            )
          ),
          synsets=Seq(
            Synset(
              id="pl-77760"))
        )
      ))
    resource.toString should be (expectedValue.toString)
  }
}
