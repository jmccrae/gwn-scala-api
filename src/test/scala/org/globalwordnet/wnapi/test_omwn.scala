package org.globalwordnet.api.serialize

import eu.monnetproject.lang.Language
import java.io.File
import org.globalwordnet.api.wn._
import org.scalatest._

class OMWNSpec extends FlatSpec with Matchers {
  "OMWN" should "read a file" in {
    val wn30 = new WNLMF(false).read(new File("src/test/resources/example.xml"))
    val resource = OpenMultilingualWordNet.read(
      new File("src/test/resources/test.tsv"), wn30, "gle", "")

    resource.lexicons should have size 5
    resource.lexicons.find(_.language == Language.DANISH).get.entries should have size 1
    resource.lexicons.find(_.language == Language.DANISH).get.entries(0).lemma should be (Lemma("farfar", noun, None))
    resource.lexicons.find(_.language == Language.IRISH).get.entries should have size 1
    resource.lexicons.find(_.language == Language.IRISH).get.entries(0).lemma should be (Lemma("seanathair", noun, None))
    resource.lexicons.find(_.language == Language.ENGLISH).get.synsets.flatMap(_.definitions) should contain (Definition("faren til en persons far", Some(Language.NORWEGIAN_BOKMAL)))
  }
}
