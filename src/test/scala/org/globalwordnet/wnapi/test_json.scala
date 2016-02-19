package org.globalwordnet.api.serialize

import org.globalwordnet.api.wn._
import org.scalatest._

class JsonSpec extends FlatSpec with Matchers {
  var resource : LexicalResource = null
  "Json reader" should "successfully read an XML file" in {
    resource = WNJSON.read(new java.io.FileReader("src/test/resources/example.json"))
  }
}
 
