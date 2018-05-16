package org.globalwordnet.api.web

import org.scalatra.test.scalatest._

class GWNConverterTests extends ScalatraFunSuite {

  addServlet(classOf[GWNConverter], "/*")

  test("GET / on GWNConverter should return status 200") {
    get("/") {
      status should equal (200)
    }
  }

}
