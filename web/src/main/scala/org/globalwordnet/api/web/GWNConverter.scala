package org.globalwordnet.api.web

import org.scalatra._

class GWNConverter extends ScalatraServlet {

  get("/") {
    views.html.index()
  }

}
