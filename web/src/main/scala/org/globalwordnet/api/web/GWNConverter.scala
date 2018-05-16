package org.globalwordnet.api.web

import org.scalatra._
import org.scalatra.servlet.{FileUploadSupport, MultipartConfig, SizeConstraintExceededException, FileItem}
import org.globalwordnet.api.serialize._
import scala.util.Try
import java.io.Reader

class GWNConverter extends ScalatraServlet with FileUploadSupport {
  configureMultipartHandling(MultipartConfig(maxFileSize = Some(3*1024*1024)))
  
  def processFile(item : FileItem) : Reader = {
    new java.io.InputStreamReader(item.getInputStream)
  }

  post("/validate") {
    val input = params("inputText") match {
      case "" => processFile(fileParams("inputFile"))
      case t => new java.io.StringReader(t)
    }
    params("inputFormat") match {
      case "lmf" =>
        Try(WNLMF.read(input))
      case "json" =>
        Try(WNJSON.read(input))
      case "rdf" =>
        Try(WNRDF.read(input, params("inputRdfLang"), ""))
    }
  }

  get("/") {
    views.html.index()
  }

}
