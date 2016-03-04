package org.globalwordnet.api

import java.io.File
import wn._

/**
 * A generic reader/writer
 */
trait Format {
  def read(file : File) : LexicalResource
  def write(lr : LexicalResource, file : File) : Unit
}

package object util {
  private lazy val digestor = java.security.MessageDigest.getInstance("SHA-256")
  private lazy val encoder  = java.util.Base64.getEncoder()
  /* Generate a highly collision-resistant identifier from a string */
  def makeId(key : String) : String= {
    encoder.encode(digestor.digest(key.getBytes()).take(9)).
      map(_.toChar).mkString.replaceAll("\\+", "_").replaceAll("/", ":")
  }

  def merge(lr1 : LexicalResource, lr2 : LexicalResource) = Merge.merge(lr1, lr2)
}
