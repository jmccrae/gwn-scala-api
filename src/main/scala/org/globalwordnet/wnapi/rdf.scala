package org.globalwordnet.api.serialize

import java.io.{File, FileReader, Reader}
import org.globalwordnet.api.wn._
import org.apache.jena.rdf.model.{Model, ModelFactory, Resource, Property, RDFNode}
import org.apache.jena.vocabulary.RDF
import scala.collection.JavaConversions._
import scala.language.dynamics

object WNRDF {
  class NameSpace(prefix : String) extends Dynamic {
    def selectDynamic(suffix : String)(implicit model : Model) = model.createResource(prefix + suffix)
  }

  private implicit class PimpedResource(resource : Resource) {
    def \(p : Property)(implicit model : Model) : Iterator[RDFNode] = {
      model.listObjectsOfProperty(resource, p)
    }
    def \(r : Resource)(implicit model : Model) : Iterator[RDFNode] = {
      model.listObjectsOfProperty(resource, model.createProperty(r.getURI()))
    }
    def \*(r : Property)(implicit model : Model) : Iterator[Resource] = {
      model.listObjectsOfProperty(resource, r).flatMap({
        case r : Resource => Some(r)
        case _ => None
      })
    }
    def \*(r : Resource)(implicit model : Model) : Iterator[Resource] = {
      model.listObjectsOfProperty(resource, model.createProperty(r.getURI())).flatMap({
        case r : Resource => Some(r)
        case _ => None
      })
    }
  }


  val ONTOLEX = new NameSpace("http://www.w3.org/ns/lemon/ontolex#")
  val LIME = new NameSpace("http://www.w3.org/ns/lemon/lime#")

  def read(file : File) : LexicalResource = {
    if(file.getName().endsWith(".rdf") || file.getName().endsWith(".xml")) {
      read(new FileReader(file), "RDF/XML", "file://" + file.getAbsolutePath())
    } else if(file.getName().endsWith(".ttl")) {
      read(new FileReader(file), "TURTLE", "file://" + file.getAbsolutePath())
    } else if(file.getName().endsWith(".nt")) {
      read(new FileReader(file), "N-TRIPLE", "file://" + file.getAbsolutePath())
    } else if(file.getName().endsWith(".n3")) {
      read(new FileReader(file), "N3", "file://" + file.getAbsolutePath())
    } else {
      read(new FileReader(file), "RDF/XML", "file://" + file.getAbsolutePath())
    }
  }

  def read(input : Reader, lang : String, baseUrl : String) : LexicalResource = {
    implicit val model = ModelFactory.createDefaultModel()
    model.read(input, baseUrl, lang)
    readLexicalResource
  }

  def readLexicalResource(implicit model : Model) : LexicalResource = {
    LexicalResource(model.listSubjectsWithProperty(RDF.`type`, ONTOLEX.Lexicon).map(readLexicon).toSeq)
  }

  def readLexicon(r : Resource)(implicit model : Model) : Lexicon = {
    readMeta(Lexicon(
      (r \* LIME.entry).map(readLexicalEntry).toSeq,
      Nil,
      "",
      "",
      "",
      "",
      "",
      "",
      None,
      None), r)
  }

  def readMeta[A <: Meta](a : A, r : Resource)(implicit model : Model) : A = a

  def readLexicalEntry(r : Resource)(implicit model : Model) : LexicalEntry = {
    null
  }
}
