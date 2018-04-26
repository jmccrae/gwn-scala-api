package org.globalwordnet.api
import org.globalwordnet.api.wn._

object MultiMap {
  import scala.language.higherKinds
  implicit class MultiMapFromTraversable[A, B, T[X] <: scala.collection.TraversableLike[X,T[X]]](t : T[(A, B)]) {
    def toMultiMap(implicit cbf: scala.collection.generic.CanBuildFrom[T[A],B,T[B]], cbf2 : scala.collection.generic.CanBuildFrom[T[(A,B)], B, T[B]]) : Map[A, T[B]] = {
      t.groupBy(_._1).mapValues(_.map(_._2)) } }

  implicit class MultiMapFromIterator[A, B](t : Iterator[(A,B)]) {
    def toMultiMap : Map[A, Set[B]] = {
      t.toSeq.groupBy(_._1).mapValues(_.map(_._2).toSet) } }
}

/** Split a resource by subject and name each subject */
object BySubject {
  import MultiMap._
  def splitBySubject(resource : LexicalResource) : Seq[(String, LexicalResource)] = {
    var lexicons = collection.mutable.ListBuffer[(String,Lexicon)]()
    for(lexicon <- resource.lexicons) {
      val synsetsBySubject = lexicon.synsets.map({ss =>
          ss.subject.getOrElse("") -> ss
      }).toMultiMap
      val entriesBySynset : Map[String, Seq[Int]] = lexicon.entries.zipWithIndex.flatMap({ case(e, id) =>
        e.senses.map({ s =>
          s.synsetRef -> id
        })
      }).toMultiMap
      for((subj,synsets) <- synsetsBySubject) {
        val synsetIds = synsets.map(_.id).toSet
        lexicons += ((subj,
          lexicon.copy(
            entries=synsets.flatMap({ ss =>
              entriesBySynset.getOrElse(ss.id, Nil).map({i => 
                mapEntry(lexicon.entries(i), synsetIds)
              })
            }),
            synsets=synsets)))
      }
    }
    lexicons.groupBy(_._1).values.map(s => 
        s(0)._1 -> LexicalResource(s.map(_._2))).toSeq
  }

  private def mapEntry(entry : LexicalEntry, ids : Set[String]) : LexicalEntry = {
    entry.copy(
      senses=entry.senses.filter(s => ids.contains(s.synsetRef)))
  }
}

