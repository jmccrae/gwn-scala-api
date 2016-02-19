package org.globalwordnet.api.seralize

import scala.io.Source
import java.io.File
import org.apache.commons.lang3.StringEscapeUtils.{escapeXml10 => escapeXml, escapeJava}
import org.globalwordnet.api.wn._

case class WNDBProperties(
  iliRef : String,
  id : String,
  label : String,
  language : String,
  email : String,
  license : String,
  version : String,
  url : Option[String],
  citation : Option[String])

object WNDB {

  def read(wnFolder : File, props : WNDBProperties) : Map[String, Lexicon] = {
    val iliRef = props.iliRef
    
    def words(pos : String) : Iterator[WordNetDataItem] = if(new File(wnFolder+"data."+pos).exists) {
      for(line <- Source.fromFile(wnFolder+"data."+pos).getLines() if !(line matches "\\s+.*")) yield {
        WordNetDataItem.fromString(line)
      }
    } else {
      Nil.iterator
    }
    
    val lexNames = loadLexname(wnFolder + "lexnames")
    val exc = Map(
      "v" -> excForms(wnFolder + "verb.exc"),
      "n" -> excForms(wnFolder + "noun.exc"),
      "a" -> excForms(wnFolder + "adj.exc"),
      "r" -> excForms(wnFolder + "adv.exc"))
    
    val sentences = loadSentences(wnFolder + "sents.vrb")
    
    val iliMap = loadILIMap(iliRef)
    
    val items = words("verb").toSeq ++ words("noun").toSeq ++ words("adj").toSeq ++ words("adv").toSeq

    val satellites = buildSatellites(items)
    val counts = loadCounts(wnFolder + "cntlist")
    
    lexNames.map({
      case (lexId, lexName) =>
        lexName -> buildLMF(items, props, sentences, iliMap, lexId, exc, satellites, counts)
    })
  }
  
  def loadLexname(file : String) = io.Source.fromFile(file).getLines.map({ line =>
    val l = line.split("\\s+")
    l(0).toInt -> l(1)
  }).toMap

  def excForms(file : String) : Map[String,Seq[String]] = {
    (for(line <- Source.fromFile(file).getLines()) yield {
      line.split(" ") match {
        case Array(variant,lemma) => Some(lemma -> variant)
        case _ => None
      }
    }).flatten.toSeq.groupBy(_._1).mapValues(_.map(_._2))
  }

  def buildSatellites(items : Seq[WordNetDataItem]) = {
    val it2 = items.filter(_.pos == adjective)
    (for(item <- it2) yield {
      item.offset -> (item.lemmas(0).lemma, item.lexNo)
    }).toMap
  }

  def loadCounts(file : String) = io.Source.fromFile(file).getLines.map({ line =>
    val l = line.split("\\s+")
    l(1) -> l(0).toInt
  }).toMap


  def buildLMF(items : Seq[WordNetDataItem], 
    props : WNDBProperties, sentences : Map[Int, String],
    ili : Map[(Int, String), String], lex : Int,
    exc : Map[String, Map[String, Seq[String]]],
    satellites : Map[Int, (String, Int)],
    counts : Map[String, Int]) : Lexicon = {
      Lexicon(buildEntries(items, props.id, sentences, lex, exc).toSeq,
              buildSynsets(items, props.id, props.language, ili, lex),
              props.id,
              props.label,
              props.language,
              props.email,
              props.license,
              props.version,
              props.url,
              props.citation)
  }

  def buildEntries(items : Seq[WordNetDataItem], id : String,
    sentences : Map[Int, String], lex : Int, exc : Map[String, Map[String, Seq[String]]]) = {
    val map = collection.mutable.HashMap[String, Seq[WordNetDataItem]]()
    for(item <- items) {
      for(lemma <- item.lemmas) {
        val lemma_key = lemma.lemma + "-" + item.pos.shortForm
        if(!(map contains lemma_key)) {
          map.put(lemma_key, Seq(item))
        } else{
          map.put(lemma_key, map(lemma_key) :+ item)
        }
      }
    }
    for((lemma_key, items) <- map if items.exists(_.lexNo == lex)) yield {
      val lemma = lemma_key.dropRight(2)
      val pos = items(0).pos 
      LexicalEntry(
          id=id + "-" + escapeJava(lemma_key.replace(" ", "_").replace("'", "-ap-").replace("(","-lb-").replace(")","-rb-").replace("/","-sl-")),
          lemma=Lemma(writtenForm=lemma, partOfSpeech=pos),
          forms=exc.getOrElse(pos.shortForm, Map()).getOrElse(lemma, Nil).map({
            ex =>
              Form(writtenForm=ex)
          }),
          senses=for(WordNetDataItem(offset, lexNo, pos, lemmas, pointers, frames, gloss) <- items) yield {
            val word = lemmas.find(_.lemma == lemma).get
            val srcIdx = word.synNo
            Sense(id="%s-%08d-%s-%d" format (id, offset, pos.shortForm, srcIdx),
             synsetRef="%s-%08d-%s" format (id, offset, pos.shortForm),
             senseRelations={
               for(Pointer(typ, targetOffset, pos, src, trg) <- pointers 
                   if srcIdx == src) yield {
                 SenseRelation(target="%s-%08d-%s-%d" format (id, targetOffset, pos.shortForm, trg), relType=typ.asInstanceOf[SenseRelType])
               }
             },
             senseExamples=Nil)
          },
          syntacticBehaviours={
            val frames = items.flatMap(_.frames).toSet
            (for(frame <- frames) yield {
              SyntacticBehaviour(subcategorizationFrame=sentences.getOrElse(frame.frameId, "ERROR"))
            }).toSeq
          }
        )
     }
  }

  def buildSynsets(items : Seq[WordNetDataItem], id : String, language : String,
      ili : Map[(Int, String), String], lex : Int) = {
    for(WordNetDataItem(offset, lexNo, pos, lemmas, pointers, frames, gloss) <- items if lexNo == lex) yield {
      val iliId = ili.get((offset, pos.shortForm)) match {
        case Some(id) =>
          id
        case None =>
//          System.err.println("new,,,%08d-%s,%s" format (offset, pos.shortForm, lemmas.map(_.lemma).mkString("/")))
          "in"
      }
      Synset(id="%s-%08d-%s" format (id, offset, pos.shortForm),
             ili="${iliId}",
             definitions=Seq(Definition(gloss)),
             iliDefinition={
               if(iliId == "in") {
                 if(gloss.length < 20 && gloss.split(" ").length < 5) {
                   System.err.println("Too short: " + gloss)
                 }
                 Some(ILIDefinition(gloss))
               } else { 
                 None
               }
             },
             synsetRelations={
               for(Pointer(typ, targetOffset, pos, src, trg) <- pointers 
                   if src == 0 && trg == 0) yield {
                 SynsetRelation(target="%s-%08d-%s" format (id, targetOffset, pos.shortForm), relType=typ.asInstanceOf[SynsetRelType])
              }
            })
        }
    }

  def loadSentences(fileName : String) : Map[Int, String] = {
    (io.Source.fromFile(fileName).getLines.map { line =>
      val (id, sentence) = line.splitAt(line.indexOf(" "))
      id.toInt -> sentence.drop(1)
    }).toMap
  }

  val iliIdType1 = "ili:(i\\d+)".r
  val iliIdType2 = "<(i\\d+)>".r

  def loadILIMap(fileName : String) : Map[(Int, String), String] = {
    (io.Source.fromFile(fileName).getLines.filter(_.contains("owl:sameAs")).flatMap { line =>
      val elems = line.split("\\s+")
      val ili = elems(0) match {
        case iliIdType1(id) => id
        case iliIdType2(id) => id
      }
      val offset = elems(2).drop(7).dropRight(2).toInt
      val pos = elems(2).takeRight(1)
      if(elems(1) == "owl:sameAs") {
        if(pos == "s") {
          Some((offset, "a") -> ili)
        } else {
          Some((offset, pos) -> ili)
        }
      } else {
        None
      }
    }).toMap
  }

  def dumpDefinitions(items : Seq[WordNetDataItem]) {
    val out = new java.io.PrintWriter("defs-wn30.csv")
    for(WordNetDataItem(offset, _, pos, _, _, _, gloss) <- items) {
      out.println("%s,%08d,%s" format (gloss.replaceAll(",",""), offset, pos.shortForm))
    }
    out.flush
    out.close
  }

  object POS {
    def mk(s : String) = s match {
      case "n" => noun
      case "v" => verb
      case "a" => adjective
      case "s" => adjective_satellite
      case "r" => adverb
    }
    def code(pos : PartOfSpeech) = {
      if(pos == noun) {"1"}
      else if(pos == verb) {"2"}
      else if(pos == adjective) {"3"}
      else if(pos == adverb) {"4"}
      else if(pos == adjective_satellite) {"5"}
      else { throw new RuntimeException() }
    }
  }
    
  case class WordNetDataItem(val offset : Int, val lexNo : Int,
       val pos : PartOfSpeech, val lemmas : Seq[Word],
       val pointers : Seq[Pointer], val frames : Seq[Frame],
       val gloss : String) {
  }
       
  object WordNetDataItem {
    def fromString(s : String) = s.split("\\| ") match {
      case Array(data,gloss) => {
        readData(data.split(" ")) match {
          case (o,l,p,ls,ps,fs) => WordNetDataItem(o,l,p,ls,ps,fs,gloss.trim())
        }
      }
      case Array(data) => {
        readData(data.split(" ")) match {
          case (o,l,p,ls,ps,fs) => WordNetDataItem(o,l,p,ls,ps,fs,"")
        }
      }
      case _ => { throw new RuntimeException("Did not understand: " + s) }
    }
    
    private def readData(d : Seq[String]) = {
       val w_cnt = Integer.parseInt(d(3),16)
       val w_end = 4 + w_cnt * 2
       val p_cnt = d(w_end).toInt
       val p_end = w_end + 1 + 4 * p_cnt
       val f_cnt = if(p_end < d.size) {
         d(p_end).toInt
       } else {
         0
       }
       val f_end = p_end + 1 + 3 * f_cnt
       val pos = POS.mk(d(2))
       val lexNo = d(1).toInt
       val ptrs = readPointers(d.slice(w_end + 1, p_end),d(2)).toSeq 
       val head = if(pos == adjective_satellite) {
         getHead(ptrs)
       } else {
         None
       }
       val rval = (d(0).toInt, lexNo,pos,
          readWords(d.slice(4,w_end),lexNo,pos,head).toSeq,
          ptrs, 
          if(p_end < d.size) { readFrames(d.slice(p_end + 1, f_end)).toSeq } else { Nil })
       rval
    }
    
    private def getHead(ptrs : Seq[Pointer]) : Option[Int] = {
      ptrs.find(_.typ == fuzzynym) match {
        case Some(Pointer(_,offset,_,_,_)) => Some(offset)
        case _ => None
      }
    }

    private def readWords(d : Seq[String], lexNo : Int, pos : PartOfSpeech, head : Option[Int]) = {
        for((word,idx) <- d.grouped(2).zipWithIndex) yield {
          Word(word(0).replaceAll("_"," "),Integer.parseInt(word(1),16),idx+1,lexNo,pos,head) 
        }
    }
    
    private def readPointers(d : Seq[String], pos : String) = {
      for(ptr <- d.grouped(4)) yield {
        Pointer(PointerType.mk(ptr(0),pos), ptr(1).toInt,
          POS.mk(ptr(2)), (Integer.parseInt(ptr(3),16) & 0xff00) >> 8, Integer.parseInt(ptr(3),16) & 0x00ff)
      }
    }
    
    private def readFrames(d : Seq[String]) = {
      for(f <- d.grouped(3)) yield {
        Frame(f(1).toInt,Integer.parseInt(f(2),16))
      }
    }
  }

  case class Word(val lemma : String, val lexId : Int, val synNo : Int, lexNo : Int, pos : PartOfSpeech, head : Option[Int]) {
    def senseIdx(sattelites : Map[Int,(String,Int)]) = {
      if(head != None) {
        "%s-%s#%s:%02d:%02d:%s:%02d" format (lemma.replaceAll(" ","_"), pos.shortForm, POS.code(pos), lexNo, lexId, sattelites(head.get)._1,
        sattelites(head.get)._2)
      } else {
        "%s-%s#%s:%02d:%02d::" format (lemma.replaceAll(" ","_"), pos.shortForm, POS.code(pos), lexNo, lexId)
      }
    }
  }

  case class Pointer(val typ : RelType, val targetOffset : Int,
       val pos : PartOfSpeech, val src : Int, val trg : Int)
       
  case class Frame(val frameId : Int, val wordId : Int)

  object PointerType {
    var nonStandardRels = Set[String]()
    def mk(s : String, pos : String) : RelType = s match {
      case "!" => antonym 
      case "@" => hypernym 
      case "@i" => instance_hypernym
      case "~" => hyponym 
      case "~i" => instance_hyponym
      case "#m" => holo_member
      case "#s" => holo_substance
      case "#p" => holo_part
      case "%m" => mero_member
      case "%s" => mero_substance
      case "%p" => mero_part 
      case "=" => attribute 
      case "+" => derivation
      case ";c" => domain_topic
      case "-c" => has_domain_topic
      case ";r" => domain_region
      case "-r" => has_domain_region
      case ";u" => domain_usage
      case "-u" => has_domain_usage
      case "*" =>  entails 
      case ">" => causes
      case "^" => also
      case "$" => fuzzynym 
      case "&" => fuzzynym 
      case "<" => participle 
      case "\\" => if(pos == "a" || pos == "s" || pos == "n") {
          pertainym
      } else if(pos == "r") {
          derivation
      } else throw new IllegalArgumentException("pos="+pos)
      case _ => {
        throw new IllegalArgumentException("pos="+pos)
      }
    }
  }
}
