package org.globalwordnet.api.serialize

import org.globalwordnet.api.Format
import eu.monnetproject.lang.{Language, Script}
import scala.io.Source
import java.io.File
import java.io.PrintWriter
import org.apache.commons.lang3.StringEscapeUtils.{escapeXml10 => escapeXml, escapeJava}
import org.globalwordnet.api.wn._

class WNDB(
  iliRef : File,
  id : String,
  label : String,
  language : Language,
  email : String,
  license : String,
  version : String,
  url : Option[String],
  citation : Option[String],
  usePrincetonHeader : Boolean = true,
  filterFile : Option[File] = None) extends Format {

  var lexNames : Map[Int, String] = null
  def read(wnFolder : File) : LexicalResource = {
    
    def words(pos : String) : Iterator[WordNetDataItem] = if(new File(wnFolder,"data."+pos).exists) {
      for(line <- Source.fromFile(new File(wnFolder,"data."+pos)).getLines() if !(line matches "\\s+.*")) yield {
        WordNetDataItem.fromString(line)
      }
    } else {
      throw new RuntimeException("data." + pos + " does not exist")
    }
    
    lexNames = loadLexname(new File(wnFolder, "lexnames"))
    val exc = Map(
      "v" -> excForms(new File(wnFolder, "verb.exc")),
      "n" -> excForms(new File(wnFolder, "noun.exc")),
      "a" -> excForms(new File(wnFolder, "adj.exc")),
      "r" -> excForms(new File(wnFolder, "adv.exc")))
    
    val sentences = loadSentences(new File(wnFolder, "sents.vrb"))

    val filter = filterFile.map(loadFilter)
    
    val iliMap = loadILIMap(iliRef)
    
    val items = words("verb").toSeq ++ words("noun").toSeq ++ words("adj").toSeq ++ words("adv").toSeq

    val satellites = buildSatellites(items)
    val counts = loadCounts(new File(wnFolder, "cntlist"))

    val lemmas = buildLemmas(items)

    val posMap = buildPosMap(items)
    
    LexicalResource(Seq(
        buildLMF(items,  sentences, iliMap, exc, satellites, counts, filter, lemmas, posMap)))
  }
  
  private final val filterLine = "([nvar]) \\[(.*)%(.*)\\] \\[(.*)\\]( .*)?".r
  private def loadFilter(file : File) = io.Source.fromFile(file).getLines.map({
    case filterLine(pos, keylemma, sensekey, lemma, defn) => "%s-%s#%s" format (keylemma, pos, sensekey)
  }).toSet


  private def loadLexname(file : File) = io.Source.fromFile(file).getLines.map({ line =>
    val l = line.split("\\s+")
    l(0).toInt -> l(1)
  }).toMap

  private def excForms(file : File) : Map[String,Seq[String]] = {
    (for(line <- Source.fromFile(file).getLines()) yield {
      line.split(" ") match {
        case Array(variant,lemma) => Some(lemma -> variant)
        case _ => None
      }
    }).flatten.toSeq.groupBy(_._1).mapValues(_.map(_._2).toList)
  }

  private def buildSatellites(items : Seq[WordNetDataItem]) = {
    val it2 = items.filter(_.pos == adjective)
    (for(item <- it2) yield {
      item.offset -> (item.lemmas(0).lemma, item.lexNo)
    }).toMap
  }

  private def buildLemmas(items : Seq[WordNetDataItem]) : Map[(Int, PartOfSpeech, Int), String] = {
    items.flatMap({
      case  WordNetDataItem(offset, lexNo, pos, lemmas, pointers, frames, gloss) =>
        lemmas.map({
          case Word(lemma, _, lexNo, _, pos, _) =>
            (offset, pos, lexNo) -> lemma
        })
    }).toMap
  }

  private def buildPosMap(items : Seq[WordNetDataItem]) : Map[(Int, PartOfSpeech), PartOfSpeech] =
    items.map({
      case WordNetDataItem(offset, _, pos, _, _, _, _) =>
        if(pos == adjective_satellite) {
          (offset, adjective) -> pos
        } else {
          (offset, pos) -> pos
        }
    }).toMap


  private def loadCounts(file : File) = io.Source.fromFile(file).getLines.map({ line =>
    val l = line.split("\\s+")
    l(1) -> l(0).toInt
  }).toMap

  private def buildLMF(items : Seq[WordNetDataItem], 
    sentences : Map[Int, String],
    ili : Map[(Int, String), String], 
    exc : Map[String, Map[String, Seq[String]]],
    satellites : Map[Int, (String, Int)],
    counts : Map[String, Int],
    filter : Option[Set[String]],
    lemmas : Map[(Int, PartOfSpeech, Int), String],
    posMap : Map[(Int, PartOfSpeech), PartOfSpeech]) : Lexicon = {
      Lexicon(id,
              label,
              language,
              email,
              license,
              version,
              url,
              citation,
              buildEntries(items, id, sentences, exc, satellites, counts, filter, lemmas, posMap).toSeq,
              buildSynsets(items, id, language, ili, satellites, filter, posMap))
  }

  private def buildEntries(items : Seq[WordNetDataItem], id : String,
    sentences : Map[Int, String], exc : Map[String, Map[String, Seq[String]]],
    satellites : Map[Int, (String, Int)],
    counts : Map[String, Int],
    filter : Option[Set[String]],
    lemmaMap : Map[(Int, PartOfSpeech, Int), String],
    posMap : Map[(Int, PartOfSpeech), PartOfSpeech]) = {
    val inItems : Map[(Int, Int), WordNetDataItem] = filter match {
      case Some(f) =>
        items.flatMap(item => item.lemmas.filter(l => f.contains(l.senseIdx(satellites))).
          map(l => (item.offset, l.synNo) -> item)).toMap
      case None =>
        items.flatMap(item => item.lemmas.map(l => (item.offset, l.synNo) -> item)).toMap
    }
    val map = collection.mutable.HashMap[String, Seq[WordNetDataItem]]()
    for(item <- items) {
      for(lemma <- item.lemmas if filter.map(_.contains(lemma.senseIdx(satellites))).getOrElse(true)) {
        val lemma_key = lemma.lemma + "-" + item.pos.shortForm
        if(!(map contains lemma_key)) {
          map.put(lemma_key, Seq(item))
        } else {
          map.put(lemma_key, map(lemma_key) :+ item)
        }
      }
    }
    for((lemma_key, items) <- map/* if items.exists(_.lexNo == lex)*/) yield {
      val lemma = lemma_key.dropRight(2)
      val pos = items(0).pos 
      val lexEntId = id + "-" + escapeJava(lemma_key.replace(" ", "_").replace("'", "-ap-").replace("(","-lb-").replace(")","-rb-").replace("/","-sl-"))
      LexicalEntry(
          id=lexEntId,
          lemma=Lemma(writtenForm=lemma, partOfSpeech=pos, script=None),
          forms=exc.getOrElse(pos.shortFormNoSatellite, Map()).getOrElse(lemma, Nil).map({
            ex =>
              Form(writtenForm=ex, tag=Nil, script=None)
          }),
          senses=for(WordNetDataItem(offset, lexNo, pos, lemmas, pointers, frames, gloss) <- items) yield {
            val word = lemmas.find(_.lemma == lemma).get
            val srcIdx = word.synNo
            Sense(id="%s-%08d" format (lexEntId, offset),
             synsetRef="%s-%08d-%s" format (id, offset, pos.shortForm),
             senseRelations={
               for(Pointer(typ, targetOffset, pos, src, trg) <- pointers 
                   if srcIdx == src && inItems.contains(targetOffset, trg)) yield {
                     val pos2 = posMap(targetOffset, pos)
                 SenseRelation(
                   target="%s-%s-%s-%08d" format (id, lemmaMap((targetOffset, pos2, trg)).replace(" ", "_").replace("'", "-ap-").replace("(","-lb-").replace(")","-rb-").replace("/","-sl-"), pos2.shortForm, targetOffset),
                   //target="%s-%08d-%s-%d" format (id, targetOffset, pos.shortForm, trg), 
                   relType=typ.asInstanceOf[SenseRelType])
               }
             },
             senseExamples=Nil,
             counts=counts.get(word.senseIdx(satellites)).map(x => Count(x)).toSeq).withIdentifier(word.senseKey(satellites))
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

  private val exampleRegex = "(.*?)((; ['\"](.*?)['\"])*)".r

  private def extractExamples(gloss : String) : (String, Seq[String]) = {
    var exampleRegex(definition, examples,_,_) = gloss
    val exampleList = new collection.mutable.ListBuffer[String]()
    while(examples.indexOf("; ") == 0) {
      val j = examples.indexOf("; ", 1)
      if(j < 0) {
        exampleList += examples.substring(3,examples.length - 1)
        examples = ""
      } else {
        exampleList += examples.substring(3, j - 1)
        examples = examples.substring(j)
      }
    }
    (definition, exampleList.toSeq)
  }

  private def buildSynsets(items : Seq[WordNetDataItem], id : String, language : Language,
      ili : Map[(Int, String), String], satellites : Map[Int, (String, Int)], 
      filter : Option[Set[String]], posMap : Map[(Int, PartOfSpeech), PartOfSpeech]) = {
    val inItems : Map[Int, WordNetDataItem] = filter match {
      case Some(f) =>
        items.filter(item => item.lemmas.exists(l => f contains l.senseIdx(satellites))).
          map(item => item.offset -> item).toMap
      case None =>
        items.map(item => item.offset -> item).toMap
    }
    for(WordNetDataItem(offset, lexNo, pos, lemmas, pointers, frames, gloss) <- items
         /* if lexNo == lex*/
        if inItems contains offset) yield {
      val iliId = ili.get((offset, pos.shortForm)) match {
        case Some(id) =>
          id
        case None if pos.shortForm == "s" =>
          ili.get((offset, "a")) match {
            case Some(id) =>
              id
            case None =>
              "in"
          }
        case None =>
          "in"
      }
      val (definition, examples) = extractExamples(gloss)
      Synset(id="%s-%08d-%s" format (id, offset, pos.shortForm),
             ili=Some(iliId),
             definitions=Seq(Definition(definition)),
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
                   if src == 0 && trg == 0 && (inItems contains targetOffset)) yield {
                     val pos2 = posMap((targetOffset, pos))
                 SynsetRelation(target="%s-%08d-%s" format (id, targetOffset, pos2.shortForm), relType=typ.asInstanceOf[SynsetRelType])
              }
            }, 
            synsetExamples=examples.map(Example(_)),
            partOfSpeech=Some(pos)).withSubject(lexNames(lexNo))
        }
    }

  private def loadSentences(fileName : File) : Map[Int, String] = {
    (io.Source.fromFile(fileName).getLines.map { line =>
      val (id, sentence) = line.splitAt(line.indexOf(" "))
      id.toInt -> sentence.drop(1)
    }).toMap
  }

  private val iliIdType1 = "ili:(i\\d+)".r
  private val iliIdType2 = "<(i\\d+)>".r

  private def loadILIMap(fileName : File) : Map[(Int, String), String] = {
    (io.Source.fromFile(fileName).getLines.filter(_.contains("owl:sameAs")).flatMap { line =>
      val elems = line.split("\\s+")
      val ili = elems(0) match {
        case iliIdType1(id) => id
        case iliIdType2(id) => id
      }
      val offset = if(elems(2).startsWith("pwn31:")) {
        elems(2).drop(7).dropRight(2).toInt
      } else if(elems(2).startsWith("pwn30:")) {
        elems(2).drop(6).dropRight(2).toInt
      } else {
        throw new RuntimeException(elems(2))
      }
      val pos = elems(2).takeRight(1)
      if(elems(1) == "owl:sameAs") {
        if(pos == "s" && elems(2).startsWith("pwn31")) {
          Some((offset, "a") -> ili)
        } else {
          Some((offset, pos) -> ili)
        }
      } else {
        None
      }
    }).toMap
  }

  private def dumpDefinitions(items : Seq[WordNetDataItem]) {
    val out = new java.io.PrintWriter("defs-wn30.csv")
    for(WordNetDataItem(offset, _, pos, _, _, _, gloss) <- items) {
      out.println("%s,%08d,%s" format (gloss.replaceAll(",",""), offset, pos.shortForm))
    }
    out.flush
    out.close
  }

  private object POS {
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
    
  private case class WordNetDataItem(val offset : Int, val lexNo : Int,
       val pos : PartOfSpeech, val lemmas : Seq[Word],
       val pointers : Seq[Pointer], val frames : Seq[Frame],
       val gloss : String) {
  }
       
  private object WordNetDataItem {
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
      ptrs.find(_.typ == similar) match {
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
        "%s-%s-%s:%02d:%02d:%s:%02d" format (lemma.replaceAll(" ","_"), pos.shortForm, POS.code(pos), lexNo, lexId, sattelites(head.get)._1,
        sattelites(head.get)._2)
      } else {
        "%s-%s-%s:%02d:%02d::" format (lemma.replaceAll(" ","_"), pos.shortForm, POS.code(pos), lexNo, lexId)
      }
    }
    def senseKey(sattelites : Map[Int,(String,Int)]) = {
      if(head != None) {
        "%s%%%s:%02d:%02d:%s:%02d" format (lemma.replaceAll(" ","_").toLowerCase, POS.code(pos), lexNo, lexId, sattelites(head.get)._1,
        sattelites(head.get)._2)
      } else {
        "%s%%%s:%02d:%02d::" format (lemma.replaceAll(" ","_").toLowerCase, POS.code(pos), lexNo, lexId)
      }
    }


  }

  private case class Pointer(val typ : RelType, val targetOffset : Int,
       val pos : PartOfSpeech, val src : Int, val trg : Int)
       
  private case class Frame(val frameId : Int, val wordId : Int)

  private object PointerType {
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
      case ";u" => exemplifies
      case "-u" => is_exemplified_by
      case "*" =>  entails 
      case ">" => causes
      case "^" => also
      case "$" => similar 
      case "&" => similar 
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

    def toWN(r : RelType, pos : PartOfSpeech) = {
      if(r == antonym) "!" 
      else if(r == hypernym) "@" 
      else if(r == instance_hypernym) "@i" 
      else if(r == hyponym) "~" 
      else if(r == instance_hyponym) "~i" 
      else if(r == holo_member) "#m" 
      else if(r == holo_substance) "#s" 
      else if(r == holo_part) "#p" 
      else if(r == mero_member) "%m" 
      else if(r == mero_substance) "%s" 
      else if(r == mero_part) "%p" 
      else if(r == attribute) "=" 
      else if(r == derivation) "+" 
      else if(r == domain_topic) ";c" 
      else if(r == has_domain_topic) "-c" 
      else if(r == domain_region) ";r" 
      else if(r == has_domain_region) "-r" 
      else if(r == exemplifies) ";u" 
      else if(r == is_exemplified_by) "-u" 
      else if(r == entails) "*" 
      else if(r == causes) ">" 
      else if(r == also) "^" 
      else if(r == similar && pos == verb) "$" 
      else if(r == similar) "&" 
      else if(r == participle) "<" 
      else if(r == pertainym) "\\"
      else if(r == derivation) "\\"
      else throw new WNDBNotSerializable("Unsupported relation type: " + r)
    }
 
  }

  def write(lr : LexicalResource, file : File) { 
    if(lr.lexicons.size != 1) {
      throw new WNDBNotSerializable("WNDB can only write a single lexicon")
    }
    val lexicon = lr.lexicons(0)

    import org.globalwordnet.api.MultiMap._
    val entriesForSynset : Map[String, Seq[(LexicalEntry,Sense)]] = lexicon.entries.flatMap({ entry =>
      entry.senses.map({ sense =>
        sense.synsetRef -> (entry, sense)
      })
    }).toMultiMap
    val synsetLookup = collection.mutable.Map[String,(String,PartOfSpeech)]()
    val stringBuilders = Seq(adjective,adverb,noun,verb)
      .map(p => p -> new StringBuilder()).toMap
 
    for((posShort,posLong) <- Seq((adjective,"adj"),(adverb,"adv"),
         (noun,"noun"),(verb,"verb"))) {
      writeExc(lexicon, posShort, new File(file, posLong + ".exc"))
      writeData(lr, lexicon, posShort, entriesForSynset, synsetLookup, 
        stringBuilders(posShort), { (oldId, newId) => stringBuilders.values.foreach({
          sb => replaceAll(sb, oldId, newId)
        }) })
    }

    for((posShort,posLong) <- Seq((adjective,"adj"),(adverb,"adv"),
         (noun,"noun"),(verb,"verb"))) {
      val out = new PrintWriter(new File(file, "data." + posLong))
      try {
        out.println(stringBuilders(posShort))
      } finally {
        out.close
      }
      writeIndex(lexicon, posShort, synsetLookup, 
        new PrintWriter(new File(file, "index." + posLong)))
    }
  }

  def writeExc(lexicon : Lexicon, pos : PartOfSpeech, target : File) { 
    val out = new PrintWriter(target)
    try {
      for((form, lemma) <-
           lexicon.entries
             .filter(entry => posMatch(entry.lemma.partOfSpeech, pos))
             .flatMap({ entry =>
              entry.forms.map({ form => (entry.lemma.writtenForm, form.writtenForm) })
          })) {
          if (form.contains(" ")) {
            throw new WNDBNotSerializable("Forms with spaces are not supported")
          }
          out.println(s"$form $lemma")
      }
    } finally {
      out.close
    }
  }

  lazy val extraLexNames = collection.mutable.Map[String, Int]()

  def lexName(name : String) : Int = name match {
    case "adj.all" => 0     
    case "adj.pert" => 1            
    case "adv.all" => 2     
    case "noun.Tops" => 3
    case "noun.act" => 4
    case "noun.animal" => 5
    case "noun.artifact" => 6
    case "noun.attribute" => 7
    case "noun.body" => 8
    case "noun.cognition" => 9
    case "noun.communication" => 10
    case "noun.event" => 11
    case "noun.feeling" => 12
    case "noun.food" => 13
    case "noun.group" => 14
    case "noun.location" => 15
    case "noun.motive" => 16
    case "noun.object" => 17
    case "noun.person" => 18
    case "noun.phenomenon" => 19
    case "noun.plant" => 20
    case "noun.possession" => 21     
    case "noun.process" => 22
    case "noun.quantity" => 23
    case "noun.relation" => 24
    case "noun.shape" => 25
    case "noun.state" => 26
    case "noun.substance" => 27
    case "noun.time" => 28
    case "verb.body" => 29
    case "verb.change" => 30
    case "verb.cognition" => 31
    case "verb.communication" => 32
    case "verb.competition" => 33
    case "verb.consumption" => 34
    case "verb.contact" => 35
    case "verb.creation" => 36
    case "verb.emotion" => 37
    case "verb.motion" => 38
    case "verb.perception" => 39     
    case "verb.possession" => 40     
    case "verb.social" => 41
    case "verb.stative" => 42
    case "verb.weather" => 43
    case "adj.ppl" => 44    
    case other if extraLexNames.contains(other) => extraLexNames(other)
    case other => {
      val id = 45 + extraLexNames.size
      extraLexNames.put(other, id)
      id
    }
  }


  def posMatch(x : Option[PartOfSpeech], pos : PartOfSpeech) : Boolean = {
    x == Some(pos) || (pos == adjective && x == Some(adjective_satellite))
  }

  def posMatch(x : PartOfSpeech, pos : PartOfSpeech) : Boolean = {
    x == pos || (pos == adjective && x == adjective_satellite)
  }

  def PRINCETON_HEADER = """  1 This software and database is being provided to you, the LICENSEE, by  
  2 Princeton University under the following license.  By obtaining, using  
  3 and/or copying this software and database, you agree that you have  
  4 read, understood, and will comply with these terms and conditions.:  
  5   
  6 Permission to use, copy, modify and distribute this software and  
  7 database and its documentation for any purpose and without fee or  
  8 royalty is hereby granted, provided that you agree to comply with  
  9 the following copyright notice and statements, including the disclaimer,  
  10 and that the same appear on ALL copies of the software, database and  
  11 documentation, including modifications that you make for internal  
  12 use or for distribution.  
  13   
  14 WordNet 3.1 Copyright 2011 by Princeton University.  All rights reserved.  
  15   
  16 THIS SOFTWARE AND DATABASE IS PROVIDED "AS IS" AND PRINCETON  
  17 UNIVERSITY MAKES NO REPRESENTATIONS OR WARRANTIES, EXPRESS OR  
  18 IMPLIED.  BY WAY OF EXAMPLE, BUT NOT LIMITATION, PRINCETON  
  19 UNIVERSITY MAKES NO REPRESENTATIONS OR WARRANTIES OF MERCHANT-  
  20 ABILITY OR FITNESS FOR ANY PARTICULAR PURPOSE OR THAT THE USE  
  21 OF THE LICENSED SOFTWARE, DATABASE OR DOCUMENTATION WILL NOT  
  22 INFRINGE ANY THIRD PARTY PATENTS, COPYRIGHTS, TRADEMARKS OR  
  23 OTHER RIGHTS.  
  24   
  25 The name of Princeton University or Princeton may not be used in  
  26 advertising or publicity pertaining to distribution of the software  
  27 and/or database.  Title to copyright in this software, database and  
  28 any associated documentation shall at all times remain with  
  29 Princeton University and LICENSEE agrees to preserve same.  
"""

  def writeData(lr : LexicalResource, lexicon : Lexicon, pos : PartOfSpeech, 
    entriesForSynset : Map[String, Seq[(LexicalEntry,Sense)]],
    synsetLookup : collection.mutable.Map[String, (String, PartOfSpeech)],
    data : StringBuilder, updateId : (String, String) => Unit) {
      if(usePrincetonHeader) {
        data ++= PRINCETON_HEADER
      }
      val lexIds = collection.mutable.Map[String, Int]()
      for(synset <- lexicon.synsets.filter(ss => posMatch(ss.partOfSpeech,pos))) {
        val id = synset.id
        val eightDigitCode = "%08d" format (data.size)
        if(synsetLookup.contains(id)) {
          updateId(synsetLookup(id)._1, eightDigitCode)
        }
        synsetLookup.put(id, (eightDigitCode, pos))
        data ++= eightDigitCode
        data ++= " %02d " format (lexName(synset.subject.getOrElse("none")))
        data ++= synset.partOfSpeech.get.shortForm
        val entries = entriesForSynset.getOrElse(synset.id, Nil)
        data ++= " %02d " format entries.size
        for((entry, sense) <- entries) {
          data ++= entry.lemma.writtenForm.replace(" ", "_")
          val lexId = lexIds.getOrElse(entry.lemma.writtenForm, -1) + 1
          lexIds.put(entry.lemma.writtenForm, lexId)
          data ++= " %d " format lexId
        }
        val totalRelations = synset.synsetRelations.size + entries.map({
          case (entry, sense) => sense.senseRelations.size
        }).sum
        data ++= "%03d " format totalRelations
        for(rel <- synset.synsetRelations) {
          val (targetId, targetPos) = wnSynsetIdFromGlobal(rel.target, lr, synsetLookup)

          data ++= "%s %s %s 0000 " format (PointerType.toWN(rel.relType, pos),
            targetId, targetPos.shortForm)
        }
        for((entry, sense) <- entries) {
          for(rel <- sense.senseRelations) {
            val (targetEntry, targetSense) = lr.senseLookup.getOrElse(rel.target,
              throw new WNDBNotSerializable("Target of sense relation not in lexical resource: " + rel.target))
            val (targetId, targetPos) = wnSynsetIdFromGlobal(targetSense.synsetRef, lr, synsetLookup)

            val srcIdx = entries.indexOf((entry, sense))
            val trgIdx = entriesForSynset.getOrElse(targetSense.synsetRef, Nil)
              .indexOf((targetEntry, targetSense))

            data ++= "%s %s %s %02d%02d " format (PointerType.toWN(rel.relType, pos),
              targetId, targetPos.shortForm, srcIdx+1, trgIdx+1)
          }
        }
        for(defn <- synset.definitions) {
          data ++= "| " + defn.content
        }
        data ++= "\n"
      }
  }

  def wnSynsetIdFromGlobal(target : String, lr : LexicalResource,
    synsetLookup : collection.mutable.Map[String, (String, PartOfSpeech)]) : (String, PartOfSpeech) = {
      synsetLookup.get(target) match {
        case Some(i) => i
        case None => 
          val id2 = "Z%06dZ" format synsetLookup.size
          val pos2 =  lr.synsetLookup.get(target)
            .getOrElse(
              throw new WNDBNotSerializable("Target of synset relation not in lexical resource:" + target))
            .partOfSpeech.getOrElse(
              throw new WNDBNotSerializable("Link to synset without part of speech: " + target))
          synsetLookup.put(target, (id2,pos2))
          (id2, pos2)
      }
 
    }

  def replaceAll(sb : StringBuilder, orig : String, replace : String) = {
    var i = -1
    while({ i = sb.indexOf(orig) ; i} >= 0) {
      sb.replace(i, i + orig.length, replace)
    }
  }

  def writeIndex(lexicon : Lexicon, pos : PartOfSpeech, 
    synsetLookup : collection.mutable.Map[String, (String, PartOfSpeech)],
    out : PrintWriter) { 
    try {
      if(usePrincetonHeader) {
        out.print(PRINCETON_HEADER)
      }
      val words = lexicon.entries
        .filter(e => posMatch(e.lemma.partOfSpeech, pos))
        .groupBy(_.lemma.writtenForm).toSeq.sortBy(_._1)
      for((lemma, entries) <- words) {
        val synsetCnt = entries.map(_.senses.size).sum
        val ptrs = entries.flatMap({ entry =>
          entry.senses.flatMap({ sense =>
            sense.senseRelations.map(_.relType)
          })
        }).toSet
        val ptrsStr = ptrs.map(rt => PointerType.toWN(rt, pos) + " ").mkString("")
        val synsets = entries.flatMap({ entry =>
          entry.senses.map({ sense =>
            synsetLookup.getOrElse(sense.synsetRef,
              throw new RuntimeException("Failed to find synset in indexing (should be impossible)"))._1
          })
        })

        out.println("%s %s %d %d %s%d 0 %s" format(lemma.replace(" ", "_"), pos.shortForm,
          synsetCnt, ptrs.size, ptrsStr, synsetCnt, synsets.mkString(" ")
          ))
      }
    } finally {
      out.close
    }
  }
}

class WNDBNotSerializable(msg : String = "", cause : Throwable = null) extends RuntimeException(msg, cause)
