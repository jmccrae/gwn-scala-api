package org.globalwordnet.api.serialize

import eu.monnetproject.lang.{Language, Script}
import org.globalwordnet.api.wn._
import org.scalatest._

class WNDBSpec extends FlatSpec with Matchers {
  var lr = new WNLMF(false).read(new java.io.FileReader("src/test/resources/example3.xml"))


  it should "output a correct data.noun file" in {
    val wndb = new WNDB(null, null, null, null, null, null, null, None, None, true, None)
    import org.globalwordnet.api.MultiMap._
    val entriesForSynset : Map[String, Seq[(LexicalEntry,Sense)]] = lr.lexicons(0).entries.flatMap({ entry =>
      entry.senses.map({ sense =>
        sense.synsetRef -> (entry, sense)
      })
    }).toMultiMap
    val synsetLookup = collection.mutable.Map[String, (String, PartOfSpeech)]()
    val data = (new StringBuilder(), collection.mutable.Map[String, Seq[Int]]())
    
    wndb.writeData(lr, lr.lexicons(0), noun, entriesForSynset, synsetLookup, data,
      (oldId, newId) => {
        wndb.replaceAll(data, oldId, newId)
      })


    data._1.toString should be ("""  1 This software and database is being provided to you, the LICENSEE, by  
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
00001740 45 n 01 paternal_grandfather 0 001 + 00001848 n 0101 | A father's father; a paternal grandfather  
00001848 45 n 01 grandfather 0 001 @ 00001941 n 0000 | the father of your father or mother  
00001941 45 n 00 000   
""")
  }

  it should "output a correct index.noun file" in {
    val wndb = new WNDB(null, null, null, null, null, null, null, None, None, true, None)
    import org.globalwordnet.api.MultiMap._
    val entriesForSynset : Map[String, Seq[(LexicalEntry,Sense)]] = lr.lexicons(0).entries.flatMap({ entry =>
      entry.senses.map({ sense =>
        sense.synsetRef -> (entry, sense)
      })
    }).toMultiMap
    val synsetLookup = collection.mutable.Map[String, (String, PartOfSpeech)]()
    val data = (new StringBuilder(), collection.mutable.Map[String, Seq[Int]]())
    
    val sw = new java.io.StringWriter()
    val out = new java.io.PrintWriter(sw)

    wndb.writeData(lr, lr.lexicons(0), noun, entriesForSynset, synsetLookup, data,
      (oldId, newId) => {
        wndb.replaceAll(data, oldId, newId)
      })

    wndb.writeIndex(lr.lexicons(0), noun, synsetLookup, out)

    sw.toString should be ("""  1 This software and database is being provided to you, the LICENSEE, by  
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
grandfather n 1 1 @ 1 0 00001848  
paternal_grandfather n 1 1 + 1 0 00001740  
""")
  }

  it should "output a correct index.sense" in {
    val wndb = new WNDB(null, null, null, null, null, null, null, None, None, true, None)
    import org.globalwordnet.api.MultiMap._
    val entriesForSynset : Map[String, Seq[(LexicalEntry,Sense)]] = lr.lexicons(0).entries.flatMap({ entry =>
      entry.senses.map({ sense =>
        sense.synsetRef -> (entry, sense)
      })
    }).toMultiMap
    val synsetLookup = collection.mutable.Map[String, (String, PartOfSpeech)]()
    val data = (new StringBuilder(), collection.mutable.Map[String, Seq[Int]]())
    
    val sw = new java.io.StringWriter()
    val out = new java.io.PrintWriter(sw)

    wndb.writeData(lr, lr.lexicons(0), noun, entriesForSynset, synsetLookup, data,
      (oldId, newId) => {
        wndb.replaceAll(data, oldId, newId)
      })

    wndb.writeSenseIndex(lr.lexicons(0), synsetLookup, entriesForSynset, out)

    sw.toString should be ("""grandfather%1:01:00:: 00001848 1 0
paternal_grandfather%1:01:00:: 00001740 1 0
""")
  }
}
