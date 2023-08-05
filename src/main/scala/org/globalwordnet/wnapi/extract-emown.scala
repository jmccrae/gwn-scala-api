import java.io.File

import org.globalwordnet.api.wn._
import org.globalwordnet.api.serialize.WNLMF

object EmoWNExtract {

  def findPWN(synset : String,
    synset_mapping : Map[String,String],
    synrelations : Map[String, Seq[(String, String)]]) : Seq[String] = {
      synrelations.getOrElse(synset, Seq()).filter(x => x._2 == "208" || 
        x._2 == "209" || x._2 == "210" || x._2 == "211" || x._2 == "212" ||
        x._2 == "213").map(_._1)
  }

  def main(args : Array[String]) : Unit = {

    import org.globalwordnet.api.serialize.plWordNetReader._
    import org.globalwordnet.api.plwn._
    val plWordNetFile = new java.io.File("../plwordnet-to-gw/plWordNet-dev.xml.gz")
    // ./gwn -i ~/Downloads/WordNet-3.0/dict -o wn30.xml -f WNDB -t WNLMF -a wn-data/ili-map-wn30.ttl
    val wn31          = new WNLMF().read(new File("wn30.xml"))
    val (eentries, esynsets, elexrelations, esynrelations, edescriptions) = load_plwordnet(true, plWordNetFile)
    val (pentries, psynsets, plexrelations, psynrelations, pdescriptions) = load_plwordnet(false, plWordNetFile)
    val (pwn_entries, pwn_defns, ili) = load_pwn(wn31)
    val esenses = build_senses(esynsets)
    val psenses = build_senses(psynsets)
        
    val (entry_mapping, synset_mapping) = map_plwordnet_to_pwn(eentries, esenses, esynsets, pwn_entries, pwn_defns)

    val (em, sm) = map_plwordnet_to_pwn(eentries, esenses, esynsets, pwn_entries, pwn_defns)

    val plD = pdescriptions.view.mapValues(parseDescription)
 
    var nolink = 0
    var noili = 0
    var emo = 0
    var emouniq = 0
    val out = new java.io.PrintWriter("emos.txt")
    for((key, descs) <- plD) {
      for(desc <- descs) {
        desc match {
          case Emotion(an, p, u, _, _) =>
            emo += 1
            if(!descs.exists({
              case Emotion(an2, _, _, _, _) if an2 > an =>
                true
              case _ =>
                false
            })) {
              emouniq += 1
              if(psenses(key).isEmpty) {
                System.err.println("No Synset: " + key)
              }
              for(ss <- psenses(key)) {
                if(findPWN(ss, sm, psynrelations).isEmpty) {
                  System.err.println("No link to PWN: " + ss)
                  nolink += 1
                }
                for(t <- findPWN(ss, sm, psynrelations)) {
                  val i = ili.getOrElse(sm.getOrElse(t, "in"), "in")
                  if(i != "in") {
                    out.println("%s %s %s" format (i, p.mkString(" "), u.mkString(" ")))
                  } else {
                    System.err.println("Not linked to ILI: " + t)
                    noili += 1
                  }
                }
              }
            }
          case _ =>
        }
      }
    }
    println(nolink)
    println(noili)
    println(emo)
    println(emouniq)
    out.flush
    out.close
  }
}
