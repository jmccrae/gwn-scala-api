package org.globalwordnet.api.serialize

import eu.monnetproject.lang.Language
import org.globalwordnet.api._
import org.globalwordnet.api.wn._
import java.io.{File, PrintWriter}
import org.apache.commons.lang3.StringEscapeUtils.{escapeXml10 => escapeXml}

class TEI() extends Format {
    def read(file : File) : LexicalResource =
        throw new UnsupportedOperationException()

import MoreStringEscapeUtils._

    def write(resource : LexicalResource, output : File) {
        val out = new PrintWriter(output)
        writeLexicalResource(resource, out)
        out.flush
        out.close
    }

    def writeLexicalResource(resource : LexicalResource, out : PrintWriter) {
        if(resource.lexicons.size == 1) {
            writeLexicon(resource.lexicons(0), out)
        } else {
            throw new IllegalArgumentException("Can only serialize single lexicon wordnets as TEI")
        }
    }

    def writeLexicon(lexicon : Lexicon, out : PrintWriter) {     
        out.print(s"""<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <teiHeader>""")
        writeHeader(lexicon, out)
        out.print(s"""
  </teiHeader>
  <text>
    <body>""")
        for(entry <- lexicon.entries) {
            writeEntry(entry, out, lexicon.language, lexicon.synsetsById)
        }
        out.print(s"""
    </body>
  </text>
</TEI>""")
    }

    def writeHeader(lexicon : Lexicon, out : PrintWriter) {
       out.print(s"""
    <fileDesc>
      <titleStmt>
        <title>${lexicon.label} (Version ${lexicon.version})</title>
      </titleStmt>
      <publicationStmt>
        <distributor>
          <email>${lexicon.email}</email>
        </distributor>
        <availability>
          <licence target="${lexicon.license}"/>
        </availability>
      </publicationStmt>""")
        if(!lexicon.citation.isEmpty || !lexicon.url.isEmpty) {
            out.print("""
      <sourceDesc>""")
        }
        lexicon.citation match {
          case Some(cit) => out.print(s"""
        <bibl>$cit</bibl>""")
          case None => {}
        }
        lexicon.url match {
          case Some(url) => out.print(s"""
        <ref target="$url"/>""")
          case None => {}
        }
        if(!lexicon.citation.isEmpty || !lexicon.url.isEmpty) {
            out.print("""
      </sourceDesc>""")
        }
        out.print(s"""
     </fileDesc>
    <profileDesc>
      <langUsage>
        <language ident="${lexicon.language.toString}">${lexicon.language.getName}</language>
      </langUsage>
    </profileDesc>""")
        if(!lexicon.contributor.isEmpty ||
           !lexicon.coverage.isEmpty ||
           !lexicon.creator.isEmpty ||
           !lexicon.date.isEmpty ||
           !lexicon.description.isEmpty ||
           !lexicon.format.isEmpty ||
           !lexicon.identifier.isEmpty ||
           !lexicon.publisher.isEmpty ||
           !lexicon.relation.isEmpty ||
           !lexicon.rights.isEmpty ||
           !lexicon.source.isEmpty ||
           !lexicon.subject.isEmpty ||
           !lexicon.title.isEmpty ||
           !lexicon.`type`.isEmpty ||
           !lexicon.status.isEmpty) {
            out.print(s"""
    <xenoData xmlns:dc="http://purl.org/dc/terms/">""")
        }
        lexicon.coverage.foreach(x => out.print(s"""
      <dc:coverage>${escapeXml(x)}</dc:coverage>""""))
        lexicon.creator.foreach(x => out.print(s"""
      <dc:creator>${escapeXml(x)}</dc:creator>""""))
        lexicon.contributor.foreach(x => out.print(s"""
      <dc:contributor>${escapeXml(x)}</dc:contributor>""""))
        lexicon.contributor.foreach(x => out.print(s"""
      <dc:contributor>${escapeXml(x)}</dc:contributor>""""))
        lexicon.contributor.foreach(x => out.print(s"""
      <dc:contributor>${escapeXml(x)}</dc:contributor>""""))
        lexicon.contributor.foreach(x => out.print(s"""
      <dc:contributor>${escapeXml(x)}</dc:contributor>""""))
        lexicon.contributor.foreach(x => out.print(s"""
      <dc:contributor>${escapeXml(x)}</dc:contributor>""""))
        lexicon.contributor.foreach(x => out.print(s"""
      <dc:contributor>${escapeXml(x)}</dc:contributor>""""))
        lexicon.contributor.foreach(x => out.print(s"""
      <dc:contributor>${escapeXml(x)}</dc:contributor>""""))
        lexicon.contributor.foreach(x => out.print(s"""
      <dc:contributor>${escapeXml(x)}</dc:contributor>""""))
        lexicon.contributor.foreach(x => out.print(s"""
      <dc:contributor>${escapeXml(x)}</dc:contributor>""""))
        lexicon.contributor.foreach(x => out.print(s"""
      <dc:contributor>${escapeXml(x)}</dc:contributor>""""))
        lexicon.contributor.foreach(x => out.print(s"""
      <dc:contributor>${escapeXml(x)}</dc:contributor>""""))
    }

    def writeEntry(entry : LexicalEntry, out : PrintWriter, 
        _lang : Language,
        synsetsById : Map[String, Synset]) {
        val lang = entry.lemma.script match {
            case Some(s) => _lang.getScriptVariant(s)
            case None => _lang
        }
        out.print(s"""
      <entry xml:id="${escapeXmlId(entry.id)}">
        <form type="lemma">
          <orth xml:lang="${lang}">${escapeXml(entry.lemma.writtenForm)}</orth>
        </form>""")
        for(form <- entry.forms) {
            writeForm(form, out, _lang)
        }
        // TODO: Script
        out.print(s"""
        <gramGrp>
          <pos norm="${entry.lemma.partOfSpeech.name}">${entry.lemma.partOfSpeech.name}</pos>""")
        for(tag <- entry.lemma.tag) {
            out.print(s"""
          <gram type="${escapeXmlId(tag.category)}">${escapeXmlId(tag.value)}</gram>""")
        }
        out.print(s"""
        </gramGrp>""")
        for(sense <- entry.senses) {
            writeSense(sense, out, synsetsById)
        }
    }

    def writeForm(form : Form, out : PrintWriter, _lang : Language) {
        val lang = form.script match {
            case Some(s) => _lang.getScriptVariant(s)
            case None => _lang
        }
        out.print(s"""
        <form type="inflected">
            <orth xml:lang="${lang}">${escapeXml(form.writtenForm)}</orth>""")
        if(!form.tag.isEmpty) {
            out.print(s"""
            <gramGrp>""")
            for(tag <- form.tag) {
                out.print(s"""
              <gram type="${escapeXmlId(tag.category)}">${escapeXmlId(tag.value)}</gram>""")
            }
            out.print(s"""
            </gramGrp>""")
        }
        out.print(s"""
        </form>""")
    }

    def writeSense(sense : Sense, out : PrintWriter,
        synsetsById : Map[String, Synset]) {
        out.print(s"""
        <sense xml:id=${escapeXmlId(sense.id)}>""")
        val synset = synsetsById.getOrElse(sense.synsetRef,
            throw new RuntimeException("Synset ref not in lexicon"))
        for(defn <- synset.definitions) {
            writeDefinition(defn, out)
        }
        synset.ili.foreach(i =>
            out.print(s"""
            <xr type="ili"><ref target="http://ili.globalwordnet.org/ili/${i}"/></xr>"""))
        for(rel <- synset.synsetRelations) {
            writeSynsetRelation(rel, out)
        }
        for(rel <- sense.senseRelations) {
            writeSenseRelation(rel, out)
        }
        for(example <- sense.senseExamples) {
            writeExample(example,out)
        }
        // IGNORED: Synset Examples
        // IGNORED: Counts
        // IGNORED: Synset Part of Speech        
    }

    def writeDefinition(defn : Definition, out : PrintWriter) {
        out.print(s"""
          <def""")
        defn.language match {
            case Some(l) => out.print(s""" xml:lang="$l"""")
            case None => {}
        }
        defn.sourceSense match {
            case Some(l) => out.print(s""" orig="${escapeXmlId(l)}">""")
            case None => out.print(s""">"""")
        }
        out.print("""${escapeXml(defn.content)}</def>""")
    }

    def writeSynsetRelation(rel : SynsetRelation, out : PrintWriter) {
        val rt = rel.relType match {
            case other(t) => t
            case x => x.name
        }
        val rtPretty = rt.replaceAll("_", " ").head.toUpper +
            rt.replaceAll("_", " ").tail
        out.print(s"""
          <xr type="$rt">$rtPretty: <ref target="#${rel.target}">${rel.target}</ref></xr>""")
    }

    def writeSenseRelation(rel : SenseRelation, out : PrintWriter) {
        val rt = rel.relType match {
            case other(t) => t
            case x => x.name
        }
        val rtPretty = rt.replaceAll("_", " ").head.toUpper +
            rt.replaceAll("_", " ").tail
        out.print(s"""
          <xr type="$rt">$rtPretty: <ref target="#${rel.target}">${rel.target}</ref></xr>""")
    }

    def writeExample(example : Example, out : PrintWriter) {
        out.print("""
          <cit type="example"""")
        example.language match {
            case Some(l) => out.print(s""" xml:lang="$l">""")
            case None => out.print(">")
        }
        out.print(s"""${escapeXml(example.content)}</cit>""")
    }
}
