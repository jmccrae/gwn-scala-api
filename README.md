# Global WordNet Converter and Validator

API and command line tool for working with Global WordNet Interlingual Index 
formats 

Installation
------------

You will need the [Scala SBT](http://www.scala-sbt.org/) tool to compile and run
the program as well as Java. Once that is installed the tool can be run as

    ./gwn

The options are as follows:

    Usage: gwn [options]
    
      -v | --validate
        Validate the input file only
      -i <inputFile> | --input <inputFile>
        The input file to process
      -o <outputFile> | --output <outputFile>
        The output file to process
      -a <auxFile> | --aux <auxFile>
        The auxiliary file (normaly PWN 3.0) required by some formats
      -f <format> | --from <format>
        The format of the input file: WNLMF, JSON, RDF, WNDB, OMWN, PLWN, DEBVISDIC, W3C
      -t <format> | --to <format>
        The format of the output file: WNLMF, JSON, RDF
      --aux-format <format>
        The format of the auxiliary file
      --id <id>
        The identifier for the resource
      --label <label>
        The label for the resource
      --language <isoCode>
        The language of the resource (ISO code)
      --email <address>
        The email address for this resource
      --license <url>
        The license of the resource
      --version <versionId>
        The version of the resource
      --url <url>
        The URL of the resource
      --citation <paper>
        The citation string of the resource
      --input-rdf-lang <RDF/XML|TURTLE|N-TRIPLE|N3>
        The RDF language to serialize from
      --output-rdf-lang <RDF/XML|TURTLE|N-TRIPLE|N3>
        The RDF language to serialize to
      --input-base-url <url>
        The Base URL, i.e., where the file is on the Web, for the input file
      --output-base-url <url>
        The Base URL, i.e., where the file is on the Web, for the output file

Example
=======

Command to generate Princeton WordNet 3.1 in XML

    ./gwn -i wn31/dict -o wn31.xml -f WNDB -t WNLMF \
      -a wn-data/ili-map-wn31.ttl --id pwn31 --label "Princeton WordNet 3.1" \
      --version "3.1" --url "http://wordnet.princeton.edu/" \
      --license "http://wordnet.princeton.edu/wordnet/license/" \
      --email "***@princeton.edu"

