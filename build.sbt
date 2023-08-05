libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies += "io.spray" %%  "spray-json" % "1.3.6"

libraryDependencies += "org.apache.jena" % "jena-arq" % "3.0.0" exclude("org.slf4j","jcl-over-slf4j")

libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.1" 

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.2.0"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

mainClass in assembly := Some("org.globalwordnet.api.Main")

version := "0.2"

scalaVersion := "2.13.11"
