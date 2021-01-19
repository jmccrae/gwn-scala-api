libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

libraryDependencies += "io.spray" %%  "spray-json" % "1.3.2"

libraryDependencies += "org.apache.jena" % "jena-arq" % "3.0.0" exclude("org.slf4j","jcl-over-slf4j")

libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.0" 

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-target:jvm-1.7")

javacOptions ++= Seq("-source", "1.7", "-target", "1.7")

mainClass in assembly := Some("org.globalwordnet.api.Main")

version := "0.2"
