libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "io.spray" %%  "spray-json" % "1.3.2"

libraryDependencies += "org.apache.jena" % "jena-arq" % "3.0.0"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0" 

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

mainClass in assembly := Some("org.globalwordnet.api.Main")
