val ScalatraVersion = "2.6.3"

organization := "org.globalwordnet"

name := "gwn-scala-api-web"

version := "0.2"

scalaVersion := "2.12.6"

resolvers += Classpaths.typesafeReleases

libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra" % ScalatraVersion,
  "org.scalatra" %% "scalatra-scalatest" % ScalatraVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.2.3" % "runtime",
  "org.eclipse.jetty" % "jetty-webapp" % "9.4.9.v20180320" % "container",
  "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided",
  "default" %% "gwn-scala-api" % "0.2"
)

enablePlugins(SbtTwirl)
enablePlugins(ScalatraPlugin)
