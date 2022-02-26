
name := "untitled4"

version := "0.1"

scalaVersion := "2.12.10"

scalacOptions += "-Ypartial-unification"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

val catsVersion = "1.6.0"
val http4sVersion = "0.20.23"
val scalaXmlVersion = "1.2.0"
val scalaLoggingVersion = "3.9.4"
val logbackVersion = "1.1.2"

val catsCore ="org.typelevel" %% "cats-core" % catsVersion
val scalaXml = "org.scala-lang.modules" %% "scala-xml" % scalaXmlVersion
val scalalogging = "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion
val logback = "ch.qos.logback" % "logback-classic" % logbackVersion
val http4s = Seq(
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion
)

libraryDependencies ++= (logback +: scalalogging +: catsCore +: scalaXml +: http4s)