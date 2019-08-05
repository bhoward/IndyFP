import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0"

  val continuationsVersion = "1.0.3"
  lazy val continuationsPlugin = "org.scala-lang.plugins" % "scala-continuations-plugin_2.12.2" % continuationsVersion
  lazy val continuationsLibrary =  "org.scala-lang.plugins" %% "scala-continuations-library" % continuationsVersion
}
