import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "edu.depauw",
      scalaVersion := "2.12.8",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Derivatives",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalaCheck % Test,

    autoCompilerPlugins := true,
    addCompilerPlugin(continuationsPlugin),
    libraryDependencies += continuationsLibrary,
    scalacOptions += "-P:continuations:enable"
  )
