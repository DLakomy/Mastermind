ThisBuild / scalaVersion := "3.3.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "MasterMind",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.16" % Test
  )
