lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      scalaVersion := "3.3.0",
    )),
    name := "MasterMind",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.16" % Test
  )
