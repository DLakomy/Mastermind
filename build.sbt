import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      scalaVersion := "2.12.3",
    )),
    name := "MasterMind",
    libraryDependencies += scalaTest % Test,

    // logger
    libraryDependencies ++= Seq(
      "ch.qos.logback"             %  "logback-classic" % "1.1.7",
      "com.typesafe.scala-logging" %% "scala-logging"   % "3.5.0"
    )
  )
