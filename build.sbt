import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.dlakomy",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "MasterMind",
    libraryDependencies += scalaTest % Test,

    // logger
    libraryDependencies ++= Seq(
      "ch.qos.logback"             %  "logback-classic" % "1.1.7",
      "com.typesafe.scala-logging" %% "scala-logging"   % "3.5.0"
    )
  )
