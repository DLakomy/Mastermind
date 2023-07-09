Global / excludeLintKeys := Set(nativeImageVersion)

ThisBuild / scalaVersion := "3.3.0"

lazy val root = project
  .in(file("."))
  .enablePlugins(NativeImagePlugin)
  .settings(
    name := "MasterMind",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.16" % Test,
    nativeImageOptions  += "--no-fallback",
    nativeImageVersion  := "22.1.0",
    nativeImageOutput := file("target") / "native-image" /"mastermind"
  )
