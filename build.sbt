
ThisBuild / scalaVersion := "3.2.2"
ThisBuild / organization := "de.schoolsdebate"
ThisBuild / version      := "0.1-SNAPSHOT"

lazy val wuebutab = (project in file("."))
  .settings(
    name := "Wuebutab",
    libraryDependencies ++= Seq(
      "org.rogach"           %% "scallop"       % "4.1.0",
      "dev.optics"           %% "monocle-core"  % "3.1.0",
      "dev.optics"           %% "monocle-macro" % "3.1.0",
      "com.github.tototoshi" %% "scala-csv"     % "1.3.10",
      "org.jgrapht"          %  "jgrapht-core"  % "1.5.1",
    ),
    assembly / mainClass := Some("wuebutab.main"),
  )
