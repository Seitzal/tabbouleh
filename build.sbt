
ThisBuild / scalaVersion := "3.3.1"
ThisBuild / organization := "de.schoolsdebate"
ThisBuild / version      := "0.1-SNAPSHOT"

lazy val wuebutab = (project in file("."))
  .settings(
    name := "Wuebutab",
    libraryDependencies ++= Seq(
      "org.rogach"              %% "scallop"                    % "5.0.0",
      "dev.optics"              %% "monocle-core"               % "3.2.0",
      "dev.optics"              %% "monocle-macro"              % "3.2.0",
      "com.lihaoyi"             %% "upickle"                    % "3.2.0",
      "org.jgrapht"             %  "jgrapht-core"               % "1.5.2",
      "com.google.api-client"   %  "google-api-client"          % "2.0.0",
      "com.google.oauth-client" %  "google-oauth-client-jetty"  % "1.34.1",
      "com.google.apis"         %  "google-api-services-sheets" % "v4-rev20230815-2.0.0"
    ),
    assembly / mainClass := Some("wuebutab.main"),
  )
