
ThisBuild / scalaVersion := "3.8.3"
ThisBuild / organization := "de.schoolsdebate"
ThisBuild / version      := "0.3-SNAPSHOT"

lazy val tabbouleh = (project in file("."))
  .settings(
    name := "tabbouleh",
    libraryDependencies ++= Seq(
      "org.rogach"              %% "scallop"                    % "6.0.0",
      "dev.optics"              %% "monocle-core"               % "3.3.0",
      "dev.optics"              %% "monocle-macro"              % "3.3.0",
      "com.lihaoyi"             %% "upickle"                    % "4.4.3",
      "org.jgrapht"             %  "jgrapht-core"               % "1.5.2",
      "com.google.api-client"   %  "google-api-client"          % "2.9.0",
      "com.google.oauth-client" %  "google-oauth-client-jetty"  % "1.39.0",
      "com.google.apis"         %  "google-api-services-sheets" % "v4-rev20260213-2.0.0"
    ),
    scalacOptions += "-deprecation",
    assembly / mainClass := Some("de.schoolsdebate.tabbouleh"),
  )
