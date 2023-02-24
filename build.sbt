scalaVersion := "3.2.2"

lazy val wuebutab = (project in file("."))
  .settings(
    name := "Wuebutab",
    libraryDependencies ++= Seq(
      "org.springframework.boot"  %  "spring-boot-starter"  % "3.0.2",
      "org.springframework.shell" %  "spring-shell-starter" % "3.0.0",
      "org.jgrapht"               %  "jgrapht-core"         % "1.5.1",
      "dev.optics"                %% "monocle-core"         % "3.1.0",
      "dev.optics"                %% "monocle-macro"        % "3.1.0",
      "com.github.tototoshi"      %% "scala-csv"            % "1.3.10"
    )
  )