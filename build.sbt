scalaVersion := "3.2.2"

lazy val wuebutab = (project in file("."))
  .settings(
    name := "Wuebutab",
    libraryDependencies += "org.springframework.boot" % "spring-boot-starter" % "3.0.2",
    libraryDependencies += "org.springframework.shell" % "spring-shell-starter" % "3.0.0",
    libraryDependencies += "org.jgrapht" % "jgrapht-core" % "1.5.1",
    libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.10"
  )