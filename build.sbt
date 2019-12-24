


lazy val root = (project in file("."))
  .settings(
    name := "trees",
    scalaVersion := "2.13.0",
    version := "0.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
  )
