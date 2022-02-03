ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "LRUCache",
    idePackagePrefix := Some("com.github.dcameronmauch"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % Test
  )
