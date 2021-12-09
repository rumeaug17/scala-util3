ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.0.1"

lazy val root = (project in file("."))
  .settings(
    name := "scala-util3",
    idePackagePrefix := Some("org.rg.su3")
  )
