ThisBuild / version := "1.0.1-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "scala-util3",
    idePackagePrefix := Some("org.rg.su3")
  )
