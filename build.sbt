ThisBuild / version := "1.0.1-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.10"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "scala-util3",
    idePackagePrefix := Some("org.rg.su3")
  )
