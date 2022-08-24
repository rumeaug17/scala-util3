ThisBuild / version := "1.1.0"

maintainer := "rumeaug17@gmail.com"

ThisBuild / scalaVersion := "3.1.3"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.12"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "scala-util3",
    organization := "org.rg",
    idePackagePrefix := Some("org.rg.su3")
  )

// for dist packaging
enablePlugins(JavaAppPackaging)

// after a publish-local, adding dependencies to other projects via
// libraryDependencies += "org.rg" %% "scala-util3" % "1.0.0"