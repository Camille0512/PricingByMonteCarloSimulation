ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

libraryDependencies  ++= Seq(
  // Last stable release
  "org.scalanlp" %% "breeze" % "2.1.0",

  // The visualization library is distributed separately as well.
  // It depends on LGPL code
  "org.scalanlp" %% "breeze-viz" % "2.1.0",

  "org.scalanlp" %% "breeze-natives" % "2.1.0",

  "org.scalatest" %% "scalatest" % "3.2.11" % Test
)

lazy val root = (project in file("."))
  .settings(
    name := "MonteCarloSimulation"
  )
