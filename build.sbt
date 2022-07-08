ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val scalatest_version = "3.2.12"


ThisBuild / libraryDependencies += "org.scalactic" %% "scalactic" % scalatest_version
ThisBuild / libraryDependencies += "org.scalatest" %% "scalatest" % scalatest_version % "test"

ThisBuild / libraryDependencies += "org.scalatest" %% "scalatest-funsuite" % scalatest_version % "test"

ThisBuild / resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"


lazy val root = (project in file("."))
  .settings(
    name := "SkyFly",
    idePackagePrefix := Some("com.skyfly")
  ).aggregate(util, core)



lazy val util = (project in file("util"))
  .settings(
    name := "util",
  )

lazy val core = (project in file("core"))
  .settings(
    name := "core",
  ).dependsOn(util)