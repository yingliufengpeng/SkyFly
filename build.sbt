ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

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