ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"
//ThisBuild / scalaVersion := "3.0.0"

lazy val scalatest_version = "3.2.12"


ThisBuild / libraryDependencies += "org.scalactic" %% "scalactic" % scalatest_version
ThisBuild / libraryDependencies += "log4j" % "log4j" % "1.2.17"
ThisBuild / libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.13"
ThisBuild / libraryDependencies += "org.scalatest" %% "scalatest" % scalatest_version % "test"
//
ThisBuild / libraryDependencies += "org.scalatest" %% "scalatest-funsuite" % scalatest_version % "test"

ThisBuild / resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"

//ThisBuild / autoScalaLibrary := false

ThisBuild / libraryDependencies += "junit" % "junit" % "4.12"

ThisBuild / libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test"

//docs https://github.com/sbt/junit-interface
ThisBuild / testOptions += Tests.Argument(TestFrameworks.JUnit
  //tests to run, Only individual test case names are matched
  //, --tests=<REGEXPS>
)


lazy val root = (project in file("."))
  .settings(
    name := "SkyFly",
  ).aggregate(util, core)



lazy val util = (project in file("util"))
  .settings(
    name := "util",
  )

lazy val core = (project in file("core"))
  .settings(
    name := "core",
  ).dependsOn(util)