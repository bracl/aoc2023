ThisBuild / scalaVersion := "3.3.1"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "aoc2023",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.19",
      "dev.zio" %% "zio-test" % "2.0.19"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
