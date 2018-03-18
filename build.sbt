organization := "com.github.dos65"
name := "mdparse"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.8"

val circeVersion = "0.9.1"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0"
)

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion % "test")
