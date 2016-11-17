import sbt._
import sbt.Keys._

object PoemMakerBuilder extends Build {

  lazy val poemmaker = Project(
    id = "poem-maker",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "poem-maker",
      organization := "org.github.zachncst",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.12.0",
      // add other settings here,
      libraryDependencies += "co.fs2" %% "fs2-core" % "0.9.2",
      libraryDependencies += "co.fs2" %% "fs2-io" % "0.9.2"
    )
  )
}
