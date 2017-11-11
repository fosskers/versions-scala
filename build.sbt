name := """versions"""

version := "1.0.0"

scalaVersion in ThisBuild := "2.12.4"

scalacOptions := Seq(
  "-deprecation",
  "-unchecked",
  "-Ypartial-unification"
  // "-Ypatmat-exhaust-depth", "100"
)

/* For Monocle's Lens auto-generation */
addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "com.github.julien-truffaut" %% "monocle-core"          % "1.5.0-cats-M2",
  "com.github.julien-truffaut" %% "monocle-macro"         % "1.5.0-cats-M2",
  "org.typelevel"              %% "cats-core"             % "1.0.0-RC1",
  "org.typelevel"              %% "kittens"               % "1.0.0-RC1",
  "org.scalatest"              %% "scalatest"             % "3.0.4" % "test"
)

initialCommands in console :=
"""
import versions._
import cats.implicits._
"""
