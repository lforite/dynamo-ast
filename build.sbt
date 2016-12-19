name := "dynamo-ast"

version := "1.0"

scalaVersion := "2.12.0"

libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.6" % Test
libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "3.8.6" % Test
libraryDependencies += "org.typelevel" %% "cats" % "0.8.1"


scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-unchecked",
  "-feature",
  "-deprecation:false",
  "-Xlint",
  "-Xcheckinit",
  "-Ywarn-unused-import",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-dead-code",
  "-Yno-adapted-args",
  "-language:_",
  "-target:jvm-1.8",
  "-encoding", "UTF-8")
