name := "dynamo-ast"

organization := "com.github.louis-forite"

version := "0.1"

scalaVersion := "2.12.0"
crossScalaVersions := Seq("2.11.8", "2.12.0")

lazy val dynamoast =  project.in(file("."))
    .settings(publishSettings)

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

lazy val publishSettings = Seq(
  publishTo := Option("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"),
  publishMavenStyle := true,
  homepage := Some(url("https://github.com/louis-forite/dynamo-ast")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/louis-forite/dynamo-ast"), "scm:git:git@github.com:louis-forite/dynamo-ast.git")),
  autoAPIMappings := true,
  pomExtra := (
      <developers>
        <developer>
          <id>louis-forite</id>
          <name>Louis Forite</name>
          <url>https://github.com/louis-forite/</url>
        </developer>
      </developers>
      ),
  publishArtifact in Test := false
) ++ credentialSettings

lazy val credentialSettings = Seq(
  // For Travis CI - see http://www.cakesolutions.net/teamblogs/publishing-artefacts-to-oss-sonatype-nexus-using-sbt-and-travis-ci
  credentials ++= (for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
)
