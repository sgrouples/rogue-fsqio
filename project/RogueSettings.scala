// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.
// Copyright 2015 Sgrouples Inc. All Rights Reserved.

import sbt._
import Keys.{scalaVersion, _}
//import scalafix.sbt.ScalafixPlugin.autoImport.{scalafixScalaBinaryVersion, scalafixSemanticdb}
import sbtghpackages.GitHubPackagesPlugin.autoImport._
import com.github.sbt.git.GitPlugin.autoImport._

object RogueSettings {

  lazy val macroSettings: Seq[Setting[_]] = Seq(
    /*libraryDependencies ++= Seq(
      scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided,
      scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided
    ),
    scalacOptions ++= Seq("-Ymacro-annotations")*/
  )

  lazy val defaultSettings: Seq[Setting[_]] = Seq(
    commands += Command.single("testOnlyUntilFailed") { (state, param) =>
      s"testOnly $param" :: s"testOnlyUntilFailed $param" :: state
    },
    githubOwner := "Sgrouples",
    githubRepository := "rogue-fsqio",
    githubTokenSource := TokenSource.Or(
      TokenSource.Environment("GITHUB_TOKEN"),
      TokenSource.Environment("SHELL")
    ),
    git.useGitDescribe := true,
    git.gitDescribePatterns := Seq("v*"),
    versionScheme := Some("strict"),
    organization := "me.sgrouples",
    scalaVersion := "3.2.1", //2.13.8",
    isSnapshot := false,
    publishMavenStyle := true,
    //publishMavenStyle := true,
    Test / publishArtifact := false,
    pomIncludeRepository := { _ => false },
    Test / fork := true,
    Test / logBuffered := false,
    Test / parallelExecution := false,
    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked",
      "-explain",
      "-feature",
      "-language:implicitConversions"
    )
    //resolvers ++= Seq(nexusReleases, nexusSnapshots),
    //, "-P:semanticdb:synthetics:on"), //"-Ymacro-debug-lite"), //, "-Xlog-implicit-conversions"),
    //scalacOptions ++= Seq("-feature", "-language:_", "-rewrite", "-source:3.0-migration"),
    //semanticdbVersion := scalafixSemanticdb.revision,
    //scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value),
  ) ++ macroSettings
}

object RogueDependencies {
  val mongoVer = "4.9.0"
  val nettyVer = "4.1.86.Final"
  val testcontainersScalaVersion = "0.40.12"

  val bsonDeps = Seq("org.mongodb" % "bson" % mongoVer % Compile)

  val mongoDeps = Seq(
    "org.mongodb.scala" %% "mongo-scala-driver" % mongoVer % Compile cross (CrossVersion.for3Use2_13)
  )

  val testDeps = Seq(
    "org.slf4j" % "slf4j-simple" % "1.7.36" % Test,
    "org.scalameta" %% "munit" % "1.0.0-M7" % Test,
    "io.netty" % "netty-all" % nettyVer % Test,
    "io.netty" % "netty-transport-native-epoll" % nettyVer % Test,
    "io.netty" % "netty-transport-native-unix-common" % nettyVer % Test,
    "com.dimafeng" %% "testcontainers-scala-mongodb" % testcontainersScalaVersion % Test
  )

  val enumeratum = "com.beachape" %% "enumeratum" % "1.7.2"
  val tagging = "com.softwaremill.common" %% "tagging" % "2.3.3"
  val coreDeps = mongoDeps ++ Seq(enumeratum)

  val ccDeps = mongoDeps ++ Seq(tagging) ++ testDeps
}
