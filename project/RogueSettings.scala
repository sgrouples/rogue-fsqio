// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.
// Copyright 2015 Sgrouples Inc. All Rights Reserved.

import sbt._
import Keys.{scalaVersion, _}

object RogueSettings {

  val nexus = "https://nexus.groupl.es/"
  val nexusReleases = "releases" at nexus+"repository/maven-releases/"
  val nexusSnapshots = "snapshots" at nexus+"repository/maven-snapshots/"

  def priorTo2_13(scalaVersion: String): Boolean =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, minor)) if minor < 13 => true
      case _                              => false
    }

  val paradiseVersion = "2.1.1"

  lazy val macroSettings: Seq[Setting[_]] = Seq(
    libraryDependencies ++= Seq(
      scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided,
      scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided
    ) ++ (
      if (priorTo2_13(scalaVersion.value)) {
        Seq(
          compilerPlugin(("org.scalamacros" % "paradise" % paradiseVersion).cross(CrossVersion.patch))
        )
      } else Nil
      ),
    scalacOptions ++= (
      if (priorTo2_13(scalaVersion.value)) Nil else Seq("-Ymacro-annotations")
      )
  )

  lazy val defaultSettings: Seq[Setting[_]] = Seq(
    commands += Command.single("testOnlyUntilFailed") { (state, param) =>
      s"testOnly $param" :: s"testOnlyUntilFailed $param" :: state
    },
    version := "5.4.0",
    organization := "me.sgrouples",
    scalaVersion := "2.13.1",
    crossScalaVersions := Seq("2.12.10", "2.13.1"),
    isSnapshot := true,
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    publishTo := version { v =>
      if (v.endsWith("-SNAPSHOT"))
        Some(nexusSnapshots)
      else
        Some(nexusReleases)
    }.value,
    fork in Test := true,
    logBuffered in Test := false,
    parallelExecution in Test := false,
    resolvers ++= Seq(nexusReleases, nexusSnapshots),
    scalacOptions ++= Seq("-deprecation", "-unchecked"), //"-Ymacro-debug-lite"), //, "-Xlog-implicit-conversions"),
    scalacOptions ++= Seq("-feature", "-language:_"),
    credentials += Credentials(Path.userHome / ".ivy2" / ".meweCredentials") ,
    testOptions in Test ++= Seq(Tests.Setup(() => MongoEmbedded.start), Tests.Cleanup(()=> MongoEmbedded.stop))
	) ++ macroSettings
}

object RogueDependencies {
  val specsVer = "4.8.1"
  val mongoVer = "3.12.1"
  val mongoReactiveVer = "1.13.0"

  val joda = Seq(
    "joda-time"                % "joda-time"           % "2.10.1"        % "compile",
    "org.joda"                 % "joda-convert"        % "2.2.0"        % "compile"
  )

  val bosnDeps = Seq("org.mongodb" %  "bson" % mongoVer % "compile")

  val mongoDeps = Seq(
    "org.mongodb"              % "mongodb-driver"       % mongoVer     % "compile",
    "org.mongodb"              % "mongodb-driver-async" % mongoVer    % "compile",
    "org.mongodb"              % "mongodb-driver-reactivestreams" % mongoReactiveVer    % "compile"
  )

  val testDeps = Seq(
    "junit"                    % "junit"               % "4.5"        % "test",
    "org.specs2"              %% "specs2-core"              % specsVer % "test",
    "org.specs2"              %% "specs2-matcher"              % specsVer % "test",
    "org.specs2"              %% "specs2-junit"              % specsVer % "test",
    "org.scalatest" %% "scalatest" % "3.0.8" % "test",
    "com.novocode" % "junit-interface" % "0.11" % "test",
    "org.slf4j" % "slf4j-simple" % "1.7.26" % "test"
  )

  val shapeless = "com.chuusai" %% "shapeless" % "2.3.3"

  val collectionCompat = "org.scala-lang.modules" %% "scala-collection-compat" % "2.1.3"

  val coreDeps = mongoDeps ++ joda

  val ccDeps = mongoDeps ++ Seq(shapeless, collectionCompat)  ++ testDeps
}
