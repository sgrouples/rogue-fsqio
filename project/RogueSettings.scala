// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.
import sbt._
import Keys.{scalaVersion, _}

object RogueSettings {

  val nexus = "https://nexus.groupl.es/"
  val nexusReleases = "releases" at nexus+"repository/maven-releases/"
  val nexusSnapshots = "snapshots" at nexus+"repository/maven-snapshots/"

  lazy val defaultSettings: Seq[Setting[_]] = Seq(
    version := "4.2.1-SNAPSHOT",
    organization := "me.sgrouples",
    scalaVersion := "2.12.8",
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
    parallelExecution in Test := false,
    resolvers ++= Seq(nexusReleases, nexusSnapshots),
    scalacOptions ++= Seq("-deprecation", "-unchecked"), //"-Ymacro-debug-lite"), //, "-Xlog-implicit-conversions"),
    scalacOptions ++= Seq("-feature", "-language:_"),
    credentials += Credentials(Path.userHome / ".ivy2" / ".meweCredentials") ,
    testOptions in Test ++= Seq(Tests.Setup(() => MongoEmbedded.start), Tests.Cleanup(()=> MongoEmbedded.stop))
	)
}

object RogueDependencies {
  val specsVer = "3.8.9"
  val mongoVer = "3.8.2"

  val joda = Seq(
    "joda-time"                % "joda-time"           % "2.9.9"        % "compile",
    "org.joda"                 % "joda-convert"        % "1.8.1"        % "compile"
  )

  val bosnDeps = Seq("org.mongodb" %  "bson" % mongoVer % "compile")

  val mongoDeps = Seq(
    "org.mongodb"              % "mongodb-driver"       % mongoVer     % "compile",
    "org.mongodb"              % "mongodb-driver-async" % mongoVer    % "compile"
  )

  val testDeps = Seq(
    "junit"                    % "junit"               % "4.5"        % "test",
    "org.specs2"              %% "specs2-core"              % specsVer % "test",
    "org.specs2"              %% "specs2-matcher"              % specsVer % "test",
    "org.specs2"              %% "specs2-junit"              % specsVer % "test",
    "org.scalatest" %% "scalatest" % "3.0.3" % "test",
    "com.novocode" % "junit-interface" % "0.11" % "test",
    "org.slf4j" % "slf4j-simple" % "1.7.25" % "test"
  )

  val shapeless = "com.chuusai" %% "shapeless" % "2.3.3"

  val coreDeps = mongoDeps ++ joda

  val ccDeps = mongoDeps ++ Seq(shapeless)  ++ testDeps
}
