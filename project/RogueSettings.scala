// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.
import sbt._
import Keys.{scalaVersion, _}

object RogueSettings {

  val nexus = "https://nexus.groupl.es/"
  val nexusReleases = "releases" at nexus+"repository/maven-releases/"
  val nexusSnapshots = "snapshots" at nexus+"repository/maven-snapshots/"

  lazy val defaultSettings: Seq[Setting[_]] = Seq(
    commands += Command.single("testOnlyUntilFailed") { (state, param) =>
      s"testOnly $param" :: s"testOnlyUntilFailed $param" :: state
    },
    version := "5.0.2", //before 5.0.0 with streams
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
    fork in Test := true,
    logBuffered in Test := false,
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
  val mongoVer = "3.11.0"
  val mongoReactiveVer = "1.12.0"

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

  val coreDeps = mongoDeps ++ joda

  val ccDeps = mongoDeps ++ Seq(shapeless)  ++ testDeps
}
