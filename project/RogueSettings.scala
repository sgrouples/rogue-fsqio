// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.
// Copyright 2015 Sgrouples Inc. All Rights Reserved.

import sbt._
import Keys.{scalaVersion, _}

object RogueSettings {

  val nexus = "https://nexus.groupl.es/"
  val nexusReleases = "releases" at nexus+"repository/maven-releases/"
  val nexusSnapshots = "snapshots" at nexus+"repository/maven-snapshots/"

  lazy val macroSettings: Seq[Setting[_]] = Seq(
    libraryDependencies ++= Seq(
      scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided,
      scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided
    )
  )

  lazy val defaultSettings: Seq[Setting[_]] = Seq(
    commands += Command.single("testOnlyUntilFailed") { (state, param) =>
      s"testOnly $param" :: s"testOnlyUntilFailed $param" :: state
    },
    version := "5.3.0",
    organization := "me.sgrouples",
    scalaVersion := "2.13.6",
    crossScalaVersions := Seq("2.13.6", "3.0.0"),
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
  val mongoVer = "4.2.3"

  val joda = Seq(
    "joda-time"                % "joda-time"           % "2.10.1"        % Compile,
    "org.joda"                 % "joda-convert"        % "2.2.0"        % Compile
  )

  val bosnDeps = Seq("org.mongodb" %  "bson" % mongoVer % Compile)

  val mongoDeps = Seq(
   // "org.mongodb"              % "mongodb-driver-sync"       % mongoVer     % Compile,
    "org.mongodb.scala"        %% "mongo-scala-driver" % mongoVer % Compile,
    "org.mongodb"              % "mongodb-driver-reactivestreams" % mongoVer   % Compile
  )

  val testDeps = Seq(
    "junit"                    % "junit"               % "4.5"        % Test,
    "org.specs2"              %% "specs2-core"              % specsVer % Test,
    "org.specs2"              %% "specs2-matcher"              % specsVer % Test,
    "org.specs2"              %% "specs2-junit"              % specsVer % Test,
    "org.scalatest" %% "scalatest" % "3.0.8" % Test,
    "org.slf4j" % "slf4j-simple" % "1.7.30" % Test
  )

  val shapeless = "com.chuusai" %% "shapeless" % "2.3.7"

  val collectionCompat = "org.scala-lang.modules" %% "scala-collection-compat" % "2.1.3"

  val coreDeps = mongoDeps ++ joda

  val ccDeps = mongoDeps ++ Seq(shapeless, collectionCompat)  ++ testDeps
}
