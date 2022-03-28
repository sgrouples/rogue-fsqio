// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.
// Copyright 2015 Sgrouples Inc. All Rights Reserved.

import sbt._
import Keys.{scalaVersion, _}
import scalafix.sbt.ScalafixPlugin.autoImport.{scalafixScalaBinaryVersion, scalafixSemanticdb}

object RogueSettings {

  val nexus = "https://nexus.groupl.es/"
  val nexusReleases = "releases" at nexus+"repository/maven-releases/"
  val nexusSnapshots = "snapshots" at nexus+"repository/maven-snapshots/"

  lazy val macroSettings: Seq[Setting[_]] = Seq(
    libraryDependencies ++= Seq(
      scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided,
      scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided
    ),
    scalacOptions ++= Seq("-Ymacro-annotations")
  )

  lazy val defaultSettings: Seq[Setting[_]] = Seq(
    commands += Command.single("testOnlyUntilFailed") { (state, param) =>
      s"testOnly $param" :: s"testOnlyUntilFailed $param" :: state
    },
    version := "7.0.0-SNAPSHOT",
    organization := "me.sgrouples",
    scalaVersion := "2.13.8",
    isSnapshot := false,
    publishMavenStyle := true,
    Test / publishArtifact := false,
    pomIncludeRepository := { _ => false },
    publishTo := version { v =>
      if (v.endsWith("-SNAPSHOT"))
        Some(nexusSnapshots)
      else
        Some(nexusReleases)
    }.value,
    Test / fork := true,
    Test / logBuffered := false,
    Test / parallelExecution := false,
    resolvers ++= Seq(nexusReleases, nexusSnapshots),
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-Yrangepos"), //, "-Ymacro-debug-lite"),
    //, "-P:semanticdb:synthetics:on"), //"-Ymacro-debug-lite"), //, "-Xlog-implicit-conversions"),
    scalacOptions ++= Seq("-feature", "-language:_", "-Xsource:3"),
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value),
    credentials += Credentials(Path.userHome / ".ivy2" / ".meweCredentials") ,
    Test / testOptions ++= Seq(Tests.Setup(() => MongoEmbedded.start), Tests.Cleanup(()=> MongoEmbedded.stop))
	) ++ macroSettings
}

object RogueDependencies {
  val mongoVer = "4.5.1"
  val nettyVer = "4.1.74.Final"


  val bosnDeps = Seq("org.mongodb" %  "bson" % mongoVer % Compile)

  val mongoDeps = Seq(
    "org.mongodb.scala" %% "mongo-scala-driver" % mongoVer    % Compile
  )

  val testDeps = Seq(
    "org.slf4j" % "slf4j-simple" % "1.7.36" % Test,
    "org.scalameta" %% "munit" % "0.7.29" % Test,
    "io.netty" % "netty-all" % nettyVer % Test,
    "io.netty" % "netty-transport-native-epoll" % nettyVer % Test,
    "io.netty" % "netty-transport-native-unix-common" % nettyVer % Test
  )

  val shapeless = "com.chuusai" %% "shapeless" % "2.3.8"

  val coreDeps = mongoDeps

  val ccDeps = mongoDeps ++ Seq(shapeless)  ++ testDeps

}
