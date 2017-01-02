// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.
import sbt._
import Keys._

object RogueSettings {
  val nexus = "https://nexus.groupl.es/"
  val nexusReleases = "releases" at nexus+"repository/maven-releases/"
  val nexusSnapshots = "snapshots" at nexus+"repository/maven-snapshots/"
	
  lazy val defaultSettings: Seq[Setting[_]] = Seq(
    version := "2.5.1-MongoAsync-shapeless-28",
    organization := "io.fsq",
    scalaVersion := "2.11.8",
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    publishTo <<= version { v =>
      if (v.endsWith("-SNAPSHOT"))
        Some(nexusSnapshots)
      else
        Some(nexusReleases)
    },
   
    resolvers ++= Seq(nexusReleases, nexusSnapshots),
    scalacOptions ++= Seq("-deprecation", "-unchecked"), //, "-Xlog-implicit-conversions"),
    scalacOptions <++= scalaVersion map { scalaVersion =>
        scalaVersion.split('.') match {
            case Array(major, minor, _*) if major.toInt >= 2 && minor.toInt >= 10 => Seq("-feature", "-language:_")
            case _ => Seq()
        }
    },
    credentials += Credentials(Path.userHome / ".ivy2" / ".meweCredentials") ,
    testOptions in Test ++= Seq(Tests.Setup(() => MongoEmbedded.start), Tests.Cleanup(()=>MongoEmbedded.stop))
	)
}

object RogueDependencies {
  val liftVersion = "2.6.2-MongoAsync-9"
  val specsVer = "3.8.4"
  val liftDeps = Seq(
    "net.liftweb"              %% "lift-mongodb"    % liftVersion  % "compile" intransitive(),
    "net.liftweb"              %% "lift-common"     % liftVersion  % "compile",
    "net.liftweb"              %% "lift-json"       % liftVersion  % "compile",
    "net.liftweb"              %% "lift-util"       % liftVersion  % "compile"
  )
  
  val liftRecordDeps = Seq(
  "net.liftweb"              %% "lift-record"         % liftVersion  % "compile" intransitive(),
  "net.liftweb"              %% "lift-mongodb-record" % liftVersion  % "compile" intransitive(),
  "net.liftweb"              %% "lift-webkit"         % liftVersion  % "compile" intransitive()
  )
  
  
  val joda = Seq(
    "joda-time"                % "joda-time"           % "2.9.4"        % "compile",
    "org.joda"                 % "joda-convert"        % "1.8.1"        % "compile"
  )
  val mongoDeps = Seq(
    "org.mongodb"              % "mongodb-driver"      % "3.4.1"     % "compile",
    "org.mongodb"              % "mongodb-driver-async"% "3.4.1"     % "compile"
  )

  val testDeps = Seq(
    "junit"                    % "junit"               % "4.5"        % "test",
    "org.specs2"              %% "specs2-core"              % specsVer % "test",
    "org.specs2"              %% "specs2-matcher"              % specsVer % "test",
    "org.specs2"              %% "specs2-junit"              % specsVer % "test",
    "org.scalatest" %% "scalatest" % "3.0.0" % "test",
    "com.novocode" % "junit-interface" % "0.11" % "test",
    "org.slf4j" % "slf4j-simple" % "1.7.21" % "test"
  )

  val shapeless = "com.chuusai" %% "shapeless" % "2.3.1"

  val coreDeps = mongoDeps ++ joda
  
  val rogueLiftDeps = mongoDeps ++ joda ++ liftDeps ++ liftRecordDeps

  val ccDeps = mongoDeps ++ Seq(shapeless)  ++ testDeps
	
}
