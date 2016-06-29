// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.
import sbt._
import Keys._

object RogueSettings {
  val nexus = "https://nexus.groupl.es/"
  val nexusReleases = "releases" at nexus+"repository/maven-releases/"
  val nexusSnapshots = "snapshots" at nexus+"repository/maven-snapshots/"
	
  lazy val defaultSettings: Seq[Setting[_]] = Seq(
    version := "2.5.1-MongoAsync-11",
    organization := "com.foursquare",
    scalaVersion := "2.11.8",
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    publishTo <<= (version) { v =>
      if (v.endsWith("-SNAPSHOT"))
        Some(nexusSnapshots)
      else
        Some(nexusReleases)
    },
   
    resolvers ++= Seq(nexusReleases, nexusSnapshots),
    scalacOptions ++= Seq("-deprecation", "-unchecked"),
    scalacOptions <++= scalaVersion map { scalaVersion =>
        scalaVersion.split('.') match {
            case Array(major, minor, _*) if major.toInt >= 2 && minor.toInt >= 10 => Seq("-feature", "-language:_")
            case _ => Seq()
        }
    },
    credentials += Credentials(Path.userHome / ".ivy2" / ".meweCredentials")
	)
}

object RogueDependencies {
  val liftVersion = "2.6.2-MongoAsync"
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
    "joda-time"                % "joda-time"           % "2.9.1"        % "provided",
    "org.joda"                 % "joda-convert"        % "1.8.1"        % "provided"
  )
  val mongoDeps = Seq(
    "org.mongodb"              % "mongodb-driver"      % "3.2.2"     % "compile",
    "org.mongodb"              % "mongodb-driver-async"% "3.2.2"     % "compile"
  )
  
  
/*	libraryDependencies <++= (scalaVersion) { scalaVersion =>
	  val specsVersion = "2.4.2"
	  Seq(
	    "com.foursquare"           %% "rogue-field"     % "2.4.0"      % "compile",
	    "junit"                    % "junit"               % "4.5"        % "test",
	    "com.novocode"             % "junit-interface"     % "0.6"        % "test",
	    "ch.qos.logback"           % "logback-classic"     % "1.0.9"     % "provided",
	    "org.specs2"              %% "specs2"              % specsVersion % "test",
	    "org.scala-lang"           % "scala-compiler"      % scalaVersion % "test"
	  )
	}

	Seq(RogueBuild.defaultSettings: _*)
	*/
  val coreDeps = mongoDeps ++ joda ++ liftDeps
  
  val rogueLiftDeps = mongoDeps ++ joda ++ liftDeps ++ liftRecordDeps
	
}
