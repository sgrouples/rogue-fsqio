addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

import RogueSettings._

Seq(RogueSettings.defaultSettings: _*)

lazy val field = (project in file("field")).settings(defaultSettings)
lazy val bsonformats = (project in file("bsonformats")).settings(defaultSettings)
lazy val index = (project in file("index")).settings(defaultSettings).dependsOn(field)
lazy val core = (project in file("core")).settings(defaultSettings).dependsOn(field, index % "compile;test->test;runtime->runtime")
lazy val indexchecker = (project in file("indexchecker")).settings(defaultSettings).dependsOn(core)
lazy val lift = (project in file("lift")).settings(defaultSettings).dependsOn(field, indexchecker, core % "compile;test->test;runtime->runtime")
lazy val bsonmacros = (project in file("bsonmacros")).dependsOn(bsonformats).settings(defaultSettings)
lazy val bsonshapeless = (project in file("bsonshapeless")).dependsOn(bsonformats).settings(defaultSettings)
lazy val cc = (project in file("cc")).dependsOn(bsonformats,field, core, bsonmacros, bsonshapeless).settings(defaultSettings)
lazy val root = (project in file(".")).settings(defaultSettings).aggregate(field,core,index,indexchecker,lift,cc)