import RogueSettings._

Seq(RogueSettings.defaultSettings: _*)

lazy val field = (project in file("field")).settings(defaultSettings)
lazy val bsonformats = (project in file("bsonformats")).settings(defaultSettings)
lazy val index = (project in file("index")).settings(defaultSettings).dependsOn(field)
lazy val core = (project in file("core")).settings(defaultSettings).dependsOn(field, index % "compile;test->test;runtime->runtime")
lazy val indexchecker = (project in file("indexchecker")).settings(defaultSettings).dependsOn(core)
lazy val lift = (project in file("lift")).settings(defaultSettings).dependsOn(field, indexchecker, core % "compile;test->test;runtime->runtime")
lazy val ccmacros = (project in file("ccmacros")).dependsOn(bsonformats).settings(defaultSettings)
lazy val cc = (project in file("cc")).dependsOn(bsonformats,field, core, ccmacros).settings(defaultSettings)
lazy val root = (project in file(".")).settings(defaultSettings).aggregate(field,core,index,indexchecker,lift,cc)