import RogueSettings._
import RogueDependencies._

Seq(RogueSettings.defaultSettings: _*)

lazy val field = (project in file("field")).settings(defaultSettings)
lazy val index = (project in file("index")).settings(defaultSettings).dependsOn(field)
lazy val core = (project in file("core")).settings(defaultSettings).dependsOn(field, index)
lazy val indexchecker = (project in file("indexchecker")).settings(defaultSettings).dependsOn(core)
lazy val lift = (project in file("lift")).settings(defaultSettings).dependsOn(field, core)
lazy val spindle = (project in file("spindle")).settings(defaultSettings).dependsOn(core)

lazy val root = (project in file(".")).settings(defaultSettings).aggregate(field,core,index,indexchecker,lift,spindle)