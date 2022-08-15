import RogueSettings._
Seq(RogueSettings.defaultSettings: _*)

scalaVersion := "3.1.3"

lazy val field = (project in file("field")).settings(defaultSettings)
lazy val bsonformats = (project in file("bsonformats")).settings(defaultSettings ++ Seq(
  name := "rogue-bsonformats",
  libraryDependencies ++= RogueDependencies.bosnDeps
))
lazy val index = (project in file("index")).settings(defaultSettings ++ Seq(
  name := "rogue-index",
  libraryDependencies ++= RogueDependencies.coreDeps ++ RogueDependencies.testDeps
)).dependsOn(field)
lazy val core = (project in file("core")).settings(defaultSettings ++ Seq(
  name := "rogue-core",
  libraryDependencies ++= RogueDependencies.coreDeps ++ RogueDependencies.testDeps
)).dependsOn(field, index % "compile;test->test;runtime->runtime")
lazy val indexchecker = (project in file("indexchecker")).settings(defaultSettings).dependsOn(core)
lazy val bsonmacros = (project in file("bsonmacros")).dependsOn(bsonformats).settings(defaultSettings ++ Seq(
  name := "rogue-bson-macros",
  libraryDependencies ++=
    RogueDependencies.mongoDeps ++
      RogueDependencies.testDeps //++
      //Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
))
/*lazy val bsonshapeless = (project in file("bsonshapeless")).dependsOn(bsonformats).settings(defaultSettings ++ Seq(
  name := "rogue-fsqio-bson-shapeless",
  libraryDependencies ++= RogueDependencies.ccDeps// ++
    //Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
))*/
lazy val cc = (project in file("cc")).dependsOn(bsonformats,field, core, bsonmacros /*, bsonshapeless */)
  .settings(defaultSettings ++ Seq(
    name := "rogue-shapeless",
    libraryDependencies ++= RogueDependencies.ccDeps //++
      //Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
  ))
lazy val root = (project in file("."))
  .settings(defaultSettings)
  .aggregate(field,core,index,indexchecker,cc,bsonmacros,/*bsonshapeless,*/ bsonformats)
