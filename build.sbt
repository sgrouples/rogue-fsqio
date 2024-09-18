import RogueSettings._
Seq(RogueSettings.defaultSettings: _*)

scalaVersion := "3.3.0"

lazy val field = (project in file("field")).settings(defaultSettings)
lazy val bsonformats = (project in file("bsonformats")).settings(
  defaultSettings ++ Seq(
    name := "rogue-bsonformats",
    libraryDependencies ++= RogueDependencies.bsonDeps
  )
)
lazy val index = (project in file("index"))
  .settings(
    defaultSettings ++ Seq(
      name := "rogue-index",
      libraryDependencies ++= RogueDependencies.coreDeps ++ RogueDependencies.testDeps
    )
  )
  .dependsOn(field)
lazy val core = (project in file("core"))
  .settings(
    defaultSettings ++ Seq(
      name := "rogue-core",
      libraryDependencies ++= RogueDependencies.coreDeps ++ RogueDependencies.testDeps
    )
  )
  .dependsOn(field, index % "compile;test->test;runtime->runtime")
lazy val indexchecker =
  (project in file("indexchecker")).settings(defaultSettings).dependsOn(core)
lazy val bsonmacros = (project in file("bsonmacros"))
  .dependsOn(bsonformats)
  .settings(
    defaultSettings ++ Seq(
      name := "rogue-bson-macros",
      libraryDependencies ++=
        RogueDependencies.mongoDeps ++
          RogueDependencies.testDeps ++ Seq(RogueDependencies.enumeratum)
    )
  )
lazy val cc = (project in file("cc"))
  .dependsOn(bsonformats, field, core, bsonmacros)
  .settings(
    defaultSettings ++ Seq(
      name := "rogue-shapeless",
      libraryDependencies ++= RogueDependencies.ccDeps
    )
  )
lazy val root = (project in file("."))
  .enablePlugins(GitVersioning)
  .settings(defaultSettings)
  .settings(publish / skip := true)
  .aggregate(
    field,
    core,
    index,
    indexchecker,
    cc,
    bsonmacros,
    bsonformats
  )
