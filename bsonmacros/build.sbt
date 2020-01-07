libraryDependencies ++=
  RogueDependencies.mongoDeps ++
  RogueDependencies.testDeps ++
  Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value, RogueDependencies.collectionCompat)

organization := "me.sgrouples"
name := "rogue-bson-macros"
