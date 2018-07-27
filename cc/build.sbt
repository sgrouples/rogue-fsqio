addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
libraryDependencies ++= RogueDependencies.ccDeps ++ Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)

organization := "me.sgrouples"
name := "rogue-shapeless"
