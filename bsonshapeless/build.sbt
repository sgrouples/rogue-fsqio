libraryDependencies ++= RogueDependencies.ccDeps ++
  Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)

organization := "me.sgrouples"
name := "rogue-fsqio-bson-shapeless"
/*unmanagedSourceDirectories in Compile += {
  val sourceDir = (sourceDirectory in Compile).value
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, n)) if n >= 13 => sourceDir / "scala-2.13+"
    case _                       => sourceDir / "scala-2.12"
  }
}*/