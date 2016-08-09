package me.sgrouples.rogue.naming

/**
  * Created by mwielocha on 09/08/16.
  */
sealed trait NamingStrategy {

  def apply[T : Manifest]: String

}

class ClassManifestNamingStrategy(format: String => String) extends NamingStrategy {
  override def apply[T: Manifest]: String =
    format(implicitly[Manifest[T]].runtimeClass.getSimpleName)
}

object LowerCaseNamingStrategy extends ClassManifestNamingStrategy(_.toLowerCase())
object UpperCaseNamingStrategy extends ClassManifestNamingStrategy(_.toUpperCase())
object SnakeCaseNamingStrategy extends ClassManifestNamingStrategy(
  _.replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
    .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
    .toLowerCase
)



