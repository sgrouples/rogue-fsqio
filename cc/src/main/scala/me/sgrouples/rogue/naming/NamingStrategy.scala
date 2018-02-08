package me.sgrouples.rogue.naming

import scala.reflect.ClassTag

/**
 * Created by mwielocha on 09/08/16.
 */
sealed trait NamingStrategy {

  def apply[T: ClassTag]: String

}

class ClassNamingStrategy(format: Class[_] => String) extends NamingStrategy {
  override def apply[T: ClassTag]: String =
    format(implicitly[ClassTag[T]].runtimeClass)
}

object LowerCase extends ClassNamingStrategy(_.getSimpleName.toLowerCase())
object PluralLowerCase extends ClassNamingStrategy(_.getSimpleName.toLowerCase() + "s")
object UpperCase extends ClassNamingStrategy(_.getSimpleName.toUpperCase())
object SnakeCase extends ClassNamingStrategy(
  _.getSimpleName.replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
    .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
    .toLowerCase)

