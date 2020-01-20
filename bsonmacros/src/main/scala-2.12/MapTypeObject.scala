package me.sgrouples.rogue.cc.macros
import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros
import scala.collection.MapLike

class MapTypeObject(val c: Context) {
  import c.universe._
  val mapTypeSymbol = typeOf[MapLike[_, _, _]].typeSymbol
}
