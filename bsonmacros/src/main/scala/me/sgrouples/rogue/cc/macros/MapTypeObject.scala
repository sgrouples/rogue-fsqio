package me.sgrouples.rogue.cc.macros
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class MapTypeObject(val c: Context) {
  import c.universe._
  val mapTypeSymbol = typeOf[Map[_, _]].typeSymbol
}
