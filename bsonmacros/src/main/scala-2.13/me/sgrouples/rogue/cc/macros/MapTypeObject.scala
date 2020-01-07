package me.sgrouples.rogue.cc.macros
import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

class MapTypeObject(val c: Context) {
  import c.universe._
  val mapTypeSymbol = typeOf[Map[_, _]].typeSymbol
}
