package me.sgrouples.rogue.cc

import scala.language.experimental.macros
import scala.reflect.macros.blackbox._
import shapeless.tag
import shapeless.tag._

trait MacroNamesResolver[T] extends NamesResolver {
  override def named[T <: io.fsq.field.Field[_, _]](name: String)(func: String => T): T @@ Marker
  override def named[T <: io.fsq.field.Field[_, _]](func: String => T): T @@ Marker
}

object MacroCC {
  implicit def gen[T]: MacroNamesResolver[T] = macro MacroCCGenerator.genImpl[T]
}

/*
  case term if term.isAccessor =>
            term.getter match {
              case getterSymbol if getterSymbol.isMethod =>
                returnsMarkedField(getterSymbol.asMethod)
 */

class MacroCCGenerator(val c: Context) {
  import c.universe._
  def genImpl[T: c.WeakTypeTag]: c.Tree = {
    val idx = 0
    val tpe = weakTypeOf[T]
    val name = c.freshName()
    val members = tpe.members
    val terms = members.flatMap { symbol =>
      if (symbol.isTerm) {
        val term = symbol.asTerm
        if (term.isAccessor && term.getter.isMethod) {
          Some(term.name)
        } else None
      } else None
    }
    val sortedNames = terms.toSeq.sorted
    println(s"terms ${terms}")
    q"""new MacroNamesResolver[$tpe] {
      val names =
      
      )
      """
  }
}
