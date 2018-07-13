package me.sgrouples.rogue.cc.macros

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

trait MacroGen[T] {
  def namesMap(): Vector[(Int, String)]
}

object MacroCC {
  implicit def gen[T]: MacroGen[T] = macro MacroCCGenerator.genImpl[T]
}

class MacroCCGenerator(val c: Context) {
  import c.universe._
  def genImpl[T: c.WeakTypeTag]: c.Tree = {
    val tpe = weakTypeOf[T]
    val members = tpe.members
    val terms = members.flatMap { symbol =>
      if (symbol.isTerm) {
        val term = symbol.asTerm
        if (term.isAccessor && term.getter.isMethod) {
          Some(term.name)
        } else None
      } else None
    }
    println(s"T ${tpe}")
    println(s"names ${terms}")
    val zipNames = terms.zipWithIndex.map {
      case (n, i) =>
        (i -> s"$n")
    }.toVector
    println(s"Name map is ${zipNames}")
    //println(s"terms ${terms}")
    val r = q"""new MacroGen[$tpe] {
                ${zipNames}
                  override def namesMap():Vector[(Int, String)] = ${zipNames}
                }
    """
    println(s"Will return ${r}")
    r
    //c.Expr[MacroNamesResolver[T]](r)
  }
}