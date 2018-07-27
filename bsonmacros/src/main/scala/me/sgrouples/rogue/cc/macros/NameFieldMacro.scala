package me.sgrouples.rogue.cc.macros

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.annotation.compileTimeOnly
import scala.reflect.macros.whitebox

@compileTimeOnly("f annotation requires macros paradise plugin")
final class f extends StaticAnnotation {
  throw new AssertionError("enableIf.scala requires macro paradise plugin")

  def macroTransform(annottees: Any*): Any = macro GenFieldName.impl

}

object GenFieldName {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val result = annottees.map(_.tree).toList match {
      case q"$mods val $pat = $expr" :: Nil =>
        val fname = Constant(pat.toString())
        println(s"Expr ${expr} , fname ${fname}")
        val res = q"$mods val $pat = $expr($fname)"
        println(s"res ${res}")
        res
      case _ => c.abort(c.enclosingPosition, "@f macro can only annotate val in cc meta")
    }
    c.Expr[Any](result)
  }
}