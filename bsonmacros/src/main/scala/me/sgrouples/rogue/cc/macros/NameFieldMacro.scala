package me.sgrouples.rogue.cc.macros

import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.language.experimental.macros
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
      case q"$mods val $pat = ClassField[..$tpt]($expr)" :: Nil =>
        val fname = Constant(pat.toString())
        q"$mods val $pat = ClassField[..$tpt]($fname, $expr)"

      case q"$mods val $pat = OptClassField[..$tpt]($expr)" :: Nil =>
        val fname = Constant(pat.toString())
        q"$mods val $pat = OptClassField[..$tpt]($fname, $expr)"

      case q"$mods val $pat = ClassRequiredField[..$tpt]($e1, $e2)" :: Nil =>
        val fname = Constant(pat.toString())
        q"$mods val $pat = ClassRequiredField[..$tpt]($fname, $e1, $e2)"

      case q"$mods val $pat = ClassListField[..$tpt]($expr)" :: Nil =>
        val fname = Constant(pat.toString())
        q"$mods val $pat = ClassListField[..$tpt]($fname, $expr)"

      case q"$mods val $pat = OptClassListField[..$tpt]($expr)" :: Nil =>
        val fname = Constant(pat.toString())
        q"$mods val $pat = OptClassListField[..$tpt]($fname, $expr)"

      case q"$mods val $pat = ClassArrayField[..$tpt]($expr)" :: Nil =>
        val fname = Constant(pat.toString())
        q"$mods val $pat = ClassArrayField[..$tpt]($fname, $expr)"

      case q"$mods val $pat = OptClassArrayField[..$tpt]($expr)" :: Nil =>
        val fname = Constant(pat.toString())
        q"$mods val $pat = OptClassArrayField[..$tpt]($fname, $expr)"

      case q"$mods val $pat = EnumField($expr)" :: Nil =>
        val fname = Constant(pat.toString())
        q"$mods val $pat = EnumField($fname, $expr)"

      case q"$mods val $pat = OptEnumField($expr)" :: Nil =>
        val fname = Constant(pat.toString())
        q"$mods val $pat = OptEnumField($fname, $expr)"

      case q"$mods val $pat = EnumIdField($expr)" :: Nil =>
        val fname = Constant(pat.toString())
        q"$mods val $pat = EnumIdField($fname, $expr)"

      case q"$mods val $pat = OptEnumIdField($expr)" :: Nil =>
        val fname = Constant(pat.toString())
        q"$mods val $pat = OptEnumIdField($fname, $expr)"

      case q"$mods val $pat = $expr($exp1)" :: Nil =>
        val r = q"$mods val $pat = $expr($exp1)"
        //println(s"No change - let's assume we have name - ${r}")
        r

      case q"$mods val $pat = $expr" :: Nil =>
        val fname = Constant(pat.toString())
        //println(s"Expr ${expr} , fname ${fname}")
        val res = q"$mods val $pat = $expr($fname)"
        //println(s"rewrote to ${res}")
        res
      case _ =>
        c.abort(
          c.enclosingPosition,
          "@f macro can only annotate val in cc meta"
        )
    }
    c.Expr[Any](result)
  }
}
