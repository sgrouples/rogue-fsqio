package me.sgrouples.rogue.macros

import scala.reflect.macros.blackbox

sealed trait Marker

object FieldFinder {

  def find[M]: Map[Int, String] = macro find_impl[M]

  def find_impl[M](c: blackbox.Context)(implicit tt: c.WeakTypeTag[M]): c.Expr[Map[Int, String]] = {
    import c.universe._

    println(c.typecheck(c.prefix.tree))

    /*
      The idea of automatic name resolution is taken from Scala's Enumeration,
      but imlemented without falling back to Java's reflection api.
     */

    val decode: Symbol => String = _.name.decodedName.toString.trim

    // we are looking for vals accessible from io.fsq.field.Field[_, _] and tagged with Marker

    def returnsMarkedField(symbol: Symbol): Boolean = {

      val typeArgs = symbol.asMethod.returnType.typeArgs

      typeArgs.exists(_ =:= weakTypeOf[Marker]) &&
        typeArgs.exists(_ <:< weakTypeOf[io.fsq.field.Field[_, _]])
    }

    /*
      So it seems rewrite of traits encoding in Scala 2.12
      was a good reason to simplify this little piece of code.
      I think this time its self-explanatory.
    */

    val valuesOfMarkedFieldTypeOnly: Symbol => Boolean = {
      case symbol if symbol.isTerm =>
        symbol.asTerm match {
          case term if term.isVal =>
            term.getter match {
              case getterSymbol if getterSymbol.isMethod =>
                returnsMarkedField(getterSymbol.asMethod)
              case _ => false
            }
          case _ => false
        }
      case _ => false
    }

    /*
      This reverse here is because traits are initialized in opposite order than declared...
     */

    val values: Map[Int, String] = {
      weakTypeOf[M].baseClasses.reverse.flatMap {
        baseClass =>
          appliedType(baseClass).decls
            .sorted.filter(valuesOfMarkedFieldTypeOnly)
      }.map(decode).zipWithIndex.map(_.swap)(scala.collection.breakOut)
    }

    // name map in order of trait linearization

    c.Expr[Map[Int, String]](q"""$values""")
  }
}
