package me.sgrouples.rogue.cc.macros

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros
import org.bson.types.ObjectId
import java.util.UUID

import scala.collection.Seq
import scala.annotation.implicitNotFound
import scala.collection.{ GenSeqLike, MapLike }

@implicitNotFound("MacroGen can't generate for ${T}")
trait MacroGen[T] {
  def namesMap(): Vector[(Int, String)]
}

trait BlaDef {
  implicit object BlaInt extends MacroGen[Int] {
    override def namesMap(): Vector[(Int, String)] = Vector.empty
  }
  implicit object BlaString extends MacroGen[String] {
    override def namesMap(): Vector[(Int, String)] = Vector.empty

  }
}
object BlaDef extends BlaDef

object MacroCC {
  implicit def gen[T]: MacroGen[T] = macro MacroCCGenerator.genImpl[T]
}

class MacroCCGenerator(val c: Context) {
  import c.universe._
  import definitions.ArrayClass
  def genImpl[T: c.WeakTypeTag]: c.Tree = {
    val tpe = weakTypeOf[T]
    val members = tpe.decls
    val mapTypeSymbol = typeOf[MapLike[_, _, _]].typeSymbol
    val iterableTypeSymbol = typeOf[Iterable[_]].typeSymbol
    //val enumTypeSymbol = typeOf[Value]

    val ctorOpt = members.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.headOption
    ctorOpt.map { ctor =>
      println(s"Main ctor ${ctor}")
      //ctor fields
      val fields = ctor.paramLists.head

      val companion: c.universe.Symbol = tpe.typeSymbol.companion

      if (companion == NoSymbol) {
        c.error(c.enclosingPosition, s"Companion symbol not found for case class ${tpe}. Can't find companion for inner classes")
      }

      val defaults = fields.map(_.asTerm).zipWithIndex.map {
        case (p, i) =>
          if (!p.isParamWithDefault) None
          else {
            val getterName = TermName("apply$default$" + (i + 1))
            Some(q"$companion.$getterName")
          }
      }
      println(s"Defaults are ${defaults}")

      println(s"FLDS ${fields}")
      val f = fields.map { f =>
        val name = f.name
        val decoded = name.decodedName
        val retType = f.typeSignature
        //tpe.decl(name).typeSignature.finalResultType
        println(s"F ${name} / ${decoded} / ${retType}")

        //val x = q"BsonRW[c.typeOf[$retType]]"
        //println(s"x ${x}")
        //x
        val tp = f.typeSignature
        val at = appliedType(tp, tp.typeArgs)
        println(s"Base ${at.baseClasses}")
        //primitives

        if (at <:< typeOf[Int]) {
          println(s"Int member ${name}")
        } else if (at <:< typeOf[Long]) {
          println(s"Long member ${name}")
        } else if (at <:< typeOf[Boolean]) {
          println(s"Boolean member ${name}")
        } else if (at <:< typeOf[Char]) {
          println(s"Char member ${name}")
        } else if (at <:< typeOf[Short]) {
          println(s"Short member ${name}")
        } else if (at <:< typeOf[Double]) {
          println(s"Double member ${name}")
        } else if (at <:< typeOf[Float]) {
          println(s"Double member ${name}")
        } else if (at <:< typeOf[String]) {
          println(s"String member ${name}")
        } else if (tp.typeConstructor == typeOf[Option[_]].typeConstructor) {
          println(s"Option type constructor ${name}")
        } else if (tp.typeConstructor == typeOf[Array[_]].typeConstructor) {
          //Array[Byte] is special
          println(s"Array type constructor ${name}")
        } else if (at <:< typeOf[org.bson.types.ObjectId]) {
          println(s"ObjectId subtype ${name}")
        } else if (at <:< typeOf[java.util.UUID]) {
          println(s"UUID subtype ${name}")
        } else if (at.baseClasses.contains(mapTypeSymbol)) {
          if (!(tp.typeArgs.head <:< typeOf[String])) {
            c.error(c.enclosingPosition, msg = s"Map key must be of type String for case class ${tpe}.${name}")
          }
          println(s"Map type ${name}")
        } else if (at.baseClasses.contains(iterableTypeSymbol)) {
          println(s"Iterable like - ${name}")
        } else {
          //enum
          println(s"Other - ... ${name} / ${at} / ${tp.typeConstructor}")
          //Option[
          //arrays
          //Seq
          //MapLike
        }
        //val s = q"implicitly[Bla[$tp]]"
        val t = tq"_root_.me.sgrouples.rogue.cc.macros.MacroGen[$tp]"
        println(s"T ${t} type ${t.tpe} , ${at}")
        //val checkedType = c.typecheck(t)
        //println(s"Serching for implicit value for ${checkedType}")
        val s1 = c.inferImplicitValue(at)
        println(s"s1 ${s1}")
        //s1
        "a"
      }
    }
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