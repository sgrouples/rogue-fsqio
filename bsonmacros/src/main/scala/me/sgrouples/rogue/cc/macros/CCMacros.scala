package me.sgrouples.rogue.cc.macros

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros
import org.bson.types.ObjectId
import java.util.UUID

import me.sgrouples.rogue.EnumSerializeValue

import scala.collection.Seq
import scala.annotation.implicitNotFound
import scala.collection.{ GenSeqLike, MapLike }
import scala.reflect.macros.Universe
//http://www.strongtyped.io/blog/2014/05/23/case-class-related-macros/
//http://imranrashid.com/posts/learning-scala-macros/
/*trait BlaDef {
  implicit object BlaInt extends MacroGen[Int] {
    override def namesMap(): Vector[(Int, String)] = Vector.empty
  }
  implicit object BlaString extends MacroGen[String] {
    override def namesMap(): Vector[(Int, String)] = Vector.empty
  }

}
object BlaDef extends BlaDef
*/
object MacroCC {
  implicit def gen[T]: MacroBsonFormat[T] = macro MacroCCGenerator.genImpl[T]
}

class MacroCCGenerator(val c: Context) {

  import c.universe._

  //copied from https://github.com/lihaoyi/upickle/blob/master/upickle/src/upickle/internal/Macros.scala
  def companionTree(tpe: c.Type): Either[String, Tree] = {
    val companionSymbol = tpe.typeSymbol.companion

    if (companionSymbol == NoSymbol && tpe.typeSymbol.isClass) {
      val clsSymbol = tpe.typeSymbol.asClass
      val msg = "[error] The companion symbol could not be determined for " +
        s"[[${clsSymbol.name}]]. This may be due to a bug in scalac (SI-7567) " +
        "that arises when a case class within a function is upickle. As a " +
        "workaround, move the declaration to the module-level."
      Left(msg)
    } else {
      val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
      val pre = tpe.asInstanceOf[symTab.Type].prefix.asInstanceOf[Type]
      Right(c.universe.internal.gen.mkAttributedRef(pre, companionSymbol))
    }
  }
  //copied from https://github.com/lihaoyi/upickle/blob/master/upickle/src/upickle/internal/Macros.scala
  def getArgSyms(tpe: c.Type) = {
    companionTree(tpe).right.flatMap { companion =>
      //tickle the companion members -- Not doing this leads to unexpected runtime behavior
      //I wonder if there is an SI related to this?
      companion.tpe.members.foreach(_ => ())
      tpe.members.find(x => x.isMethod && x.asMethod.isPrimaryConstructor) match {
        case None => Left("Can't find primary constructor of " + tpe)
        case Some(primaryConstructor) =>
          val flattened = primaryConstructor.asMethod.paramLists.flatten
          Right((
            companion,
            tpe.typeSymbol.asClass.typeParams,
            flattened,
            flattened.map(_.asTerm.isParamWithDefault)))
      }

    }
  }

  def genImpl[T: c.WeakTypeTag]: c.Tree = {

    val tpe = weakTypeOf[T]
    val members = tpe.decls
    val mapTypeSymbol = typeOf[MapLike[_, _, _]].typeSymbol
    val iterableTypeSymbol = typeOf[Iterable[_]].typeSymbol
    val enumSerializeValueType = typeOf[EnumSerializeValue]
    val optionTypeCons = typeOf[Option[_]].typeConstructor
    val arrayTypeCons = typeOf[Array[_]].typeConstructor
    val byteType = typeOf[Byte]

    def typeFormat(at: Type, dv: Option[Tree]): Tree = {
      if (at <:< typeOf[Int]) {
        val t = dv.getOrElse(q"0")
        q"new _root_.me.sgrouples.rogue.cc.macros.IntMacroBsonFormat(${t})"
      } else if (at <:< typeOf[Long]) {
        val t = dv.getOrElse(q"0L")
        q"new _root_.me.sgrouples.rogue.cc.macros.LongMacroBsonFormat($t)"
      } else if (at <:< typeOf[Boolean]) {
        val t = dv.getOrElse(q"false")
        q"new _root_.me.sgrouples.rogue.cc.macros.BooleanMacroBsonFormat($t)"
      } else if (at <:< typeOf[Double]) {
        val t = dv.getOrElse(q"0.0d")
        q"new _root_.me.sgrouples.rogue.cc.macros.DoubleMacroBsonFormat($t)"
      } else if (at <:< typeOf[String]) {
        val t = dv.getOrElse(q"""""""")
        q"new _root_.me.sgrouples.rogue.cc.macros.StringMacroBsonFormat($t)"
      } else if (at <:< typeOf[Enumeration#Value]) {
        val enumT = at.asInstanceOf[TypeRef].pre
        val enumType = enumT.termSymbol.asModule
        val ann = enumType.annotations.map(_.tree.tpe)
        if (ann.contains(enumSerializeValueType)) {
          q"_root_.me.sgrouples.rogue.cc.macros.EnumMacroFormats.enumValueMacroFormat($enumType)"
        } else {
          q"_root_.me.sgrouples.rogue.cc.macros.EnumMacroFormats.enumNameMacroFormat($enumType)"
        }
      } else if (at <:< typeOf[org.bson.types.ObjectId]) {
        q"new _root_.me.sgrouples.rogue.cc.macros.ObjectIdMacroBsonFormat[$at]()"
      } else if (at <:< typeOf[java.util.UUID]) {
        q"new _root_.me.sgrouples.rogue.cc.macros.UUIDMacroBsonFormat[$at]()"
      } else if (at <:< typeOf[java.util.Locale]) {
        q"new _root_.me.sgrouples.rogue.cc.macros.LocaleMacroBsonFormat()"
      } else if (at <:< typeOf[java.util.Currency]) {
        q"new _root_.me.sgrouples.rogue.cc.macros.CurrencyMacroBsonFormat()"
      } else if (at <:< typeOf[java.time.Instant]) {
        q"new _root_.me.sgrouples.rogue.cc.macros.InstantMacroBsonFormat()"
      } else if (at <:< typeOf[java.time.LocalDateTime]) {
        q"new _root_.me.sgrouples.rogue.cc.macros.LocalDateTimeMacroBsonFormat()"
      } else if (at <:< typeOf[BigDecimal]) {
        q"new _root_.me.sgrouples.rogue.cc.macros.BigDecimalMacroBsonFormat()"
      } else {
        //println(s"Should run implicit search ... how ? or genImpl ? AT  ${at}")
        //at.ty
        //println(s"implicit search for format MacroGen${at}")
        //val t = c.inferImplicitValue(q"MacroBsonFormat[typeOf($at)]")
        //t
        val x = appliedType(typeOf[MacroBsonFormat[_]], at)
        //val mgType = tq"_root_.me.sgrouples.rogue.cc.macros.MacroBsonFormat[$at]"

        //println(s"Implicit search for $x")
        c.inferImplicitValue(x)
      }
    }

    def fieldFormat(f: Symbol, dv: Option[Tree]): Tree = {
      val name = f.name
      val decoded = name.decodedName
      val retType = f.typeSignature
      val tp = f.typeSignature
      val tc = tp.typeConstructor

      if (tc == optionTypeCons) {
        val inner = typeFormat(tp.typeArgs.head, None)
        q"new _root_.me.sgrouples.rogue.cc.macros.OptMacroBsonFormat($inner)"
      } else if (tc == arrayTypeCons) {
        if (tp.typeArgs.head == byteType) {
          q"new _root_.me.sgrouples.rogue.cc.macros.BinaryMacroBsonFormat()"
        } else {
          val inner = typeFormat(tp.typeArgs.head, None)
          q"new _root_.me.sgrouples.rogue.cc.macros.ArrayMacroBsonFormat($inner)"
        }
      } else {
        val at = appliedType(tp, tp.typeArgs)
        if (at.baseClasses.contains(mapTypeSymbol)) {
          val inner = typeFormat(tp.typeArgs.tail.head, None)
          val keyT = tp.typeArgs.head
          val innerT = tp.typeArgs.tail.head

          q"new _root_.me.sgrouples.rogue.cc.macros.MapMacroFormat[$keyT, $innerT]($inner)"
        } else if (at.baseClasses.contains(iterableTypeSymbol)) {
          val inner = typeFormat(tp.typeArgs.head, None)
          q"new _root_.me.sgrouples.rogue.cc.macros.IterableLikeMacroFormat[${tp.typeArgs.head}, $at]($inner)"
        } else {
          //println(s"Type format search for ${at}")
          typeFormat(at, dv)
        }
      }
    }

    //val enumTypeSymbol = typeOf[Value]

    val ctorOpt = members.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.headOption
    ctorOpt.map { ctor =>

      //println(s"Main ctor ${ctor} - members ${members}")
      //ctor.name = "Value" members == "scala$Enumeration$$outerEnum"
      //if(ctor.name == "Value" && members.collectFirst())
      //ctor fields
      val fields = ctor.paramLists.head

      val companion: c.universe.Symbol = tpe.typeSymbol.companion

      if (companion == NoSymbol) {
        c.error(c.enclosingPosition, s"Companion symbol not found for case class ${tpe}. Can't find companion for inner classes")
      }

      val defaults: Seq[Option[c.universe.Tree]] = getArgSyms(tpe) match {
        case Right((companion, typeParams, argSyms, hasDefaults)) =>
          companion.tpe.member(TermName("apply")).info
          val x = fields.map(_.asTerm).zipWithIndex.map {
            case (p, i) =>
              if (!p.isParamWithDefault) None
              else {
                val getterName = TermName("apply$default$" + (i + 1))
                Some(q"$companion.$getterName")
              }
          }
          x
        case Left(_) =>
          fields.map(_.asTerm).map(_ => None)
      }

      /*val defaults = fields.map(_.asTerm).zipWithIndex.map {
        case (p, i) =>
          if (!p.isParamWithDefault) None
          else {
            val getterName = TermName("apply$default$" + (i + 1))
            Some(q"$companion.$getterName")
          }
      }*/
      //} getOrElse(c.error(c.enclosingPosition, "no stor"))
      //println(s"Defaults are ${defaults}")

      //println(s"FLDS ${fields}")
      println(s"Defaults are ${defaults}")
      val df = fields zip defaults
      val namesFormats = df.map {
        case (f, dvOpt) =>
          val name = f.name
          val formatName = TermName(name.decodedName.toString + "_fmt") // c.freshName()
          val format = fieldFormat(f, dvOpt)
          (name.decodedName.toString, formatName, format)
      }

      // println(s"outs ${namesFormats}")
      val terms = members.flatMap { symbol =>
        if (symbol.isTerm) {
          val term = symbol.asTerm
          if (term.isAccessor && term.getter.isMethod) {
            Some(term.name)
          } else None
        } else None
      }

      //println(s"T ${tpe}")
      //println(s"names ${terms}")
      val zipNames = terms.map { n => s"$n" }.toVector
      //println(s"Name map is ${zipNames}")
      val bsonFormats = namesFormats.map {
        case (_, formatName, format) =>
          q"val $formatName = $format"
      }
      val writers = namesFormats.map {
        case (fldName, formatName, _) =>
          val key = Constant(fldName)
          val accessor = TermName(fldName)
          q"addNotNull(doc, $key, $formatName.write(t.$accessor))"
      }
      val reads = namesFormats.map {
        case (fldName, formatName, _) =>
          val key = Constant(fldName)
          val accessor = TermName(fldName)
          q"$formatName.readOrDefault(doc.get($key))"
      }
      val appends = namesFormats.map {
        case (fldName, formatName, _) =>
          val key = Constant(fldName)
          val accessor = TermName(fldName)
          q"$formatName.append(writer, $key, t.$accessor)"
      }
      val fldsMap = namesFormats.map {
        case (fldName, formatName, _) =>
          val fn = fldName.toString
          ///subfields($fn, formatName)
          q"$fn -> $formatName"
      }
      val subFieldsAdd = namesFormats.map {
        case (fldName, formatName, _) =>
          val fn = fldName.toString
          q"subfields($fn, $formatName)"
      }

      val tpC = Constant(tpe.toString)

      val defImpl = if (defaults.forall(_.isDefined)) {
        val dd = defaults.flatten
        println("case class has all default values, can create deafult ")
        q"""$companion.apply(..$dd)"""
      } else {
        q"""throw new RuntimeException("No defaultValue implementation for type " + $tpC)"""
      }

      val r =
        q"""new MacroBsonFormat[$tpe] {
           ..$bsonFormats
           override val flds = Map(..$fldsMap) ++ Seq(..$subFieldsAdd).flatten
                  override def validNames():Vector[String] = ${zipNames}
                  override def defaultValue(): $tpe = {
                    $defImpl
                  }
                  override def read(b: _root_.org.bson.BsonValue): $tpe = {
                   if(b.isDocument()) {
                     val doc = b.asDocument()
                     new $tpe(..$reads)
                   } else {
                      defaultValue()
                   }
                  }
                  override def write(t: $tpe): _root_.org.bson.BsonValue = {
                    val doc = new _root_.org.bson.BsonDocument()
                    ..$writers
                    doc
                  }
                  private def appendVals(writer:_root_.org.bson.BsonWriter, t:$tpe): Unit = {
                     ..$appends
                  }
                  override def append(writer:_root_.org.bson.BsonWriter, k:String, t:$tpe): Unit = {
                    writer.writeStartDocument(k)
                    appendVals(writer, t)
                    writer.writeEndDocument()
                  }
                  override def append(writer:_root_.org.bson.BsonWriter, t:$tpe): Unit = {
                    writer.writeStartDocument()
                    appendVals(writer, t)
                    writer.writeEndDocument()
                  }
                }
          """
      //println(s"Will return ${r}")
      r
      //c.Expr[MacroNamesResolver[T]](r)
    } getOrElse {
      //c.abort(c.enclosingPosition, s"no Macro can be generated for ${tpe}")

      println(s"Not CC - tpe is ${tpe}, mem ${tpe.members}")
      println(s"TS ${tpe.typeSymbol} tc ${tpe.typeConstructor}")
      q""" new MacroBsonFormat[$tpe] {
          override def validNames():Vector[String] = Vector.empty
          override def defaultValue(): $tpe = {
            println("T def val not impl")
            ???
          }
          override def read(b: _root_.org.bson.BsonValue): $tpe = {
            println("Missing read")
            ???
          }
          override def write(t: $tpe): _root_.org.bson.BsonValue = {
            println("Missing write")
            ???
          }
          override def append(writer:_root_.org.bson.BsonWriter, k:String, v:$tpe): Unit = {
            println("Missing append")
            ???
          }
          override def append(writer:_root_.org.bson.BsonWriter, v:$tpe): Unit = {
            println("missing append2")
            ???
            }
          }
     """
    }
  }
}