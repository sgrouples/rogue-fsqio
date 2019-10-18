package me.sgrouples.rogue.cc.macros

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros
import me.sgrouples.rogue.EnumSerializeValue
import me.sgrouples.rogue.map.MapKeyFormat

import scala.collection.Seq
import scala.collection.MapLike

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
    println(s"genImpl ${tpe}")
    val members = tpe.decls
    val mapTypeSymbol = typeOf[MapLike[_, _, _]].typeSymbol
    val iterableTypeSymbol = typeOf[Iterable[_]].typeSymbol
    val enumSerializeValueType = typeOf[EnumSerializeValue]
    val optionTypeCons = typeOf[Option[_]].typeConstructor
    val arrayTypeCons = typeOf[Array[_]].typeConstructor
    val byteType = typeOf[Byte]

    def typeFormat(at: Type, dv: Option[Tree]): Tree = {
      println(s"typeFormat ${at} : ${dv}")
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
        val t = dv.getOrElse(q""""".asInstanceOf[$at]""")
        q"new _root_.me.sgrouples.rogue.cc.macros.StringMacroBsonFormat[$at]($t)"
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
        val t = dv.getOrElse(q"java.time.Instant.ofEpochMilli(0L)")
        q"new _root_.me.sgrouples.rogue.cc.macros.InstantMacroBsonFormat($t)"
      } else if (at <:< typeOf[java.time.LocalDateTime]) {
        q"new _root_.me.sgrouples.rogue.cc.macros.LocalDateTimeMacroBsonFormat()"
      } else if (at <:< typeOf[BigDecimal]) {
        q"new _root_.me.sgrouples.rogue.cc.macros.BigDecimalMacroBsonFormat()"
      } else {
        val x = appliedType(typeOf[MacroBsonFormat[_]], at)
        c.inferImplicitValue(x)
      }
    }

    def tcFormat(tp: Type, tc: Type, dv: Option[Tree]): Tree = {
      println(s"tcFormat ${tp} : ${tc} : ${dv}")

      if (tc == optionTypeCons) {
        val ha = tp.typeArgs.head
        val inner = if (ha.typeArgs.nonEmpty) {
          tcFormat(ha, ha.typeConstructor, None)
        } else {
          val dealiased = tp.typeArgs.head.dealias
          if (dealiased.typeArgs.nonEmpty) {
            tcFormat(dealiased, dealiased.typeConstructor, None)
          } else {
            typeFormat(tp.typeArgs.head, None)
          }
        }
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
          val apT = appliedType(keyT.typeConstructor, keyT.typeArgs)
          val innerT = tp.typeArgs.tail.head
          val kf = appliedType(typeOf[me.sgrouples.rogue.map.MapKeyFormat[_]], apT)
          val inferred = c.inferImplicitValue(kf, false, false, c.enclosingPosition)
          if (inferred == null) {
            c.error(c.enclosingPosition, s"Can't find MapKeyFormat for ${keyT} - class ${tpe}")
          }
          q"new _root_.me.sgrouples.rogue.cc.macros.MapMacroFormat[$keyT, $innerT]($inner)($inferred)"
        } else if (at.baseClasses.contains(iterableTypeSymbol)) {
          val inner = typeFormat(tp.typeArgs.head, None)
          q"new _root_.me.sgrouples.rogue.cc.macros.IterableLikeMacroFormat[${tp.typeArgs.head}, $at]($inner)"
        } else {
          println(s"Else type format ${at} / ${dv}")
          typeFormat(at, dv)
        }
      }
    }

    def fieldFormat(f: Symbol, dv: Option[Tree]): Tree = {
      println(s"Field format ${f} : ${dv}")
      val tp = f.typeSignature
      val tc = tp.typeConstructor
      tcFormat(tp.dealias, tc.dealias, dv)
    }

    val ctorOpt = members.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.headOption
    ctorOpt.map { ctor =>
      val fields = ctor.paramLists.head

      val companion: c.universe.Symbol = tpe.typeSymbol.companion

      if (companion == NoSymbol) {
        c.error(c.enclosingPosition, s"Companion symbol not found for case class ${tpe}. Can't find companion for inner classes")
      }

      val defaults: Seq[Option[c.universe.Tree]] = getArgSyms(tpe) match {
        case Right((companion, typeParams, argSyms, hasDefaults)) =>
          println(s"Right ${companion} / ${typeParams} / ${argSyms} / ${hasDefaults}")
          companion.tpe.member(TermName("apply")).info
          fields.map(_.asTerm).zipWithIndex.map {
            case (p, i) =>
              if (!p.isParamWithDefault) None
              else {
                val getterName = TermName("apply$default$" + (i + 1))
                Some(q"$companion.$getterName")
              }
          }
        case Left(x) =>
          println(s"Left ${x}")
          fields.map(_.asTerm).map(_ => None)
      }
      println(s"DEFAULTS ${defaults}")
      val df = fields zip defaults
      val namesFormats = df.map {
        case (f, dvOpt) =>
          val name = f.name
          val formatName = TermName(name.decodedName.toString + "_fmt") // c.freshName()
          val format = fieldFormat(f, dvOpt)
          (name.decodedName.toString, formatName, format)
      }
      val terms = members.flatMap { symbol =>
        if (symbol.isTerm) {
          val term = symbol.asTerm
          if (term.isAccessor && term.getter.isMethod) {
            Some(term.name)
          } else None
        } else None
      }

      val zipNames = terms.map { n => s"$n" }.toVector
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
          q"$fn -> $formatName"
      }
      val subFieldsAdd = namesFormats.map {
        case (fldName, formatName, _) =>
          val fn = fldName.toString
          q"subfields($fn, $formatName)"
      }

      val tpC = Constant(tpe.toString)
      println(s"defaults for all ${defaults} - for ${tpC} ")
      val defImpl = if (defaults.forall(_.isDefined)) {
        val dd = defaults.flatten
        q"""$companion.apply(..$dd)"""
      } else {
        println("throw exception - lack of default value ? why")
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
      r
    } getOrElse {
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