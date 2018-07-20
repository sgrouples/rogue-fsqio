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

  val s: String = "aa"

  def genImpl[T: c.WeakTypeTag]: c.Tree = {

    //TODO - will it put proper package ?
    def enumX(t: Type, v: Type) = {
      val m = t.termSymbol.asModule

      println(s"asModule ${t.termSymbol.asModule}")
      //t.termSymbol.asModule.isPackage
      val r = q"def read(s:String):${v} = { ${m}.withName(s) } "
      println("qq ok ")
      println(r)
      ()
    }

    val tpe = weakTypeOf[T]
    val members = tpe.decls
    //val mapTypeSymbol = typeOf[MapLike[_, _, _]].typeSymbol
    //val iterableTypeSymbol = typeOf[Iterable[_]].typeSymbol
    val enumSerializeValueType = typeOf[EnumSerializeValue]
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

      val defaults = fields.map(_.asTerm).zipWithIndex.map {
        case (p, i) =>
          if (!p.isParamWithDefault) None
          else {
            val getterName = TermName("apply$default$" + (i + 1))
            Some(q"$companion.$getterName")
          }
      }
      //} getOrElse(c.error(c.enclosingPosition, "no stor"))
      //println(s"Defaults are ${defaults}")

      //println(s"FLDS ${fields}")
      val df = fields zip defaults
      val namesFormats = df.map {
        case (f, dvOpt) =>

          val name = f.name
          val decoded = name.decodedName
          val retType = f.typeSignature
          //tpe.decl(name).typeSignature.finalResultType
          //println(s"F ${name} / ${decoded} / ${retType}")

          //val x = q"BsonRW[c.typeOf[$retType]]"
          //println(s"x ${x}")
          //x
          val tp = f.typeSignature
          val at = appliedType(tp, tp.typeArgs)
          //println(s"Base ${at.baseClasses}")
          //primitives
          val fmtName = s"${name}_fmt"

          /* val format = if (at <:< typeOf[Int]) {
          println(s"Int member ${name}")
          val defVal = dvOpt.getOrElse(q"0")
          q"val $fmtName = new _root_.me.sgrouples.rogue.cc.macros.IntMacroBsonFormat($defVal)"
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
        }
        else if (at <:< typeOf[Enumeration#Value]) {
          val enumT = at.asInstanceOf[TypeRef].pre
          //val defVal = dvOpt.getOrElse(q"0")
          val enumType = enumT.termSymbol.asModule
          q"val $fmtName = _root_.me.sgrouples.rogue.cc.macros.EnumMacroFormats.dummyEnumFmt($enumType)"
          //val ev = enumValues(at)
          //println(s"EV ${ev}")
        } else {
          q""
        }
      else {
          println("something else - implicit search")
          println(s"Annotations ${f.annotations}")
          //if(at <:< Enumeration)
          //val wtt = weakTypeTag(at.typeParams)
          //enum
          println(s"Other - ... ${name} / ${at} / ${tp.typeConstructor}")
          println(s"F ${f}")
          println(s"ts ${f.typeSignature}")
          println(f.info)

          println(s"type params ${f.typeSignature.typeParams}")
          println(f.typeSignature.typeConstructor)
          println(f.typeSignature.resultType)
          //f.
          //Option[
          //arrays
          //Seq
          //MapLike
          println(s"implicit search for ${at} activated")
          val s1 = c.inferImplicitValue(at)
          println(s"found s1 ${s1}")
        }
        //val s = q"implicitly[Bla[$tp]]"
        val t = tq"_root_.me.sgrouples.rogue.cc.macros.MacroGen[$tp]"
        println(s"T ${t} type ${t.tpe} , ${at}")
        //val checkedType = c.typecheck(t)
        //println(s"Serching for implicit value for ${checkedType}")

        //s1
        "a"*/

          val formatName = TermName(name.decodedName.toString + "_fmt") // c.freshName()
          val format = if (at <:< typeOf[Int]) {
            println(s"Int member ${name}")
            //val defVal = dvOpt.getOrElse(q"0")
            val ser = q"val $formatName = new _root_.me.sgrouples.rogue.cc.macros.IntMacroBsonFormat(0)"
            // q"val $fieldName = $ser"
            println(s"SEr ${ser}")
            ser
          } else if (at <:< typeOf[Enumeration#Value]) {
            val enumT = at.asInstanceOf[TypeRef].pre
            //val defVal = dvOpt.getOrElse(q"0")
            val enumType = enumT.termSymbol.asModule
            println(s"Enum annotations ${enumType.annotations}")
            //YES - enumSerializeValue!!!!!!
            val ann = enumType.annotations.map(_.tree.tpe)
            println(s"ANN ${ann}")
            if (ann.contains(enumSerializeValueType)) {
              println("YES CONTAINS")
            } else {
              println("Nt contains")
            }
            q"val $formatName = _root_.me.sgrouples.rogue.cc.macros.EnumMacroFormats.dummyEnumFmt($enumType)"
            //val ev = enumValues(at)
            //println(s"EV ${ev}")
          } else {
            println("DUMMY")
            q"val $formatName = new _root_.me.sgrouples.rogue.cc.macros.IntMacroBsonFormat(0)"
          }
          (name.decodedName.toString, formatName, format)
      }

      println(s"outs ${namesFormats}")
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
      val zipNames = terms.zipWithIndex.map {
        case (n, i) =>
          (i -> s"$n")
      }.toVector
      println(s"Name map is ${zipNames}")
      val bsonFormats = namesFormats.map(_._3)
      val writers = namesFormats.map {
        case (fldName, formatName, _) =>
          val key = Constant(fldName)
          val accessor = TermName(fldName)
          q"temporaryDocument.put($key, $formatName.write(t.$accessor))"
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
      /* val writes = namesFormats.map { v =>

        q"${v._2}.append(${v._1}, ${v._1})"
      }*/
      //println(s"terms ${terms}")
      val testName = c.freshName()
      val r =
        q"""new MacroBsonFormat[$tpe] {
           ..$bsonFormats
                  override def namesMap():Vector[(Int, String)] = ${zipNames}
                  override def defaultValue(): $tpe = {???}
                  override def read(b: _root_.org.bson.BsonValue): $tpe = {
                   if(b.isDocument()) {
                     val doc = b.asDocument()
                     new $tpe(..$reads)
                   } else {
                      defaultValue()
                   }
                  }
                  override def write(t: $tpe): _root_.org.bson.BsonValue = {
                    val temporaryDocument = new _root_.org.bson.BsonDocument()
                    ..$writers
                    temporaryDocument
                  }
                  override def append(writer:_root_.org.bson.BsonWriter, k:String, t:$tpe): Unit = {
                    writer.writeStartDocument(k)
                    ..$appends
                    writer.writeEndDocument()
                  }

                }
          """

      println(s"Will return ${r}")
      r
      //c.Expr[MacroNamesResolver[T]](r)
    } getOrElse {
      q""" new MacroBsonFormat[$tpe] {
          override def namesMap():Vector[(Int, String)] = Vector.empty
                           override def defaultValue(): $tpe = {???}
                           override def read(b: _root_.org.bson.BsonValue): $tpe = ???
                           override def write(t: $tpe): _root_.org.bson.BsonValue = ???
                           override def append(writer:_root_.org.bson.BsonWriter, k:String, v:$tpe): Unit = ???
          }
     """
    }
  }
}