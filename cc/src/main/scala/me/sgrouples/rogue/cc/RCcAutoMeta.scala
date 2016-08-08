package me.sgrouples.rogue.cc

import java.time.LocalDateTime
import java.util.UUID

import me.sgrouples.rogue._
import org.bson.types.ObjectId

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

object RCcAutoMeta {
  def apply[T](collectionName: String)(implicit f: BsonFormat[T]): Any = macro RCcAutoMetaImpl.applyImpl[T]
}

object RCcAutoMetaImpl {

  /*
  class IntField[O](name: String, o: O) extends MCField[Int, O](name, o) {
  override def defaultValue = 0
  }
  class LongField[O](name: String, o: O) extends MCField[Long, O](name, o) {
    override def defaultValue = 0L
  }
  class DoubleField[O](name: String, o: O) extends MCField[Double, O](name, o) {
    override def defaultValue = 0d
  }
  class StringField[O](name: String, o: O) extends MCField[String, O](name, o) {
    override def defaultValue = ""
  }
  class ObjectIdField[O](name: String, o: O) extends MCField[ObjectId, O](name, o) {
    override def defaultValue = ObjectId.get()
  }

  class UUIDIdField[O](name: String, o: O) extends MCField[UUID, O](name, o) {
    override def defaultValue = UUID.randomUUID()
  }

  class LocalDateTimeField[O](name: String, o: O) extends MCField[LocalDateTime, O](name, o) {
    override def defaultValue = LocalDateTime.now()
  }

  class BooleanField[O](name: String, o: O) extends MCField[Boolean, O](name, o) {
    override def defaultValue = false
  }
  class EnumField[T <: Enumeration, O](name: String, o: O)(implicit e: T) extends MCField[T#Value, O](name, o) {
    override def defaultValue: T#Value = e(0)
  }

  class ListField[V, O](name: String, o: O) extends MCField[List[V], O](name, o) {
    override def defaultValue = Nil
  }

  class ArrayField[V: ClassTag, O](name: String, o: O) extends MCField[Array[V], O](name, o) {
    override def defaultValue = Array.empty[V]
  }

  class CClassField[C, MC <: CcMeta[C], O](val name: String, val childMeta: MC, val owner: O) extends Field[C, O]

  class CClassListField[C, MC <: CcMeta[C], O](name: String, val childMeta: MC, owner: O) extends MCField[Seq[C], O](name, owner) {
    override def defaultValue: List[C] = Nil
  }

  class CClassArrayField[C: ClassTag, O](name: String, o: O) extends MCField[Array[C], O](name, o) {
    override def defaultValue = Array.empty[C]
  }

  class MapField[V, O](name: String, o: O) extends MCField[Map[String, V], O](name, o) {
    override def defaultValue = Map.empty
  }

  class OptIntField[O](name: String, o: O) extends OCField[Int, O](name, o)
  class OptLongField[O](name: String, o: O) extends OCField[Long, O](name, o)
  class OptDoubleField[O](name: String, o: O) extends OCField[Double, O](name, o)
  class OptStringField[O](name: String, o: O) extends OCField[String, O](name, o)
  class OptObjectIdField[O](name: String, o: O) extends OCField[ObjectId, O](name, o)
  class OptUUIDIdField[O](name: String, o: O) extends OCField[UUID, O](name, o)
  class OptLocalDateTimeField[O](name: String, o: O) extends OCField[LocalDateTime, O](name, o)
  class OptBooleanField[O](name: String, o: O) extends OCField[Boolean, O](name, o)
  class OptEnumField[T <: Enumeration, O](name: String, o: O)(implicit e: T) extends OCField[T#Value, O](name, o)
  class OptListField[V, O](name: String, o: O) extends OCField[List[V], O](name, o)
  class OptArrayField[V: ClassTag, O](name: String, o: O) extends OCField[Array[V], O](name, o)
  class OptCClassField[C, MC <: CcMeta[C], O](val name: String, val childMeta: MC, val owner: O) extends Field[C, O]
  class OptCClassListField[C, O](name: String, o: O) extends OCField[List[C], O](name, o)
  class OptCClassArrayField[C: ClassTag, O](name: String, o: O) extends OCField[Array[C], O](name, o)
  class OptMapField[V, O](name: String, o: O) extends OCField[Map[String, V], O](name, o)
   */

  def applyImpl[A: c.WeakTypeTag](c: Context)(collectionName: c.Expr[String])(f: c.Expr[BsonFormat[A]]): c.Expr[Any] = {

    import c.universe._

    def log(msg: String) = Console println msg

    val tpe = weakTypeOf[A]
    val sym = tpe.typeSymbol.asClass

    require(sym.isCaseClass)

    val companionObject = sym.companion
    val companionType = companionObject.typeSignature

    val applyMethod = companionType.decl(TermName("apply")).asMethod

    val params = applyMethod.paramLists.flatten

    val paramsWithNames = params.map {
      p => p.name.decodedName.toString -> p.typeSignature
    }

    val outClsName = sym.name.decodedName

    val outCls = TypeName(outClsName + "_RCcAutoMeta")

    def clsName[T: Manifest]: String = {
      implicitly[Manifest[T]].runtimeClass.getSimpleName
    }

    def mkSimpleField[C: Manifest](name: String): c.Tree = {
      val nameLiteral = Literal(Constant(name))
      val nameTerm = TermName(name)
      val fieldTypeNameTerm = TypeName(clsName[C])
      q"val $nameTerm = new $fieldTypeNameTerm[$outCls]($nameLiteral, this)"
    }

    def mkTypeParamField[C: Manifest](name: String, t: Type): c.Tree = {
      val nameLiteral = Literal(Constant(name))
      val nameTerm = TermName(name)
      val fieldTypeNameTerm = TypeName(clsName[C])
      val p = t.typeArgs.head
      q"val $nameTerm = new $fieldTypeNameTerm[$p, $outCls]($nameLiteral, this)"
    }

    // TODO: this currently doesn't work
    def mkCaseClassField[C: Manifest](name: String, t: Type): c.Tree = {
      val nameLiteral = Literal(Constant(name))
      val nameTerm = TermName(name)
      val fieldTypeNameTerm = TypeName(clsName[C])

      val metaName = TermName(List(
        t.typeSymbol.asClass.name.decodedName, "_",
        outClsName, "_", "RCcAutoMeta"
      ).mkString)

      val meta = q"val $metaName = RCcAutoMetaImpl.applyImpl[$t]"
      val metaType = q"RCcMeta[$t]"

      q"val $nameTerm = new $fieldTypeNameTerm[$t, $metaType, $outCls]($nameLiteral, $meta, this)"
    }

    // @formatter:off
    // format: OFF

    val fields = paramsWithNames.map {
      case (n, t) if t <:< typeOf[String]             => mkSimpleField[StringField[_]](n)
      case (n, t) if t <:< typeOf[Long]               => mkSimpleField[LongField[_]](n)
      case (n, t) if t <:< typeOf[Int]                => mkSimpleField[IntField[_]](n)
      case (n, t) if t <:< typeOf[Double]             => mkSimpleField[DoubleField[_]](n)
      case (n, t) if t <:< typeOf[ObjectId]           => mkSimpleField[ObjectIdField[_]](n)
      case (n, t) if t <:< typeOf[Boolean]            => mkSimpleField[BooleanField[_]](n)
      case (n, t) if t <:< typeOf[LocalDateTime]      => mkSimpleField[LocalDateTimeField[_]](n)
      case (n, t) if t <:< typeOf[UUID]               => mkSimpleField[OptUUIDIdField[_]](n)
      case (n, t) if t <:< typeOf[List[_]]            => mkTypeParamField[ListField[_, _]](n, t)
      case (n, t) if t.typeSymbol.asClass.isCaseClass => mkCaseClassField[CClassField[_, _, _]](n , t)
    }

    val dummy = TermName(c.freshName)

    log(s"Generated following fields for $tpe:\n" + q"..$fields")

    val res = q"""
      import me.sgrouples.rogue._
      import me.sgrouples.rogue.cc._
      class $outCls(c: String)(f: BsonFormat[$tpe])
      extends RCcMeta[$tpe](c)(f) { ..$fields }
      val $dummy = 1
      new $outCls($collectionName)($f)
      """

    c.Expr(res)
  }
}

