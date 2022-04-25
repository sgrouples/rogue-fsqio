package me.sgrouples.rogue

import me.sgrouples.rogue.enums.ReflectEnumInstance
import org.bson.{BsonInt32, BsonString, BsonValue}

class EnumerationFormats {

}


/** serialize enums as names
  */

trait EnumNameFormats {
  /*implicit def enumFormat[T <: Enumeration: TypeTag]
  : BasicBsonFormat[T#Value] = {

    new BasicBsonFormat[T#Value] with ReflectEnumInstance[T] {

      private val `enum` = enumeration

      override def read(b: BsonValue): T#Value = {
        try {
          `enum`.withName(b.asString().getValue)
        } catch {
          case e: IllegalArgumentException =>
            throw new IllegalArgumentException(
              s"cannot read enum name ${b.asString()} for enum ${`enum`.toString()}",
              e
            )
        }
      }

      override def write(t: T#Value): BsonValue = new BsonString(t.toString)

      override def defaultValue: T#Value = `enum`.apply(0)
    }
  } */
}

object EnumNameFormats extends EnumNameFormats

/** serialize enums as integers
  */
trait EnumValueFormats {
  /*
  implicit def enumValueFormat[T <: Enumeration: TypeTag]
  : BasicBsonFormat[T#Value] = {

    new BasicBsonFormat[T#Value] with ReflectEnumInstance[T] {

      private val `enum` = enumeration

      override def read(b: BsonValue): T#Value = try {
        `enum`.apply(b.asNumber().intValue())
      } catch {
        case e: IllegalArgumentException =>
          throw new IllegalArgumentException(
            s"cannot read enum value ${b.asNumber()}, for enum ${`enum`.toString()} ",
            e
          )
      }

      override def write(t: T#Value): BsonValue = new BsonInt32(t.id)

      override def defaultValue: T#Value = {
        `enum`.values.head
      }
    }
  } */
}
object EnumValueFormats extends EnumValueFormats

/** Sometimes it is necessary to use Enumerations that are serialized to Ints
  * and others that serialize to Strings within the same
  * [[me.sgrouples.rogue.cc.RCcMeta]]. It can be obtained by using
  * [[EnumSerializeValue]] annotation
  * @see
  *   EnumAnnotationTest example:
  * {{{
  *   object EName extends Enumeration { val v1 = Value("V1"); val v2 = Value("V2") }
  *   @EnumSerializeValue object EValue extends Enumeration { val v1 = Value("bla1"); val v2 = Value("bla2") }
  *   case class C(e:EName.Value, v: EValue.Value)
  *   import import BsonFormats._ ;import EnumAnnotatedFormats._
  *   implicit val enE = EName
  *   implicit val evE = EValue
  *   val format = BsonFormat[C]
  *   println(format.write(C(EName.v1, EValue.v2)))
  * }}}
  * will produce
  * {{{
  * { "v" : 1, "e" : "V1" }
  * }}}
  */
object EnumAnnotatedFormats {
 /* implicit def enumFormat[T <: Enumeration: TypeTag]: BsonFormat[T#Value] = {
    import scala.reflect.runtime._
    val rt = universe.typeOf[T].typeSymbol.asClass
    val eto = universe.typeOf[EnumSerializeValue]
    if (rt.annotations.exists(_.tree.tpe == eto)) {
      EnumValueFormats.enumValueFormat[T]
    } else EnumNameFormats.enumFormat[T]
  } */
}
