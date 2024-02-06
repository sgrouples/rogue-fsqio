package me.sgrouples.rogue

import java.nio.{ByteBuffer, ByteOrder}
import java.time.{Instant, LocalDateTime, ZoneOffset}
import java.util.{Currency, Locale, TimeZone, UUID}

//import me.sgrouples.rogue.enums.ReflectEnumInstance
import me.sgrouples.rogue.map.MapKeyFormat
import org.bson._
import org.bson.types.{Decimal128, ObjectId}
import scala.deriving.Mirror
import scala.compiletime.{constValue, constValueTuple, erasedValue, summonAll, summonInline}

import scala.language.implicitConversions

import com.softwaremill.tagging.*

trait StandardBsonFormats
    extends BaseBsonFormats
    with BsonCollectionFormats

object BsonFormats extends StandardBsonFormats with BsonFormats

trait BsonFormats/* extends LowPrioBsonFormats */{
  this: StandardBsonFormats =>
}


/*
trait FamilyFormats extends LowPriorityFamilyFormats {
  this: StandardFormats =>

  // scala compiler doesn't like spray-json's use of a type alias in the sig
  override implicit def optionFormat[T: JsonFormat]: JsonFormat[Option[T]] = new OptionFormat[T]
}
object FamilyFormats extends DefaultJsonProtocol with FamilyFormats
 */

/*trait LowPrioBsonFormats {
  this: BaseBsonFormats with BsonFormats =>

  abstract class WrappedBsonFromat[Wrapped, SubRepr](implicit
      tpe: Typeable[Wrapped]
  ) {
    def write(v: SubRepr): BsonValue
    def read(b: BsonValue): SubRepr
    def flds: Map[String, BsonFormat[_]]
  }

  implicit def hHilBsonFormat[Wrapped](implicit
      tpe: Typeable[Wrapped]
  ): WrappedBsonFromat[Wrapped, HNil] = new WrappedBsonFromat[Wrapped, HNil] {
    override def write(t: HNil): BsonValue = BsonNull.VALUE
    override def read(b: BsonValue) = HNil
    def flds = Map.empty
  }

  implicit def hListFormat[Wrapped, Key <: Symbol, Value, Remaining <: HList](
      implicit
      t: Typeable[Wrapped],
      key: Witness.Aux[Key],
      headSer: Lazy[BsonFormat[Value]],
      remFormat: WrappedBsonFromat[Wrapped, Remaining] //,
      //defaults: Default.AsOptions[Wrapped]
  ): WrappedBsonFromat[Wrapped, FieldType[Key, Value] :: Remaining] =
    new WrappedBsonFromat[Wrapped, FieldType[Key, Value] :: Remaining] {

      private[this] val fieldName = key.value.name
      private[this] val hs = headSer.value

      //println(s"Defaults for ${fieldName} is ${defaults()}")
      //print(s"My argument position is ${}")
      override def write(ft: FieldType[Key, Value] :: Remaining): BsonValue = {

        val rest = remFormat.write(ft.tail)
        val serializedVal = hs.write(ft.head)
        if (!serializedVal.isNull) {
          if (rest.isNull) {
            new BsonDocument(fieldName, serializedVal)
          } else {
            rest.asDocument().append(fieldName, serializedVal)
          }
        } else {
          rest
        }
      }

      override def read(b: BsonValue): FieldType[Key, Value] :: Remaining = {
        val resolved: Value =
          try {
            val v = b.asDocument().get(fieldName)
            if (v == null || v.isNull) {
              //println(s"Read null value of ${fieldName}\n rem is ${remFormat}, ")
              try {
                //really - I want to read defaults() here, but don't know how yet

                hs.defaultValue
              } catch {
                case _: Exception =>
                  hs.read(BsonNull.VALUE)
              }
            } else {
              hs.read(v)
            }
          } catch {
            case e: BsonInvalidOperationException =>
              //println("BsonInvalid op exception - resort to default")
              hs.defaultValue
          }
        val remaining = remFormat.read(b)
        field[Key](resolved) :: remaining

      }

      def flds = {
        val subfieldFlds = hs.flds.map { case (subfieldName, s) =>
          (fieldName + "." + subfieldName -> s)
        }
        remFormat.flds ++ subfieldFlds + (fieldName -> hs)
      }

    }

  /* For future support of coproducts
 implicit def cNilFormat[Wrapped](
                                    implicit
                                    t: Typeable[Wrapped]
                                  ): WrappedBsonFromat[Wrapped, CNil] = new WrappedBsonFromat[Wrapped, CNil] {
    def write(c: CNil) = throw new RuntimeException("write should never be called for CNil")
  }

  implicit def coproductFormat[Wrapped, Name <: Symbol, Instance, Remaining <: Coproduct](
  implicit
  tpe: Typeable[Wrapped],
  key: Witness.Aux[Name],
  jfh: Lazy[BsonFormat[Instance]],
  jft: WrappedBsonFromat[Wrapped, Remaining]
  ): WrappedBsonFromat[Wrapped, FieldType[Name, Instance] :+: Remaining] =
    new WrappedBsonFromat[Wrapped, FieldType[Name, Instance] :+: Remaining] {
      def write(lr: FieldType[Name, Instance] :+: Remaining) = lr match {
        case Inl(l) =>
          val serV = jfh.value.write(l)
          if(serV.isDocument) {
            //            case j: JsObject => th.write(j, key.value)
            val v = key.value
            println(s"Coproduct format called, ${serV}\nkey ${key}\nV$v")
            serV
          } else {
            throw new RuntimeException(s"Expected document  got ${serV}")
          }

        case Inr(r) =>
          jft.write(r)
      }
    }
   */

  implicit def bsonEncoder[T, Repr](implicit
      gen: LabelledGeneric.Aux[T, Repr],
      sg: Cached[Strict[WrappedBsonFromat[T, Repr]]],
      d: Default.AsRecord[T],
      tpe: Typeable[T]
  ): BsonFormat[T] = new BsonFormat[T] with BsonArrayReader[T] {
    override def write(t: T): BsonValue = sg.value.value.write(gen.to(t))
    override def read(b: BsonValue): T = gen.from(sg.value.value.read(b))

    override val flds: Map[String, BsonFormat[_]] = sg.value.value.flds

    /** for nested case classes, with default constructors provides a default
      * value consider
      * {{{
      *   case class In(j:Int = 1)
      *   case class Out(i:In = In(), x: Int = 5)
      *   val f = BsonFormat[Out]
      * }}}
      * in such case default can be provided if 'i' is missing from parameter as
      * in example
      * {{{
      *   f.parse(new BsonDocument) == Out(In(1), 0 )
      * }}}
      * value for 'i' will be `In(1)` because In has default non-parameter
      * constructor, but value for x will be 0 this is because currently only
      * full missing values will be constructed - missing partial values will be
      * filled with type-default values 0 for Int in this example
      * @return
      *   default T
      */
    override def defaultValue: T = {
      try {
        gen.from(d().asInstanceOf[Repr])
      } catch {
        case _: Exception =>
          throw new NoDefaultFormatForDerivedException(
            s"Requested default value for but case class ${tpe.describe} has no default, non-parameter constructor"
          )
      }
    }
  }
*/
