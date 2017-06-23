package me.sgrouples.rogue

import java.nio.{ ByteBuffer, ByteOrder }
import java.time.{ Instant, LocalDateTime, ZoneOffset }
import java.util.{ Currency, Locale, TimeZone, UUID }

import me.sgrouples.rogue.enums.ReflectEnumInstance
import org.bson._
import org.bson.types.{ Decimal128, ObjectId }
import shapeless._
import shapeless.labelled.{ FieldType, field }

import scala.annotation.{ StaticAnnotation, implicitNotFound }
import scala.collection.mutable.ArrayBuffer
import scala.language.{ higherKinds, implicitConversions }
import scala.reflect.ClassTag
import shapeless.tag.@@

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom

@implicitNotFound("implicit BsonFormat not found for ${T}")
trait BsonFormat[T] {
  def read(b: BsonValue): T
  def readArray(b: BsonArray): Seq[T]
  def write(t: T): BsonValue
  def flds: Map[String, BsonFormat[_]]
  def defaultValue: T
}

trait BsonArrayReader[T] {
  this: BsonFormat[T] =>
  override def readArray(b: BsonArray) = {
    val sb = Seq.newBuilder[T]
    val it = b.iterator()
    while (it.hasNext) {
      sb += read(it.next())
    }
    sb.result()
  }
}

trait BasicBsonFormat[T] extends BsonFormat[T] with BsonArrayReader[T] {
  override def flds = Map.empty
}

class EnumSerializeValue extends StaticAnnotation
/**
 * serialize enums as names
 */

trait EnumNameFormats {

  import scala.reflect.runtime.universe
  import universe._

  implicit def enumFormat[T <: Enumeration: TypeTag]: BasicBsonFormat[T#Value] = {

    new BasicBsonFormat[T#Value] with ReflectEnumInstance[T] {

      override def read(b: BsonValue): T#Value =
        enumeration.withName(b.asString().getValue)

      override def write(t: T#Value): BsonValue = new BsonString(t.toString)

      override def defaultValue: T#Value = enumeration.apply(0)
    }
  }
}

object EnumNameFormats extends EnumNameFormats
/**
 * serialize enums as integers
 */
trait EnumValueFormats {

  import scala.reflect.runtime.universe
  import universe._

  implicit def enumValueFormat[T <: Enumeration: TypeTag]: BasicBsonFormat[T#Value] = {

    new BasicBsonFormat[T#Value] with ReflectEnumInstance[T] {

      override def read(b: BsonValue): T#Value = enumeration.apply(b.asNumber().intValue())

      override def write(t: T#Value): BsonValue = new BsonInt32(t.id)

      override def defaultValue: T#Value = enumeration.apply(0)
    }
  }
}
object EnumValueFormats extends EnumValueFormats

/**
 * Sometimes it is necessary to use Enumerations that are serialized to Ints and others that serialize to Strings
 * within the same [[me.sgrouples.rogue.cc.RCcMeta]]. It can be obtained by using [[EnumSerializeValue]] annotation
 * @see EnumAnnotationTest
 * example:
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
 *  { "v" : 1, "e" : "V1" }
 * }}}
 *
 */
object EnumAnnotatedFormats {
  import scala.reflect.runtime.universe._
  implicit def enumFormat[T <: Enumeration: TypeTag]: BsonFormat[T#Value] = {
    import scala.reflect.runtime._
    val rt = universe.typeOf[T].typeSymbol.asClass
    val eto = universe.typeOf[EnumSerializeValue]
    if (rt.annotations.exists(_.tree.tpe == eto)) {
      EnumValueFormats.enumValueFormat[T]
    } else EnumNameFormats.enumFormat[T]
  }
}

/**
 * Basic bson serializers
 */
trait BaseBsonFormats {

  implicit object BooleanBsonFormat extends BasicBsonFormat[Boolean] {
    override def read(b: BsonValue): Boolean = b.asBoolean().getValue()
    override def write(t: Boolean): BsonValue = new BsonBoolean(t)
    override def defaultValue: Boolean = false
  }

  implicit object IntBsonFormat extends BasicBsonFormat[Int] {
    override def read(b: BsonValue): Int = b.asNumber().intValue()
    override def write(t: Int): BsonValue = new BsonInt32(t)
    override def defaultValue: Int = 0
  }

  implicit object LongBsonFormat extends BasicBsonFormat[Long] {
    override def read(b: BsonValue): Long = b.asNumber().longValue()
    override def write(t: Long): BsonValue = new BsonInt64(t)
    override def defaultValue: Long = 0L
  }

  implicit object StringBsonFormat extends BasicBsonFormat[String] {
    override def read(b: BsonValue): String = b.asString().getValue()
    override def write(t: String): BsonValue = new BsonString(t)
    override def defaultValue: String = ""
  }

  implicit object ObjectIdBsonFormat extends BasicBsonFormat[ObjectId] {
    override def read(b: BsonValue): ObjectId = b.asObjectId().getValue()
    override def write(t: ObjectId): BsonValue = new BsonObjectId(t)
    override def defaultValue: ObjectId = new ObjectId(0, 0, 0.toShort, 0)
  }

  implicit object DoubleBsonFormat extends BasicBsonFormat[Double] {
    override def read(b: BsonValue): Double = b.asDouble().doubleValue()
    override def write(t: Double): BsonValue = new BsonDouble(t)
    override def defaultValue: Double = 0.0d
  }

  implicit object BigDecimalBsonFormat extends BasicBsonFormat[BigDecimal] {
    override def read(b: BsonValue): BigDecimal = b.asDecimal128().decimal128Value().bigDecimalValue()
    override def write(t: BigDecimal): BsonValue = new BsonDecimal128(new Decimal128(t.bigDecimal))
    override def defaultValue: BigDecimal = BigDecimal(0)
  }

  implicit object CurrencyBsonFormat extends BasicBsonFormat[Currency] {
    override def read(b: BsonValue): Currency = Currency.getInstance(b.asString().getValue)
    override def write(t: Currency): BsonValue = new BsonString(t.getCurrencyCode)
    override def defaultValue: Currency = Currency.getInstance("USD")
  }

  implicit object LocaleBsonFormat extends BasicBsonFormat[Locale] {
    override def read(b: BsonValue): Locale = {
      //taken from lift's LocaleField as the simpliest solution, albeit not optimal.
      //commons-lang LocaleUtils were not able to parse all Locale.getAvailableLocales either.
      val value = b.asString().getValue
      Locale.getAvailableLocales.filter(_.toString == value).headOption.getOrElse(defaultValue)
    }
    override def write(l: Locale): BsonValue = new BsonString(l.toString)
    override def defaultValue: Locale = Locale.US
  }

  private def `@@AnyBsonFormat`[T, Tag](implicit tb: BsonFormat[T]): BasicBsonFormat[T @@ Tag] = {
    new BasicBsonFormat[T @@ Tag] {
      override def read(b: BsonValue): T @@ Tag = tag[Tag][T](tb.read(b))
      override def write(t: T @@ Tag): BsonValue = tb.write(t)
      override def defaultValue: T @@ Tag = tag[Tag][T](tb.defaultValue)
    }
  }

  // TODO: this should be split to BsonReader[+T] and BsonWriter[-T] so that we wouldn't have to code all that by hand

  implicit def `@@StringBsonFormat`[Tag] = `@@AnyBsonFormat`[String, Tag]
  implicit def `@@LongBsonFormat`[Tag] = `@@AnyBsonFormat`[Long, Tag]
  implicit def `@@IntBsonFormat`[Tag] = `@@AnyBsonFormat`[Int, Tag]
  implicit def `@@DoubleBsonFormat`[Tag] = `@@AnyBsonFormat`[Double, Tag]
  implicit def `@@ObjectIdBsonFormat`[Tag] = `@@AnyBsonFormat`[ObjectId, Tag]

  /**
   * care must be taken - because of different UUID formats.
   * by default codec can read any type, but for write - decision must be made
   * either format 3 - JAVA_LEGACY or format 4 - UUID_STANDARD
   * recommendation is to use UUID_STANDARD, but java driver uses JAVA_LEGACY by default
   * @see [https://jira.mongodb.org/browse/JAVA-403] for explanation
   * @see [[org.bson.codecs.UuidCodec]]
   * this implementation does not support C_SHARP_LEGACY codec at all
   */
  implicit object UUIDBsonFormat extends BasicBsonFormat[UUID] {
    override def read(bv: BsonValue): UUID = {
      val b = bv.asBinary()
      val bytes = b.getData()
      val bb = ByteBuffer.wrap(b.getData)
      if (b.getType == BsonBinarySubType.UUID_LEGACY.getValue)
        bb.order(ByteOrder.LITTLE_ENDIAN)
      else
        bb.order(ByteOrder.BIG_ENDIAN)
      new UUID(bb.getLong(0), bb.getLong(8))
    }

    override def write(t: UUID): BsonValue = {
      val bytes = ByteBuffer.allocate(16)
      //for UUID_STANDARD it should be:
      //bytes.order(ByteOrder.BIG_ENDIAN)
      //
      bytes.order(ByteOrder.LITTLE_ENDIAN)
      bytes.putLong(0, t.getMostSignificantBits)
      bytes.putLong(8, t.getLeastSignificantBits)

      new BsonBinary(BsonBinarySubType.UUID_LEGACY, bytes.array())
    }
    override def defaultValue: UUID = new UUID(0, 0)
  }

  implicit object LocalDateTimeBsonFormat extends BasicBsonFormat[LocalDateTime] {
    override def read(b: BsonValue): LocalDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(b.asDateTime().getValue), ZoneOffset.UTC)
    override def write(t: LocalDateTime): BsonValue = new BsonDateTime(t.toInstant(ZoneOffset.UTC).toEpochMilli)
    override def defaultValue: LocalDateTime = LocalDateTime.ofEpochSecond(0, 0, ZoneOffset.UTC)
  }

  implicit object InstantBsonFormat extends BasicBsonFormat[Instant] {
    override def read(b: BsonValue): Instant = Instant.ofEpochMilli(b.asDateTime().getValue)
    override def write(t: Instant): BsonValue = new BsonDateTime(t.toEpochMilli)
    override def defaultValue: Instant = Instant.ofEpochMilli(0)
  }

  implicit object TimeZoneFormat extends BasicBsonFormat[TimeZone] {
    override def read(b: BsonValue): TimeZone = TimeZone.getTimeZone(b.asString().getValue)
    override def write(tz: TimeZone): BsonValue = new BsonString(tz.getID)
    override def defaultValue: TimeZone = TimeZone.getTimeZone("UTC")
  }

  implicit def objectIdSubtypeFormat[Subtype <: ObjectId]: BasicBsonFormat[Subtype] = {
    new BasicBsonFormat[Subtype] {
      override def read(b: BsonValue): Subtype = ObjectIdBsonFormat.read(b).asInstanceOf[Subtype]
      override def defaultValue: Subtype = new ObjectId().asInstanceOf[Subtype]
      override def write(t: Subtype): BsonValue = ObjectIdBsonFormat.write(t)
    }
  }
}

trait BsonCollectionFormats {
  import scala.collection.JavaConverters._

  private[rogue]type BF[T] = BsonFormat[T]

  implicit def traversableLikeFormat[L[_], T: BsonFormat](
    implicit
    ev: L[T] <:< TraversableLike[T, L[T]],
    cb: CanBuildFrom[List[BsonValue], T, L[T]]
  ): BsonFormat[L[T]] = {

    new BsonFormat[L[T]] with BsonArrayReader[L[T]] {

      private[this] implicit val f = implicitly[BsonFormat[T]]

      def write(in: L[T]): BsonArray = {
        val b = ArrayBuffer[BsonValue]()
        in.foreach(el => b.append(f.write(el)))
        new BsonArray(b.result().asJava)
      }

      def read(value: BsonValue): L[T] = {
        val b = cb(List.empty[BsonValue])
        if (!value.isNull) {
          val arr = value.asArray()
          val i = arr.iterator()
          while (i.hasNext) {
            b.+=(f.read(i.next()))
          }
        }
        b.result()
      }

      override def flds: Map[String, BsonFormat[_]] = f.flds

      override def defaultValue: L[T] = List.empty[BsonValue].map(f.read)(cb)
    }
  }

  implicit def setFormat[T: BsonFormat]: BsonFormat[Set[T]] = new BsonFormat[Set[T]] with BsonArrayReader[Set[T]] {

    private[this] implicit val f = implicitly[BsonFormat[T]]

    def write(in: Set[T]): BsonArray = {
      new BsonArray(in.toSeq.map(f.write).asJava)
    }

    def read(value: BsonValue): Set[T] = {
      val list: Seq[BsonValue] = if (value.isNull) Seq.empty[BsonValue]
      else value.asArray().asScala
      list.map(f.read).toSet
    }

    override def flds: Map[String, BsonFormat[_]] = f.flds

    override def defaultValue: Set[T] = Set.empty[T]
  }

  implicit def arrayFormat[T: BsonFormat: ClassTag] = new BsonFormat[Array[T]] with BsonArrayReader[Array[T]] {
    implicit val f = implicitly[BsonFormat[T]]
    def write(array: Array[T]) = {
      val buff = new ArrayBuffer[BsonValue](array.length)
      array.foreach(e => buff += f.write(e))
      new BsonArray(buff.asJava)
    }
    def read(value: BsonValue) = {
      if (value.isNull) new Array[T](0)
      else {
        val arr = value.asArray()
        val b = new Array[T](arr.size())
        val it = arr.iterator()
        var idx = 0
        while (it.hasNext) {
          b(idx) = f.read(it.next())
          idx = idx + 1
        }
        b
      }
    }

    override def flds: Map[String, BF[_]] = f.flds

    override def defaultValue: Array[T] = Array.empty[T]
  }

  implicit def optionFormat[T: BF]: BF[Option[T]] = new OptionFormat[T]

  class OptionFormat[T: BsonFormat] extends BF[Option[T]] {
    implicit val f = implicitly[BsonFormat[T]]
    def write(option: Option[T]) = {
      option match {
        case Some(x) => f.write(x)
        case None => BsonNull.VALUE
      }
    }
    def read(value: BsonValue) = if (value.isNull) None
    else Option(f.read(value))

    override def readArray(b: BsonArray) = {
      val sb = Seq.newBuilder[Option[T]]
      val it = b.iterator()
      while (it.hasNext) {
        sb += read(it.next())
      }
      sb.result()
    }

    override def flds: Map[String, BF[_]] = f.flds

    override def defaultValue: Option[T] = None
  }

  implicit def mapFormat[K: BsonFormat, V: BsonFormat] = new BsonFormat[Map[K, V]] with BsonArrayReader[Map[K, V]] {
    implicit val fk = implicitly[BsonFormat[K]]
    implicit val fv = implicitly[BsonFormat[V]]
    def write(m: Map[K, V]) = {
      val doc = new BsonDocument()
      m.foreach {
        case (k, v) =>
          val kv = fk.write(k)
          val vv = fv.write(v)
          if (kv.isString && !vv.isNull) doc.append(kv.asString().getValue, vv)
      }
      doc
    }
    def read(value: BsonValue) = {
      value.asDocument().asScala.map {
        case (ks, v) =>
          (fk.read(new BsonString(ks)), fv.read(v))
      }(collection.breakOut)
    }
    //in general terms, yes, as we don't know keys, but we need 'star' format for values
    override def flds = Map("*" -> fv)

    override def defaultValue: Map[K, V] = Map.empty
  }

  //TODO - other collections - see SprayJson viaSeq
  /*

  import collection.{immutable => imm}
  def viaSeq[I <: Iterable[T], T :BsonFormat](f: imm.Seq[T] => I): BsonFormat[I] = new BsonFormat[I] {
    def write(iterable: I) = new BsonArray(iterable.map(_.toJson).toVector)
    def read(value: BsonValue) = if(value.isArray) {
      value.asArray().getValues
    } else {

      case JsArray(elements) => f(elements.map(_.convertTo[T]))
      case x => deserializationError("Expected Collection as JsArray, but got " + x)
    }
  }*/

}

trait StandardBsonFormats extends BaseBsonFormats with BsonCollectionFormats

object BsonFormats extends StandardBsonFormats with BsonFormats

trait BsonFormats extends LowPrioBsonFormats {
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

trait LowPrioBsonFormats {
  this: BaseBsonFormats with BsonFormats =>

  abstract class WrappedBsonFromat[Wrapped, SubRepr](implicit tpe: Typeable[Wrapped]) {
    def write(v: SubRepr): BsonValue
    def read(b: BsonValue): SubRepr
    def flds: Map[String, BsonFormat[_]]
  }

  implicit def hHilBsonFormat[Wrapped](implicit tpe: Typeable[Wrapped]): WrappedBsonFromat[Wrapped, HNil] = new WrappedBsonFromat[Wrapped, HNil] {
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
        val resolved: Value = try {
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
        val subfieldFlds = hs.flds.map { case (subfieldName, s) => (fieldName + "." + subfieldName -> s) }
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
    tpe: Typeable[T]): BsonFormat[T] = new BsonFormat[T] with BsonArrayReader[T] {
    override def write(t: T): BsonValue = sg.value.value.write(gen.to(t))
    override def read(b: BsonValue): T = gen.from(sg.value.value.read(b))

    override val flds = sg.value.value.flds

    /**
     * for nested case classes, with default constructors provides a default value
     * consider
     * {{{
     *   case class In(j:Int = 1)
     *   case class Out(i:In = In(), x: Int = 5)
     *   val f = BsonFormat[Out]
     * }}}
     * in such case default can be provided if 'i' is missing from parameter
     * as in example
     * {{{
     *   f.parse(new BsonDocument) == Out(In(1), 0 )
     * }}}
     * value for 'i' will be `In(1)` because In has default non-parameter constructor, but value for x will be 0
     * this is because currently only full missing values will be constructed - missing partial values will be
     * filled with type-default values 0 for Int in this example
     * @return default T
     */
    override def defaultValue: T = {
      try {
        gen.from(d().asInstanceOf[Repr])
      } catch {
        case _: Exception =>
          throw new NoDefaultFormatForDerivedException(s"Requested default value for but case class ${tpe.describe} has no default, non-parameter constructor")
      }
    }
  }

}

object BsonFormat {
  def apply[T](implicit f: Lazy[BsonFormat[T]]): BsonFormat[T] = f.value

}

//thrown for derived encoders - which can't have
class NoDefaultFormatForDerivedException(m: String) extends RuntimeException(m)