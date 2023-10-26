package me.sgrouples.rogue.cc.macros

import java.nio.{ByteBuffer, ByteOrder}
import java.time.{Instant, LocalDateTime, ZoneOffset}
import java.util.{Currency, Locale, TimeZone, UUID}
import me.sgrouples.rogue.map.MapKeyFormat
import me.sgrouples.rogue.{BaseBsonFormat, BsonFormat, SupportedLocales}
import org.bson._
import org.bson.types.{Decimal128, ObjectId}

import scala.collection.Factory
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import enumeratum._
import enumeratum.values.ValueEnumEntry

@implicitNotFound("MacroGen can't generate for ${T}")
trait MacroBsonFormat[T] extends BaseBsonFormat[T] {
  def validNames(): Vector[String]
  def append(writer: BsonWriter, k: String, v: T): Unit
  def append(writer: BsonWriter, v: T): Unit
  def readOrDefault(v: BsonValue): T = {
    if (v != null) {
      read(v)
    } else defaultValue
  }
  protected def addNotNull(
      d: _root_.org.bson.BsonDocument,
      k: String,
      v: _root_.org.bson.BsonValue
  ): Unit = {
    if (!v.isNull()) { d.put(k, v) }
  }
  protected def subfields(
      prefix: String,
      f: BsonFormat[_]
  ): Seq[(String, BsonFormat[_])] = {
    f.flds.toSeq.flatMap { case (k, v) =>
      Seq((s"$prefix.$k", v)) ++ subfields(s"$prefix.$k", v)
    }
  }
}

abstract class MacroBaseBsonFormat[T] extends MacroBsonFormat[T] {
  override def flds: Map[String, BsonFormat[_]] = Map.empty
  override def validNames(): Vector[String] = Vector.empty
}

final class IntMacroBsonFormat(default: Int = 0)
    extends MacroBaseBsonFormat[Int] {
  override def read(b: BsonValue): Int = if (b.isNumber) {
    b.asNumber().intValue()
  } else {
    default
  }
  override def write(t: Int): BsonValue = new BsonInt32(t)
  override def defaultValue: Int = default
  override def append(writer: BsonWriter, k: String, v: Int): Unit =
    writer.writeInt32(k, v)
  override def append(writer: BsonWriter, v: Int): Unit = writer.writeInt32(v)

}

final class LongMacroBsonFormat(default: Long = 0L)
    extends MacroBaseBsonFormat[Long] {
  override def read(b: BsonValue): Long = if (b.isNumber) {
    b.asNumber().longValue()
  } else {
    default
  }
  override def write(t: Long): BsonValue = new BsonInt64(t)
  override def defaultValue: Long = default
  override def append(writer: BsonWriter, k: String, v: Long): Unit = {
    writer.writeInt64(k, v)
  }
  override def append(writer: BsonWriter, v: Long): Unit = {
    writer.writeInt64(v)
  }
}

final class DoubleMacroBsonFormat(default: Double = 0.0d)
    extends MacroBaseBsonFormat[Double] {
  override def read(b: BsonValue): Double = if (b.isNumber) {
    b.asNumber().doubleValue()
  } else {
    default
  }
  override def write(t: Double): BsonValue = new BsonDouble(t)
  override def defaultValue: Double = default
  override def append(writer: BsonWriter, k: String, v: Double): Unit = {
    writer.writeDouble(k, v)
  }
  override def append(writer: BsonWriter, v: Double): Unit = {
    writer.writeDouble(v)
  }
}

final class BigDecimalMacroBsonFormat()
    extends MacroBaseBsonFormat[BigDecimal] {
  override def read(b: BsonValue): BigDecimal = if (b.isDecimal128) {
    b.asDecimal128().decimal128Value().bigDecimalValue()
  } else {
    defaultValue
  }
  override def write(t: BigDecimal): BsonValue = new BsonDecimal128(
    new Decimal128(t.bigDecimal)
  )
  override def defaultValue: BigDecimal = BigDecimal(0)
  override def append(writer: BsonWriter, k: String, v: BigDecimal): Unit = {
    writer.writeDecimal128(k, new Decimal128(v.bigDecimal))
  }
  override def append(writer: BsonWriter, v: BigDecimal): Unit = {
    writer.writeDecimal128(new Decimal128(v.bigDecimal))
  }
}

final class BooleanMacroBsonFormat(default: Boolean = false)
    extends MacroBaseBsonFormat[Boolean] {
  override def read(b: BsonValue): Boolean = if (b.isBoolean) {
    b.asBoolean().getValue
  } else {
    default
  }
  override def write(t: Boolean): BsonValue = new BsonBoolean(t)
  override def defaultValue: Boolean = default
  override def append(writer: BsonWriter, k: String, v: Boolean): Unit = {
    writer.writeBoolean(k, v)
  }
  override def append(writer: BsonWriter, v: Boolean): Unit = {
    writer.writeBoolean(v)
  }
}

final class StringMacroBsonFormat[T <: String](default: T = "")
    extends MacroBaseBsonFormat[T] {
  override def read(b: BsonValue): T =
    if (b.isString) b.asString().getValue.asInstanceOf[T] else default
  override def write(t: T): BsonValue = new BsonString(t)
  override def defaultValue: T = default
  override def append(writer: BsonWriter, k: String, v: T): Unit = {
    writer.writeString(k, v)
  }
  override def append(writer: BsonWriter, v: T): Unit = {
    writer.writeString(v)
  }
}

private object ObjectIdZero {
  val zero = new ObjectId(Array.fill[Byte](12)(0))
}

final class ObjectIdMacroBsonFormat[T <: ObjectId](
    default: T = ObjectIdZero.zero.asInstanceOf[T]
) extends MacroBaseBsonFormat[T] {
  override def read(b: BsonValue): T =
    if (b.isObjectId) b.asObjectId().getValue.asInstanceOf[T] else default
  override def write(t: T): BsonValue = new BsonObjectId(t)
  override def defaultValue: T = default
  override def append(writer: BsonWriter, k: String, v: T): Unit = {
    writer.writeObjectId(k, v)
  }
  override def append(writer: BsonWriter, v: T): Unit = {
    writer.writeObjectId(v)
  }
}

final class UUIDMacroBsonFormat[T <: UUID]() extends MacroBaseBsonFormat[T] {
  override def read(b: BsonValue): T = {
    if (b.isBinary) {
      val bin = b.asBinary()
      val bb = ByteBuffer.wrap(bin.getData)
      if (bin.getType == BsonBinarySubType.UUID_LEGACY.getValue)
        bb.order(ByteOrder.LITTLE_ENDIAN)
      else
        bb.order(ByteOrder.BIG_ENDIAN)
      new UUID(bb.getLong(0), bb.getLong(8)).asInstanceOf[T]
    } else defaultValue
  }
  override def write(t: T): BsonValue = {
    toBinary(t)
  }

  private def toBinary(t: T): BsonBinary = {
    val bytes = ByteBuffer.allocate(16)
    bytes.order(ByteOrder.LITTLE_ENDIAN)
    bytes.putLong(0, t.getMostSignificantBits)
    bytes.putLong(8, t.getLeastSignificantBits)
    new BsonBinary(BsonBinarySubType.UUID_LEGACY, bytes.array())
  }

  override def defaultValue: T = new UUID(0, 0).asInstanceOf[T]
  override def append(writer: BsonWriter, k: String, v: T): Unit = {
    writer.writeBinaryData(k, toBinary(v))
  }
  override def append(writer: BsonWriter, v: T): Unit = {
    writer.writeBinaryData(toBinary(v))
  }
}

final class CurrencyMacroBsonFormat extends MacroBaseBsonFormat[Currency] {
  override def read(b: BsonValue): Currency = if (b.isString)
    Currency.getInstance(b.asString().getValue)
  else defaultValue
  override def write(t: Currency): BsonValue = new BsonString(t.getCurrencyCode)
  override def defaultValue: Currency = Currency.getInstance("USD")
  override def append(writer: BsonWriter, k: String, v: Currency): Unit = {
    writer.writeString(k, v.getCurrencyCode)
  }
  override def append(writer: BsonWriter, v: Currency): Unit = {
    writer.writeString(v.getCurrencyCode)
  }
}
final class LocaleMacroBsonFormat extends MacroBaseBsonFormat[Locale] {
  override def read(b: BsonValue): Locale = if (b.isString) {
    val value = b.asString().getValue
    if (value.isEmpty) defaultValue
    else {
      if ("en" == value) Locale.ENGLISH
      else SupportedLocales.map.get(value).getOrElse(defaultValue)
    }
  } else defaultValue
  override def write(t: Locale): BsonValue = new BsonString(t.toString)
  override def defaultValue: Locale = Locale.ENGLISH
  override def append(writer: BsonWriter, k: String, v: Locale): Unit = {
    writer.writeString(k, v.toString)
  }
  override def append(writer: BsonWriter, v: Locale): Unit = {
    writer.writeString(v.toString)
  }
}

final class BinaryMacroBsonFormat extends MacroBaseBsonFormat[Array[Byte]] {
  override def read(b: BsonValue): Array[Byte] = if (b.isBinary)
    b.asBinary().getData
  else defaultValue
  override def write(t: Array[Byte]): BsonValue = new BsonBinary(t)
  override def defaultValue: Array[Byte] = Array.empty
  override def append(writer: BsonWriter, k: String, v: Array[Byte]): Unit = {
    writer.writeString(k, v.toString)
  }
  override def append(writer: BsonWriter, v: Array[Byte]): Unit = {
    writer.writeString(v.toString)
  }
}

final class LocalDateTimeMacroBsonFormat
    extends MacroBaseBsonFormat[LocalDateTime] {
  override def read(b: BsonValue): LocalDateTime = {
    if (b.isDateTime) {
      LocalDateTime.ofInstant(
        Instant.ofEpochMilli(b.asDateTime().getValue),
        ZoneOffset.UTC
      )
    } else defaultValue
  }
  override def write(t: LocalDateTime): BsonValue = new BsonDateTime(
    t.toInstant(ZoneOffset.UTC).toEpochMilli
  )
  override def defaultValue: LocalDateTime =
    LocalDateTime.ofEpochSecond(0, 0, ZoneOffset.UTC)
  override def append(writer: BsonWriter, k: String, v: LocalDateTime): Unit = {
    writer.writeDateTime(k, v.toInstant(ZoneOffset.UTC).toEpochMilli)
  }
  override def append(writer: BsonWriter, v: LocalDateTime): Unit = {
    writer.writeDateTime(v.toInstant(ZoneOffset.UTC).toEpochMilli)
  }
}

final class TimeZoneMacroBsonFormat extends MacroBaseBsonFormat[TimeZone] {
  override def read(b: BsonValue): TimeZone =
    TimeZone.getTimeZone(b.asString().getValue)
  override def write(tz: TimeZone): BsonValue = new BsonString(tz.getID)
  override def defaultValue: TimeZone =
    TimeZone.getTimeZone("UTC")
  override def append(writer: BsonWriter, k: String, v: TimeZone): Unit =
    writer.writeString(k, v.getID())
  override def append(writer: BsonWriter, v: TimeZone): Unit =
    writer.writeString(v.getID())
}

final class InstantMacroBsonFormat(default: Instant = Instant.ofEpochMilli(0))
    extends MacroBaseBsonFormat[Instant] {
  override def read(b: BsonValue): Instant = {
    if (b.isDateTime) {
      Instant.ofEpochMilli(b.asDateTime().getValue)
    } else if (b.isNumber) {
      Instant.ofEpochMilli(b.asNumber().longValue())
    } else defaultValue
  }
  override def write(t: Instant): BsonValue = new BsonDateTime(t.toEpochMilli)
  override def defaultValue: Instant = default
  override def append(writer: BsonWriter, k: String, v: Instant): Unit = {
    writer.writeDateTime(k, v.toEpochMilli)
  }
  override def append(writer: BsonWriter, v: Instant): Unit = {
    writer.writeDateTime(v.toEpochMilli)
  }
}

class OptMacroBsonFormat[T](inner: MacroBsonFormat[T])
    extends MacroBsonFormat[Option[T]] {
  override def read(b: BsonValue): Option[T] = if (b == null) None
  else {
    Option(inner.read(b))
  }
  override def write(t: Option[T]): BsonValue =
    if (t.isDefined) inner.write(t.get) else BsonNull.VALUE
  override def defaultValue: Option[T] = None
  override def append(writer: BsonWriter, k: String, v: Option[T]): Unit = {
    if (v.isDefined) inner.append(writer, k, v.get)
  }
  override def append(writer: BsonWriter, v: Option[T]): Unit = {
    if (v.isDefined) inner.append(writer, v.get)
  }
  override def validNames(): Vector[String] = inner.validNames()

  override def flds: Map[String, BsonFormat[_]] = inner.flds
}

trait EnumMacroFormats {

  def enumNameMacroFormat[T <: Enumeration](e: T): MacroBsonFormat[T#Value] =
    new MacroBaseBsonFormat[T#Value] {
      override def read(b: BsonValue): T#Value = {
        if (b.isString) e.withName(b.asString().getValue)
        else if (b.isNumber) e.apply(b.asNumber().intValue())
        else defaultValue
      }
      override def defaultValue: T#Value = {
        e.values.head
      }
      override def write(t: T#Value): BsonValue = new BsonString(t.toString)
      override def append(writer: BsonWriter, k: String, v: T#Value): Unit = {
        writer.writeString(k, v.toString)
      }
      override def append(writer: BsonWriter, v: T#Value): Unit = {
        writer.writeString(v.toString)
      }
    }

  def enumValueMacroFormat[T <: Enumeration](e: T): MacroBsonFormat[T#Value] =
    new MacroBaseBsonFormat[T#Value] {
      override def read(b: BsonValue): T#Value = {
        if (b.isNumber) e.apply(b.asNumber().intValue())
        else if (b.isString) e.withName(b.asString().getValue)
        else defaultValue
      }
      override def defaultValue: T#Value = e.values.head
      override def write(t: T#Value): BsonValue = new BsonInt32(t.id)
      override def append(writer: BsonWriter, k: String, v: T#Value): Unit = {
        writer.writeInt32(k, v.id)
      }
      override def append(writer: BsonWriter, v: T#Value): Unit = {
        writer.writeInt32(v.id)
      }
    }

}

trait EnumeratumMacroFormats {

  def enumMacroFormat[T <: EnumEntry](e: Enum[T]): MacroBsonFormat[T] =
    new MacroBaseBsonFormat[T] {
      override def read(b: BsonValue): T = {
        if (b.isString) e.withName(b.asString().getValue)
        else if (b.isNumber) e.values(b.asNumber().intValue())
        else defaultValue
      }
      override def defaultValue: T = e.values.head
      override def write(t: T): BsonValue = new BsonString(t.entryName)
      override def append(writer: BsonWriter, k: String, v: T): Unit = {
        writer.writeString(k, v.entryName)
      }
      override def append(writer: BsonWriter, v: T): Unit = {
        writer.writeString(v.entryName)
      }
    }
}

class IterableLikeMacroFormat[T, C <: Iterable[T]](inner: MacroBsonFormat[T])(
    implicit f: Factory[T, C]
) extends MacroBsonFormat[C] {
  private def appendVals(writer: BsonWriter, v: C) = {
    val it = v.iterator
    while (it.hasNext) {
      inner.append(writer, it.next())
    }
  }

  override def append(writer: BsonWriter, k: String, v: C): Unit = {
    writer.writeStartArray(k)
    appendVals(writer, v)
    writer.writeEndArray()
  }

  override def append(writer: BsonWriter, v: C): Unit = {
    writer.writeStartArray()
    appendVals(writer, v)
    writer.writeEndArray()
  }

  override def read(b: BsonValue): C = {
    if (b.isArray) {
      val arr = b.asArray()

      val it = arr.iterator()
      val builder = f.newBuilder
      while (it.hasNext) {
        builder.+=(inner.readOrDefault(it.next()))
      }
      builder.result()
    } else defaultValue
  }

  override def write(t: C): BsonValue = {
    val it = t.iterator
    val arr = new BsonArray()
    while (it.hasNext) {
      val t = it.next()
      arr.add(inner.write(t))
    }
    arr
  }

  override def defaultValue: C = {
    f.newBuilder.result()
  }

  override def validNames(): Vector[String] = inner.validNames()

  override def flds: Map[String, BsonFormat[_]] = inner.flds
}

class ArrayMacroBsonFormat[T: ClassTag](inner: MacroBsonFormat[T])
    extends MacroBsonFormat[Array[T]] {
  private def appendVals(writer: BsonWriter, v: Array[T]) = {
    val it = v.iterator
    while (it.hasNext) {
      inner.append(writer, it.next())
    }
  }

  override def append(writer: BsonWriter, k: String, v: Array[T]): Unit = {
    writer.writeStartArray(k)
    appendVals(writer, v)
    writer.writeEndArray()
  }

  override def append(writer: BsonWriter, v: Array[T]): Unit = {
    writer.writeStartArray()
    appendVals(writer, v)
    writer.writeEndArray()
  }

  override def read(b: BsonValue): Array[T] = {
    if (b.isArray) {
      val arr = b.asArray()
      val result = new Array[T](arr.size())
      val it = arr.iterator()
      var idx = 0
      while (it.hasNext) {
        result(idx) = inner.readOrDefault(it.next())
        idx = idx + 1
      }
      result
    } else defaultValue
  }

  override def write(t: Array[T]): BsonValue = {
    val it = t.iterator
    val arr = new BsonArray()
    while (it.hasNext) {
      val t = it.next()
      arr.add(inner.write(t))
    }
    arr
  }

  override def defaultValue: Array[T] = Array.empty[T]

  override def validNames(): Vector[String] = inner.validNames()

  override def flds: Map[String, BsonFormat[_]] = inner.flds
}
class MapMacroFormat[K, T](inner: MacroBsonFormat[T])(implicit
    kf: MapKeyFormat[K]
) extends MacroBsonFormat[Map[K, T]] {
  import scala.jdk.CollectionConverters._
  private def appendVals(writer: BsonWriter, v: Map[K, T]): Unit = {
    v.foreach { case (k, v) =>
      inner.append(writer, kf.write(k), v)
    }
  }

  override def append(writer: BsonWriter, k: String, v: Map[K, T]): Unit = {
    writer.writeStartDocument(k)
    appendVals(writer, v)
    writer.writeEndDocument()
  }

  override def append(writer: BsonWriter, v: Map[K, T]): Unit = {
    writer.writeStartDocument()
    appendVals(writer, v)
    writer.writeEndDocument()
  }

  override def read(b: BsonValue): Map[K, T] = {
    if (b.isDocument) {
      val doc = b.asDocument()
      doc.asScala.map { case (k, v) =>
        (kf.read(k), inner.read(v))
      }.toMap /*(collection.breakOut)*/
    } else defaultValue
  }

  override def write(t: Map[K, T]): BsonValue = {
    val doc = new BsonDocument()
    t.foreach { case (k, v) => doc.append(kf.write(k), inner.write(v)) }
    doc
  }

  override def defaultValue: Map[K, T] = Map.empty

  override def validNames(): Vector[String] = inner.validNames()

  override def flds: Map[String, BsonFormat[_]] = Map("*" -> inner)
}

object EnumMacroFormats extends EnumMacroFormats

object EnumeratumMacroFormats extends EnumeratumMacroFormats
