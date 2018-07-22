package me.sgrouples.rogue.cc.macros

import java.nio.{ ByteBuffer, ByteOrder }
import java.time.{ Instant, LocalDateTime, ZoneOffset }
import java.util.{ Currency, Locale, UUID }

import me.sgrouples.rogue.{ BasicBsonFormat, SupportedLocales }
import org.bson._
import org.bson.types.{ Decimal128, ObjectId }

import scala.annotation.implicitNotFound

@implicitNotFound("MacroGen can't generate for ${T}")
trait MacroBsonFormat[T] extends BasicBsonFormat[T] {
  def namesMap(): Vector[(Int, String)]
  def append(writer: BsonWriter, k: String, v: T): Unit
  def readOrDefault(v: BsonValue): T = {
    if (v != null) {
      read(v)
    } else defaultValue
  }
}

abstract class BaseBsonFormat[T] extends MacroBsonFormat[T] {
  override def namesMap(): Vector[(Int, String)] = Vector.empty
}

final class IntMacroBsonFormat(default: Int) extends BaseBsonFormat[Int] {
  override def read(b: BsonValue): Int = if (b.isNumber) {
    b.asNumber().intValue()
  } else {
    default
  }
  override def write(t: Int): BsonValue = new BsonInt32(t)
  override def defaultValue: Int = default
  override def append(writer: BsonWriter, k: String, v: Int): Unit = {
    writer.writeInt32(k, v)
  }
}

final class LongMacroBsonFormat(default: Long) extends BaseBsonFormat[Long] {
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
}

final class DoubleMacroBsonFormat(default: Double) extends BaseBsonFormat[Double] {
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
}

final class BigDecimalMacroBsonFormat(default: BigDecimal) extends BaseBsonFormat[BigDecimal] {
  override def read(b: BsonValue): BigDecimal = if (b.isDecimal128) {
    b.asDecimal128().decimal128Value().bigDecimalValue()
  } else {
    default
  }
  override def write(t: BigDecimal): BsonValue = new BsonDecimal128(new Decimal128(t.bigDecimal))
  override def defaultValue: BigDecimal = default
  override def append(writer: BsonWriter, k: String, v: BigDecimal): Unit = {
    writer.writeDecimal128(k, new Decimal128(v.bigDecimal))
  }
}

final class BooleanMacroBsonFormat(default: Boolean) extends BaseBsonFormat[Boolean] {
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
}

final class StringMacroBsonFormat(default: String) extends BaseBsonFormat[String] {
  override def read(b: BsonValue): String = if (b.isString) b.asString().getValue else default
  override def write(t: String): BsonValue = new BsonString(t)
  override def defaultValue: String = default
  override def append(writer: BsonWriter, k: String, v: String): Unit = {
    writer.writeString(k, v)
  }
}

final class ObjectIdMacroBsonFormat(default: ObjectId) extends BaseBsonFormat[ObjectId] {
  override def read(b: BsonValue): ObjectId = if (b.isObjectId) b.asObjectId().getValue else default
  override def write(t: ObjectId): BsonValue = new BsonObjectId(t)
  override def defaultValue: ObjectId = default
  override def append(writer: BsonWriter, k: String, v: ObjectId): Unit = {
    writer.writeObjectId(k, v)
  }
}

final class UUIDMacroBsonFormat(default: UUID) extends BaseBsonFormat[UUID] {
  override def read(b: BsonValue): UUID = {
    if (b.isBinary) {
      val bin = b.asBinary()
      val bb = ByteBuffer.wrap(bin.getData)
      if (bin.getType == BsonBinarySubType.UUID_LEGACY.getValue)
        bb.order(ByteOrder.LITTLE_ENDIAN)
      else
        bb.order(ByteOrder.BIG_ENDIAN)
      new UUID(bb.getLong(0), bb.getLong(8))
    } else default
  }
  override def write(t: UUID): BsonValue = {
    toBinary(t)
  }

  private def toBinary(t: UUID): BsonBinary = {
    val bytes = ByteBuffer.allocate(16)
    bytes.order(ByteOrder.LITTLE_ENDIAN)
    bytes.putLong(0, t.getMostSignificantBits)
    bytes.putLong(8, t.getLeastSignificantBits)
    new BsonBinary(BsonBinarySubType.UUID_LEGACY, bytes.array())
  }

  override def defaultValue: UUID = default
  override def append(writer: BsonWriter, k: String, v: UUID): Unit = {
    writer.writeBinaryData(k, toBinary(v))
  }
}

final class CurrencyMacroBsonFormat(default: Currency) extends BaseBsonFormat[Currency] {
  override def read(b: BsonValue): Currency = if (b.isString)
    Currency.getInstance(b.asString().getValue)
  else defaultValue
  override def write(t: Currency): BsonValue = new BsonString(t.getCurrencyCode)
  override def defaultValue: Currency = default
  override def append(writer: BsonWriter, k: String, v: Currency): Unit = {
    writer.writeString(k, v.getCurrencyCode)
  }
}
final class LocaleMacroBsonFormat(default: Locale) extends BaseBsonFormat[Locale] {
  override def read(b: BsonValue): Locale = if (b.isString) {
    val value = b.asString().getValue
    if (value.isEmpty) defaultValue
    else {
      if ("en" == value) Locale.ENGLISH
      else SupportedLocales.map.get(value).getOrElse(defaultValue)
    }
  } else defaultValue
  override def write(t: Locale): BsonValue = new BsonString(t.toString)
  override def defaultValue: Locale = default
  override def append(writer: BsonWriter, k: String, v: Locale): Unit = {
    writer.writeString(k, v.toString)
  }
}

final class BinaryMacroBsonFormat extends BaseBsonFormat[Array[Byte]] {
  override def read(b: BsonValue): Array[Byte] = if (b.isBinary)
    b.asBinary().getData
  else defaultValue
  override def write(t: Array[Byte]): BsonValue = new BsonBinary(t)
  override def defaultValue: Array[Byte] = Array.empty
  override def append(writer: BsonWriter, k: String, v: Array[Byte]): Unit = {
    writer.writeString(k, v.toString)
  }
}

final class LocalDateTimeMacroBsonFormat(default: LocalDateTime) extends BaseBsonFormat[LocalDateTime] {
  override def read(b: BsonValue): LocalDateTime = {
    if (b.isDateTime) {
      LocalDateTime.ofInstant(Instant.ofEpochMilli(b.asDateTime().getValue), ZoneOffset.UTC)
    } else defaultValue
  }
  override def write(t: LocalDateTime): BsonValue = new BsonDateTime(t.toInstant(ZoneOffset.UTC).toEpochMilli)
  override def defaultValue: LocalDateTime = default
  override def append(writer: BsonWriter, k: String, v: LocalDateTime): Unit = {
    writer.writeDateTime(k, v.toInstant(ZoneOffset.UTC).toEpochMilli)
  }
}

final class InstantMacroBsonFormat(default: Instant) extends BaseBsonFormat[Instant] {
  override def read(b: BsonValue): Instant = {
    if (b.isDateTime) {
      Instant.ofEpochMilli(b.asDateTime().getValue)
    } else defaultValue
  }
  override def write(t: Instant): BsonValue = new BsonDateTime(t.toEpochMilli)
  override def defaultValue: Instant = default
  override def append(writer: BsonWriter, k: String, v: Instant): Unit = {
    writer.writeDateTime(k, v.toEpochMilli)
  }
}

class OptMacroBsonFormat[T](inner: MacroBsonFormat[T]) extends BaseBsonFormat[Option[T]] {
  override def read(b: BsonValue): Option[T] = if (b == null) None else Option(inner.readOrDefault(b))
  override def write(t: Option[T]): BsonValue = if (t.isDefined) inner.write(t.get) else BsonNull.VALUE
  override def defaultValue: Option[T] = None
  override def append(writer: BsonWriter, k: String, v: Option[T]): Unit = {
    if (v.isDefined) inner.append(writer, k, v.get)
  }
}

trait EnumMacroFormats {

  def enumNameMacroFormat[T <: Enumeration](e: T): MacroBsonFormat[T#Value] = new BaseBsonFormat[T#Value] {
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
  }

  def enumValueMacroFormat[T <: Enumeration](e: T): MacroBsonFormat[T#Value] = new BaseBsonFormat[T#Value] {
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
  }

}

trait ObjectIdSubtypeFormats {

}

object EnumMacroFormats extends EnumMacroFormats
