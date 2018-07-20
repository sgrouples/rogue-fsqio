package me.sgrouples.rogue.cc.macros

import me.sgrouples.rogue.BasicBsonFormat
import org.bson.{ BsonInt32, BsonString, BsonValue, BsonWriter }

import scala.annotation.implicitNotFound

@implicitNotFound("MacroGen can't generate for ${T}")
trait MacroBsonFormat[T] extends BasicBsonFormat[T] {
  def namesMap(): Vector[(Int, String)]
  def append(writer: BsonWriter, k: String, v: T): Unit
}

abstract class BaseBsonFormat[T] extends MacroBsonFormat[T] {
  override def namesMap(): Vector[(Int, String)] = Vector.empty
}

final class IntMacroBsonFormat(default: Int) extends BaseBsonFormat[Int] {
  override def read(b: BsonValue): Int = b.asNumber().intValue()

  override def write(t: Int): BsonValue = new BsonInt32(t)

  override def defaultValue: Int = default

  override def append(writer: BsonWriter, k: String, v: Int): Unit = {
    writer.writeInt32(k, v)
  }
}

final class StringMacroBsonFormat(default: String) extends BaseBsonFormat[String] {
  override def read(b: BsonValue): String = b.asString().getValue

  override def write(t: String): BsonValue = new BsonString(t)

  override def defaultValue: String = default

  override def append(writer: BsonWriter, k: String, v: String): Unit = {
    writer.writeString(k, v)
  }
}

trait EnumMacroFormats {
  def enumValueMacroFormat[T <: Enumeration](e: T, default: T#Value): BaseBsonFormat[T#Value] = new BaseBsonFormat[T#Value] {
    override def read(b: BsonValue): T#Value = {
      if (b.isString) e.withName(b.asString().getValue)
      else e.apply(b.asNumber().intValue())
    }

    override def write(t: T#Value): BsonValue = new BsonString(t.toString)

    override def defaultValue: T#Value = default

    override def append(writer: BsonWriter, k: String, v: T#Value): Unit = {
      writer.writeString(k, v.toString)
    }
  }

  def dummyEnumFmt[T <: Enumeration](e: T): BaseBsonFormat[T#Value] = new BaseBsonFormat[T#Value] {
    override def read(b: BsonValue): T#Value = {
      if (b.isString) e.withName(b.asString().getValue)
      else e.apply(b.asNumber().intValue())
    }

    override def write(t: T#Value): BsonValue = new BsonString(t.toString)

    override def defaultValue: T#Value = ???

    override def append(writer: BsonWriter, k: String, v: T#Value): Unit = {
      writer.writeString(k, v.toString)
    }
  }
}

object EnumMacroFormats extends EnumMacroFormats

final class EnumValueMacroBsonFormat(default: String) extends BaseBsonFormat[String] {
  override def read(b: BsonValue): String = b.asString().getValue

  override def write(t: String): BsonValue = new BsonString(t)

  override def defaultValue: String = default

  override def append(writer: BsonWriter, k: String, v: String): Unit = {
    writer.writeString(k, v)
  }
}

