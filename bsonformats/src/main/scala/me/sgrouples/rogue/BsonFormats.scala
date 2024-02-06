package me.sgrouples.rogue
import org.bson.{BsonArray, BsonValue}
import scala.annotation.{StaticAnnotation, implicitNotFound}

trait BsonFormat[T] {
  def read(b: BsonValue): T

  def readArray(b: BsonArray): Seq[T]

  def write(t: T): BsonValue

  def flds: Map[String, BsonFormat[_]]

  def defaultValue: T
}

trait BsonArrayReader[T] {
  self: BsonFormat[T] =>
  override def readArray(b: BsonArray): Seq[T] = {
    val sb = Seq.newBuilder[T]
    val it = b.iterator()
    while (it.hasNext)
      sb += read(it.next())
    sb.result()
  }
}

trait BaseBsonFormat[T] extends BsonFormat[T] with BsonArrayReader[T]

trait BasicBsonFormat[T] extends BaseBsonFormat[T] {
  override val flds: Map[String, BsonFormat[_]] = Map.empty
}

class EnumSerializeValue extends StaticAnnotation
