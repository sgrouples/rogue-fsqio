package me.sgrouples.rogue

import org.bson.{ BsonArray, BsonValue }

import scala.annotation.{ StaticAnnotation, implicitNotFound }

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
  override def readArray(b: BsonArray): Seq[T] = {
    val sb = Seq.newBuilder[T]
    val it = b.iterator()
    while (it.hasNext) {
      sb += read(it.next())
    }
    sb.result()
  }
}

trait BasicBsonFormat[T] extends BsonFormat[T] with BsonArrayReader[T] {
  override def flds: Map[String, BsonFormat[_]] = Map.empty
}

class EnumSerializeValue extends StaticAnnotation