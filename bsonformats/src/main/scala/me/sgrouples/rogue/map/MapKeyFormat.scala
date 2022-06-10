package me.sgrouples.rogue.map

import scala.annotation.implicitNotFound
import org.bson.types.ObjectId

@implicitNotFound(
  "Please provide a valid, implicit MapKeyFormat[${T}] when using map bson format for Map[${T}, _]"
)
trait MapKeyFormat[T] {

  def read(s: String): T
  def write(t: T): String
}

object MapKeyFormat {

  private[map] def defaultWriter[T](t: T): String = t.toString

  def apply[T](
      reader: String => T,
      writer: T => String = defaultWriter _
  ): MapKeyFormat[T] = new DefaultMapKeyFormat(reader, writer)

  given MapKeyFormat[String] = DefaultMapKeyFormat[String](identity)
  given MapKeyFormat[Long] = DefaultMapKeyFormat[Long](_.toLong)
  given MapKeyFormat[Int] = DefaultMapKeyFormat[Int](_.toInt)
  given ObjectIdMapKeyFormat: MapKeyFormat[ObjectId] = DefaultMapKeyFormat[ObjectId](new ObjectId(_))
  implicit def objectIdSubtypeMapKeyFormat[S <: ObjectId]: MapKeyFormat[S] =
    apply[S](ObjectIdMapKeyFormat.read(_).asInstanceOf[S])
}

class DefaultMapKeyFormat[T](
    val reader: String => T,
    val writer: T => String = MapKeyFormat.defaultWriter _
) extends MapKeyFormat[T] {

  override def read(s: String): T = reader(s)
  override def write(t: T): String = writer(t)
}
