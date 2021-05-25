package me.sgrouples.rogue

import org.bson.{ BsonArray, BsonValue }

import scala.collection.compat.Factory
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

trait IterableLikeFormats {
  implicit def IterableLikeFormat[L[_], T: BsonFormat](
    implicit
    ev: L[T] <:< Iterable[T],
    cb: Factory[T, L[T]]): BsonFormat[L[T]] = {

    new BsonFormat[L[T]] with BsonArrayReader[L[T]] {

      private[this] implicit val f = implicitly[BsonFormat[T]]

      def write(in: L[T]): BsonArray = {
        val b = ArrayBuffer[BsonValue]()
        in.foreach(el => b.append(f.write(el)))
        new BsonArray(b.asJava)
      }

      def read(value: BsonValue): L[T] = {
        val b = cb.newBuilder
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

      override def defaultValue: L[T] = {
        cb.fromSpecific(List.empty[BsonValue].map(f.read))
      }
    }
  }
}
