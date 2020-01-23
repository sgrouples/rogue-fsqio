package me.sgrouples.rogue

import me.sgrouples.rogue.{ BsonArrayReader, BsonFormat }
import org.bson.{ BsonArray, BsonValue }
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom

trait TraversableLikeFormats {
  implicit def traversableLikeFormat[L[_], T: BsonFormat](
    implicit
    ev: L[T] <:< TraversableLike[T, L[T]],
    cb: CanBuildFrom[List[BsonValue], T, L[T]]): BsonFormat[L[T]] = {

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

}