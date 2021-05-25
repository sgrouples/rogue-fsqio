package me.sgrouples.rogue

import me.sgrouples.rogue.map.{ MapKeyFormat, MapKeyFormats }
import org.bson.{ BsonArray, BsonDocument, BsonNull, BsonValue }

import scala.collection.compat._
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.language.{ higherKinds, implicitConversions }

trait BsonCollectionFormats extends IterableLikeFormats {
  requires: MapKeyFormats =>
  import scala.jdk.CollectionConverters._

  implicit def setFormat[T: BsonFormat]: BsonFormat[Set[T]] = new BsonFormat[Set[T]] with BsonArrayReader[Set[T]] {

    private[this] implicit val f = implicitly[BsonFormat[T]]

    def write(in: Set[T]): BsonArray = {
      new BsonArray(in.toSeq.map(f.write).asJava)
    }

    def read(value: BsonValue): Set[T] = {
      val list: Seq[BsonValue] = if (value.isNull) Seq.empty[BsonValue]
      else value.asArray().asScala.toSeq
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

    override def flds: Map[String, BsonFormat[_]] = f.flds

    override def defaultValue: Array[T] = Array.empty[T]
  }

  implicit def optionFormat[T: BsonFormat]: BsonFormat[Option[T]] = new OptionFormat[T]

  class OptionFormat[T: BsonFormat] extends BsonFormat[Option[T]] {
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

    override def flds: Map[String, BsonFormat[_]] = f.flds

    override def defaultValue: Option[T] = None
  }

  implicit def mapFormat[K: MapKeyFormat, V: BsonFormat]: BsonFormat[Map[K, V]] = {

    new BsonFormat[Map[K, V]] with BsonArrayReader[Map[K, V]] {

      implicit private val kf = implicitly[MapKeyFormat[K]]
      implicit private val fv = implicitly[BsonFormat[V]]

      def write(m: Map[K, V]): BsonDocument = {
        val doc = new BsonDocument()
        m.foreach {
          case (k, v) =>
            val kv = kf.write(k)
            val vv = fv.write(v)
            if (!vv.isNull) doc.append(kv, vv)
        }
        doc
      }

      def read(value: BsonValue): Map[K, V] = {
        value.asDocument().asScala.map {
          case (ks, v) =>
            (kf.read(ks), fv.read(v))
        }.toMap /*(collection.breakOut)*/
      }

      //in general terms, yes, as we don't know keys, but we need 'star' format for values
      override def flds = Map("*" -> fv)

      override def defaultValue: Map[K, V] = Map.empty
    }
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