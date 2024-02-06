package me.sgrouples.rogue.cc.macros
import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline, constValue}
import scala.reflect.*
import me.sgrouples.rogue.BsonFormat

object MacroDerivationInline {
  inline given derived[T](using m: Mirror.Of[T]): MacroBsonFormat[T] =
    val elemInstances = summonAllFormats[m.MirroredElemTypes] // (1)
    inline m match // (2)
      case s: Mirror.SumOf[T]     => bsonFormatSum(s, elemInstances)
      case p: Mirror.ProductOf[T] => bsonFormatProduct(p, elemInstances)

  inline def summonAllFormats[T <: Tuple]: List[MacroBsonFormat[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        summonInline[MacroBsonFormat[t]] :: summonAllFormats[ts]

  private def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

  private inline def summonProductElemNames[T <: Tuple]: List[String] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => constValue[t].toString :: summonProductElemNames[ts]
    }

  inline def bsonFormatProduct[T](
      s: Mirror.ProductOf[T],
      encoders: List[MacroBsonFormat[_]]
  ): MacroBsonFormat[T] = {
    val elemNames = summonProductElemNames[s.MirroredElemLabels]

    new MacroBsonFormat[T] {
      val _validNames = elemNames.toVector
      override def validNames(): Vector[String] = _validNames
      val _works = elemNames.zip(encoders)
      val _subFieldsAdd = _works.map { case (fn, ff) =>
        ff.subfields(fn, ff)
      }.flatten
      val _allFlds = _works ++ _subFieldsAdd.toList
      val _flds: Map[String, BsonFormat[?]] = _allFlds.map { case (k, v) =>
        k -> v.asInstanceOf[me.sgrouples.rogue.BsonFormat[?]]
      }.toMap
      override def defaultValue: T = throw RuntimeException("unknown default")

      override val flds: Map[String, me.sgrouples.rogue.BsonFormat[?]] = _flds

      override def read(b: _root_.org.bson.BsonValue): T = {
        if (b.isDocument()) {
          val doc = b.asDocument()

          val readVals = _works.map { case (fn, ff) =>
            ff.read(doc.get(fn))
          }
          s.fromProduct(Tuple.fromArray(readVals.toArray))
          //new $tpe(..$reads)
        } else {
          throw new RuntimeException("No data in read")
          //defaultValue
        }
      }

      override def write(t: T): _root_.org.bson.BsonValue = {
        val doc = new _root_.org.bson.BsonDocument()
        val it = iterator(t)
        _works.foreach { case (fn, ff) =>
          val v = it.next()
          addNotNull0(doc, fn, ff.asInstanceOf[MacroBsonFormat[Any]].write(v))
        }

        doc
      }

      private def appendVals(writer: _root_.org.bson.BsonWriter, t: T): Unit = {
        val it = iterator(t)
        _works.foreach { case (fn, ff) =>
          val v = it.next()
          ff.asInstanceOf[MacroBsonFormat[Any]].append(writer, fn, v)
        }
      }

      override def append(
          writer: _root_.org.bson.BsonWriter,
          k: String,
          t: T
      ): Unit = {
        writer.writeStartDocument(k)
        appendVals(writer, t)
        writer.writeEndDocument()
      }

      override def append(writer: _root_.org.bson.BsonWriter, t: T): Unit = {
        writer.writeStartDocument()
        appendVals(writer, t)
        writer.writeEndDocument()
      }

      private def addNotNull0(
          d: _root_.org.bson.BsonDocument,
          k: String,
          v: _root_.org.bson.BsonValue
      ): Unit = {
        if (!v.isNull()) { d.put(k, v) }
      }
    }

  }

  def bsonFormatSum[T](
      s: Mirror.SumOf[T],
      encoders: List[MacroBsonFormat[_]]
  ): MacroBsonFormat[T] =
    new MacroBsonFormat[T] {
      def defaultValue: T = ???
      def flds: Map[String, me.sgrouples.rogue.BsonFormat[?]] = ???
      def read(b: org.bson.BsonValue): T = ???
      def write(t: T): org.bson.BsonValue = ???
      def append(writer: org.bson.BsonWriter, k: String, v: T): Unit = ???
      def append(writer: org.bson.BsonWriter, v: T): Unit = ???
      def validNames(): Vector[String] = ???
    }

}
