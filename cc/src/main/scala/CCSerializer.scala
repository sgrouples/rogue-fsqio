package me.sgrouples.rogue

import shapeless._
import labelled.{FieldType, field}
import org.bson._
import org.bson.types.ObjectId

import scala.annotation.implicitNotFound
import scala.language.implicitConversions
import scala.language.higherKinds

@implicitNotFound("implicit BsonFormat not found for ${T}")
trait BsonFormat[T] {
 // def read(b: BsonValue): T
  def write(t:T): BsonValue
}

/**
  * Basic bson serializers
  */
trait BaseBsonFormats{

  private[rogue] type JF[T] = BsonFormat[T] // simple alias for reduced verbosity

  implicit object IntBsonFormat extends BsonFormat[Int] {
    //override def read(b: BsonValue): Int = b.asNumber().intValue()
    override def write(t: Int): BsonValue = new BsonInt32(t)
  }

  implicit object LongBsonFormat extends BsonFormat[Long] {
    //override def read(b: BsonValue): Long = b.asNumber().longValue()
    override def write(t: Long): BsonValue = new BsonInt64(t)
  }

  implicit object StringBsonFormat extends BsonFormat[String] {
    //override def read(b: BsonValue): String = b.asString().getValue()
    override def write(t: String): BsonValue = new BsonString(t)
  }

  implicit object ObjectIdBsonFormat extends BsonFormat[ObjectId] {
    //override def read(b: BsonValue): ObjectId = b.asObjectId().getValue()
    override def write(t: ObjectId): BsonValue = new BsonObjectId(t)
  }

}

object BsonFormats extends BaseBsonFormats with BsonFormats


trait BsonFormats extends LowPrioBsonFormats {
  this: BaseBsonFormats =>
}
/*
trait FamilyFormats extends LowPriorityFamilyFormats {
  this: StandardFormats =>

  // scala compiler doesn't like spray-json's use of a type alias in the sig
  override implicit def optionFormat[T: JsonFormat]: JsonFormat[Option[T]] = new OptionFormat[T]
}
object FamilyFormats extends DefaultJsonProtocol with FamilyFormats
 */


trait LowPrioBsonFormats {
  this: BaseBsonFormats with BsonFormats =>

  abstract class WrappedBsonFromat[Wrapped, SubRepr](implicit tpe :Typeable[Wrapped]) {
    def write(v: SubRepr): BsonValue
  }

  implicit def hHilBsonFormat[Wrapped](implicit tpe: Typeable[Wrapped]): WrappedBsonFromat[Wrapped, HNil] = new WrappedBsonFromat[Wrapped, HNil]{
    override def write(t: HNil): BsonValue = BsonNull.VALUE
  }


  implicit def hListFormat[Wrapped, Key <: Symbol, Value, Remaining <: HList](
                                                                               implicit
                                                                               t: Typeable[Wrapped],
                                                                               key: Witness.Aux[Key],
                                                                               headSer: Lazy[BsonFormat[Value]],
                                                                               remFormat: WrappedBsonFromat[Wrapped, Remaining]
                                                                             ): WrappedBsonFromat[Wrapped, FieldType[Key, Value] :: Remaining] =
   new WrappedBsonFromat[Wrapped, FieldType[Key, Value] :: Remaining] {
     println(s"Creating format ${t.describe}")
     override def write(ft: FieldType[Key, Value] :: Remaining): BsonValue = {
       val rest = remFormat.write(ft.tail)
       val serializedVal = headSer.value.write(ft.head)
       if(!serializedVal.isNull) {
         val fName = key.value.name
         if(rest.isNull) {
           new BsonDocument(fName, serializedVal)
         } else {
           rest.asDocument().append(fName, serializedVal)
         }
       } else {
         rest
       }
     }
   }

  implicit def cNilFormat[Wrapped](
                                    implicit
                                    t: Typeable[Wrapped]
                                  ): WrappedBsonFromat[Wrapped, CNil] = new WrappedBsonFromat[Wrapped, CNil] {
    def write(c: CNil) = throw new RuntimeException("write should never be called for CNil")
  }

  implicit def coproductFormat[Wrapped, Name <: Symbol, Instance, Remaining <: Coproduct](
  implicit
  tpe: Typeable[Wrapped],
  key: Witness.Aux[Name],
  jfh: Lazy[BsonFormat[Instance]],
  jft: WrappedBsonFromat[Wrapped, Remaining]
  ): WrappedBsonFromat[Wrapped, FieldType[Name, Instance] :+: Remaining] =
    new WrappedBsonFromat[Wrapped, FieldType[Name, Instance] :+: Remaining] {
      def write(lr: FieldType[Name, Instance] :+: Remaining) = lr match {
        case Inl(l) =>
          val serV = jfh.value.write(l)
          if(serV.isDocument) {
            //            case j: JsObject => th.write(j, key.value)
            val v = key.value
            println(s"Coproduct format called, ${serV}\nkey ${key}\nV$v")
            serV
          } else {
            throw new RuntimeException(s"Expected document  got ${serV}")
          }

        case Inr(r) =>
          jft.write(r)
      }
    }



  implicit def bsonEncoder[T, Repr](implicit gen: LabelledGeneric.Aux[T, Repr],
                           sg: Cached[Strict[WrappedBsonFromat[T, Repr]]],
                           tpe: Typeable[T]
                          ) : BsonFormat[T] = new BsonFormat[T] {
    println(s"Creating ${tpe.describe}")
    override def write(t: T): BsonValue = sg.value.value.write(gen.to(t))
    /*override def read(b: BsonValue): T = {
      throw new RuntimeException("Not implemented reading yet")
    }*/
  }

}

object BsonEncoder {
  def apply[T](implicit f: Lazy[BsonFormat[T]]): BsonFormat[T] = f.value

}
