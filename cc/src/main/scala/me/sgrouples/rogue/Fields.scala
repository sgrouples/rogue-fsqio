package me.sgrouples.rogue
import io.fsq.field.Field
import shapeless._
import labelled.{FieldType, field}
import syntax.singleton._
import record._

import org.bson.{BsonDocument, BsonNull, BsonValue}


object Owner

trait CcField[V, R] extends Field[V, R] {
  def format:BsonFormat[V]
}

//bson with fields - how to make it better ?
trait BsonExtFormat[T] extends BsonFormat[T] {
  def flds: List[String]

}



trait LowPrioFields {
  this: BaseBsonFormats with BsonFormats =>

  abstract class WrappedExtBsonFormat[Wrapped, SubRepr](implicit tpe :Typeable[Wrapped]) {
    def write(v: SubRepr): BsonValue
    def read(b: BsonValue): SubRepr
    def flds: List[String]
  }

  implicit def hHilExtBsonFormat[Wrapped](implicit tpe: Typeable[Wrapped]): WrappedExtBsonFormat[Wrapped, HNil] = new WrappedExtBsonFormat[Wrapped, HNil]{
    override def write(t: HNil): BsonValue = BsonNull.VALUE
    override def read(b: BsonValue) = HNil
    override def flds = Nil
  }


  implicit def hListExtFormat[Wrapped, Key <: Symbol, Value, Remaining <: HList](
                                                                               implicit
                                                                               t: Typeable[Wrapped],
                                                                               key: Witness.Aux[Key],
                                                                               headSer: Lazy[BsonFormat[Value]],
                                                                               remFormat: WrappedExtBsonFormat[Wrapped, Remaining]
                                                                             ): WrappedExtBsonFormat[Wrapped, FieldType[Key, Value] :: Remaining] =
    new WrappedExtBsonFormat[Wrapped, FieldType[Key, Value] :: Remaining] {
      private[this] val fieldName  = key.value.name
      private[this] val bsonValue = headSer.value
      override def write(ft: FieldType[Key, Value] :: Remaining): BsonValue = {
        val rest = remFormat.write(ft.tail)
        val serializedVal = bsonValue.write(ft.head)
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

      override def read(b: BsonValue): FieldType[Key, Value] :: Remaining = {
        val resolved: Value = {
          val v = b.asDocument().get(fieldName)
          if(v == null && v.isNull) {
            bsonValue.read(BsonNull.VALUE)
          } else {
            bsonValue.read(v)
          }
        }
        val remaining = remFormat.read(b)
        field[Key](resolved) :: remaining
      }

      override def flds = {
        /*val cc = new CcField[Value, Owner.type]() {
          override def format: BsonFormat[Value] = bsonValue

          override def name: String = fieldName

          override def owner: Owner.type = Owner
        }*/
        //HOW TO MAKE LINE BELOW WORK?
        //val h = (key.value.name -> cc)
        //field[Key]("A") :: remFormat.flds

        //val h = (key.value.name -> "a")
        //println(s"Created fld ${h}")
        //h :: remFormat.flds
        key.value.name :: remFormat.flds
      }
    }


  implicit def bsonExtEncoder[T, Repr](implicit gen: LabelledGeneric.Aux[T, Repr],
                                       //ht : BsonExtFormat.Aux[T, H1],
                                       sg: Cached[Strict[WrappedExtBsonFormat[T, Repr]]],
                                       tpe: Typeable[T]
                                   ) : BsonExtFormat[T] = new BsonExtFormat[T] {
    override def write(t: T): BsonValue = sg.value.value.write(gen.to(t))
    override def read(b: BsonValue): T = gen.from(sg.value.value.read(b))
    override def flds = sg.value.value.flds
  }


}

object BsonExtFormats extends StandardBsonFormats with BsonFormats with LowPrioFields
object BsonExtFormat {
  type Aux[T, Repr0] = BsonExtFormat[T]{ type H = Repr0 }


  def apply[T](implicit f: Lazy[BsonExtFormat[T]]): BsonExtFormat[T] = f.value

}
