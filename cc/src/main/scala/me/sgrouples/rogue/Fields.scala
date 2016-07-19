package me.sgrouples.rogue
import io.fsq.field.{Field, RequiredField}
import shapeless._
import labelled.{FieldType, field}
import syntax.singleton._
import record._
import ops.record._
import org.bson.types.ObjectId
import org.bson.{BsonDocument, BsonNull, BsonValue}
import shapeless.ops.hlist.LiftAll
import shapeless.syntax.SingletonOps



abstract class CField[V, O](val name:String, val owner :O) extends Field[V,O]



abstract class MCField[V, O](name:String, owner:O) extends CField[V, O](name, owner) with RequiredField[V,O]


class IntField[O](name:String, o:O) extends MCField[Int,O](name, o) {
  override def defaultValue = 0
}
class LongField[O](name:String, o:O) extends MCField[Long,O](name, o){
  override def defaultValue = 0L
}
class StringField[O](name:String, o:O) extends MCField[String, O](name, o){
  override def defaultValue = ""
}
class ObjectIdField[O](name:String, o:O) extends MCField[ObjectId, O](name, o){
  override def defaultValue = ObjectId.get()
}
class BooleanField[O](name:String,o :O) extends MCField[Boolean, O](name, o){
  override def defaultValue = false
}
class EnumField[T <: Enumeration, O](name:String, o:O)(implicit e: T) extends MCField[T#Value, O](name, o){
  override def defaultValue: T#Value = e(0)
}


trait CcFields[T] {
  type RecRepr
  def flds:RecRepr
}


trait LowPrioFields {
  object Owner

  abstract class CcFieldFormat[Wrapped, SubRepr](implicit tpe :Typeable[Wrapped]) {
    type RecRepr
    def flds: RecRepr
  }

  object CcFieldFormat {
    type Aux[Wrapperd, SubRepr, H] = CcFieldFormat[Wrapperd, SubRepr] { type RecRepr = H }

    implicit def ccFieldHNil[Wrapped, R](implicit tpe: Typeable[Wrapped]): Aux[Wrapped, HNil, HNil] = new CcFieldFormat[Wrapped, HNil] {
      type RecRepr = HNil
      override def flds = HNil
    }

    implicit def hListExtFormat[Wrapped, Key <: Symbol, Value, Remaining <: HList, RecRemH <: HList](
                                                                                                      implicit
                                                                                                      t: Typeable[Wrapped],
                                                                                                      key: Witness.Aux[Key],
                                                                                                      remFormat: CcFieldFormat.Aux[Wrapped, Remaining, RecRemH]
                                                                                                    ): CcFieldFormat.Aux[Wrapped, FieldType[Key, Value] :: Remaining, FieldType[Key, CField[Value, Owner.type]] :: RecRemH ] =
      new CcFieldFormat[Wrapped, FieldType[Key, Value] :: Remaining]{
        type RecRepr =  FieldType[Key, CField[Value, Owner.type]] :: RecRemH

        override def flds = {
          val cc = new CField[Value, Owner.type ](key.value.name, Owner){

          }
          val f= field[Key](cc)
          val k= f :: remFormat.flds
          //this does not work too... val k= (key.value.name ->> cc ) :: remFormat.flds
          k
        }
      }

  }

  implicit def ccEncoder[T, Repr, RR](implicit gen: LabelledGeneric.Aux[T, Repr],
                                       sg: CcFieldFormat.Aux[T, Repr, RR],
                                       tpe: Typeable[T]
                                   ) : CcFields.Aux[T, RR] = new CcFields[T] {
    type RecRepr = sg.RecRepr
    override val flds = sg.flds
  }

}

object CcFields extends LowPrioFields{
  type Aux[T, R] = CcFields[T] { type RecRepr = R}
  def apply[T](implicit f: CcFields[T]): Aux[T, f.RecRepr] = f
}



