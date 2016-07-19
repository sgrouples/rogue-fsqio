package me.sgrouples.rogue
import io.fsq.field.Field
import shapeless._
import labelled.{FieldType, field}
import syntax.singleton._
import record._
import ops.record._
import org.bson.{BsonDocument, BsonNull, BsonValue}
import shapeless.syntax.SingletonOps


object Owner

trait CcField[V, R] extends Field[V, R] {
  def format:BsonFormat[V]
}

class CField(val name:String)

trait CcFields[T] {
  type RecRepr
  def flds:RecRepr
}


trait LowPrioFields {

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
                                                                                                    ): CcFieldFormat.Aux[Wrapped, FieldType[Key, Value] :: Remaining, FieldType[Key, CField] :: RecRemH ] =
      new CcFieldFormat[Wrapped, FieldType[Key, Value] :: Remaining]{
        type RecRepr =  FieldType[Key, CField] :: RecRemH

        override def flds = {
          remFormat.flds
          val cc = new CField(key.value.name)
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
  //  def apply[T](implicit lgen: LabelledGeneric[T]): Aux[T, lgen.Repr] = lgen

  def apply[T](implicit f: CcFields[T]): Aux[T, f.RecRepr] = f

}



