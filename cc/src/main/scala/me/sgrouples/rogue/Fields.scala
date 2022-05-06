package me.sgrouples.rogue
import java.time.{Instant, LocalDateTime, ZoneOffset}
import java.util.{Currency, Locale, UUID}

import io.fsq.field.{Field, OptionalField, RequiredField}
import me.sgrouples.rogue.cc.CcMeta
import me.sgrouples.rogue.map.MapKeyFormat
import org.bson.types.ObjectId

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import io.fsq.rogue.enums.EnumInstance

abstract class CField[V, O](val name: String, val owner: O) extends Field[V, O]

abstract class MCField[V, O](name: String, owner: O)
    extends CField[V, O](name, owner)
    with RequiredField[V, O]

abstract class OCField[V, O](name: String, owner: O)
    extends CField[V, O](name, owner)
    with OptionalField[V, O]

class IntField[O](name: String, o: O) extends MCField[Int, O](name, o) {
  override def defaultValue = 0
}

class IntSubtypeField[T <: Int, O](name: String, o: O)
    extends MCField[T, O](name, o) {
  override def defaultValue: T = 0.asInstanceOf[T]
}

class LongField[O](name: String, o: O) extends MCField[Long, O](name, o) {
  override def defaultValue = 0L
}

class BigDecimalField[O](name: String, o: O)
    extends MCField[BigDecimal, O](name, o) {
  override def defaultValue = BigDecimal(0)
}

class LongSubtypeField[T <: Long, O](name: String, o: O)
    extends MCField[T, O](name, o) {
  override def defaultValue: T = 0L.asInstanceOf[T]
}

class DoubleField[O](name: String, o: O) extends MCField[Double, O](name, o) {
  override def defaultValue = 0d
}
class StringField[O](name: String, o: O) extends MCField[String, O](name, o) {
  override def defaultValue = ""
}

class StringSubtypeField[T <: String, O](name: String, o: O)
    extends MCField[T, O](name, o) {
  override def defaultValue: T = "".asInstanceOf[T]
}

class ObjectIdField[O](name: String, o: O)
    extends MCField[ObjectId, O](name, o) {
  override def defaultValue: ObjectId = ObjectId.get()
}

class ObjectIdSubtypeField[T <: ObjectId, O](name: String, o: O)
    extends MCField[T, O](name, o) {
  override def defaultValue: T =
    ObjectId.get().asInstanceOf[T] //random oid ? hmm
}

class UUIDIdField[O](name: String, o: O) extends MCField[UUID, O](name, o) {
  override def defaultValue: UUID = UUID.randomUUID()
}

class UUIDIdSubtypeField[T <: UUID, O](name: String, o: O)
    extends MCField[T, O](name, o) {
  override def defaultValue: T = UUID.randomUUID().asInstanceOf[T]
}

class LocalDateTimeField[O](name: String, o: O)
    extends MCField[LocalDateTime, O](name, o) {
  override def defaultValue: LocalDateTime = LocalDateTime.now(ZoneOffset.UTC)
}

class InstantField[O](name: String, o: O) extends MCField[Instant, O](name, o) {
  override def defaultValue: Instant = Instant.now()
}

class CurrencyField[O](name: String, o: O)
    extends MCField[Currency, O](name, o) {
  override def defaultValue: Currency = Currency.getInstance("USD")
}

class LocaleField[O](name: String, o: O) extends MCField[Locale, O](name, o) {
  override def defaultValue: Locale = Locale.US
}

class BooleanField[O](name: String, o: O) extends MCField[Boolean, O](name, o) {
  override def defaultValue = false
}
class EnumField[T <: Enumeration, O](name: String, o: O, val e: T)
    extends MCField[e.Value, O](name, o)
    with EnumInstance(e) {
  override def defaultValue: e.Value = e(0)
}

class EnumIdField[T <: Enumeration, O](name: String, o: O, val e:T)
    extends MCField[e.Value, O](name, o)
    with EnumInstance(e) {
    override def defaultValue: e.Value = e(0)
}

class ListField[V, O](name: String, o: O) extends MCField[List[V], O](name, o) {
  override def defaultValue: List[V] = Nil
}

class SeqField[V, O](name: String, o: O) extends MCField[Seq[V], O](name, o) {
  override def defaultValue: List[V] = Nil
}

class VectorField[V, O](name: String, o: O)
    extends MCField[Vector[V], O](name, o) {
  override def defaultValue = Vector.empty[V]
}

class ArrayField[V: ClassTag, O](name: String, o: O)
    extends MCField[Array[V], O](name, o) {
  override def defaultValue = Array.empty[V]
}

class CClassField[C, MC <: CcMeta[C], O](
    val name: String,
    val childMeta: MC,
    val owner: O
) extends Field[C, O]

/** Same as CClassField but because defaultValue is required, it can be
  * selected. If you don't want to provide defaultValue use CClassField but you
  * will not be able to select that cc. To create a field like that you must
  * override defaultValue, or just use CClassRequiredField.
  * @param name
  *   @param childMeta
  * @param owner
  *   @tparam C
  * @tparam MC
  *   @tparam O
  */

abstract class CClassAbstractRequiredField[C, MC <: CcMeta[C], O](
    name: String,
    childMeta: MC,
    owner: O
) extends CClassField[C, MC, O](name, childMeta, owner)
    with RequiredField[C, O]

/** Same as CClassField but because defaultValue is required, it can be
  * selected. If you don't want to provide defaultValue use CClassField but you
  * will not be able to select that cc.
  * @param name
  *   @param childMeta
  * @param owner
  *   @param defaultValue
  * @tparam C
  *   @tparam MC
  * @tparam O
  */

class CClassRequiredField[C, MC <: CcMeta[C], O](
    name: String,
    childMeta: MC,
    override val defaultValue: C,
    owner: O
) extends CClassAbstractRequiredField[C, MC, O](name, childMeta, owner)

trait HasChildMeta[C, MC <: CcMeta[C]] {
  def childMeta: MC
}

class CClassListField[C, MC <: CcMeta[C], O](
    name: String,
    val childMeta: MC,
    owner: O
) extends MCField[List[C], O](name, owner)
    with HasChildMeta[C, MC] {
  override def defaultValue: List[C] = Nil
}

class CClassArrayField[C: ClassTag, MC <: CcMeta[C], O](
    name: String,
    val childMeta: MC,
    o: O
) extends MCField[Array[C], O](name, o)
    with HasChildMeta[C, MC] {
  override def defaultValue = Array.empty[C]
}

class MapField[K: MapKeyFormat, V, O](name: String, o: O)
    extends MCField[Map[K, V], O](name, o) {
  override def defaultValue = Map.empty
}

class OptIntField[O](name: String, o: O) extends OCField[Int, O](name, o)
class OptIntSubtypeField[T <: Int, O](name: String, o: O)
    extends OCField[T, O](name, o)

class OptLongField[O](name: String, o: O) extends OCField[Long, O](name, o)
class OptLongSubtypeField[T <: Long, O](name: String, o: O)
    extends OCField[T, O](name, o)

class OptBigDecimalField[O](name: String, o: O)
    extends OCField[BigDecimal, O](name, o)

class OptStringField[O](name: String, o: O) extends OCField[String, O](name, o)
class OptStringSubtypeField[T <: String, O](name: String, o: O)
    extends OCField[T, O](name, o)

class OptObjectIdField[O](name: String, o: O)
    extends OCField[ObjectId, O](name, o)
class OptObjectIdSubtypeField[Subtype <: ObjectId, O](name: String, o: O)
    extends OCField[Subtype, O](name, o)

class OptUUIDIdField[O](name: String, o: O) extends OCField[UUID, O](name, o)
class OptUUIDIdSubtypeField[T <: UUID, O](name: String, o: O)
    extends OCField[T, O](name, o)

class OptDoubleField[O](name: String, o: O) extends OCField[Double, O](name, o)
class OptLocalDateTimeField[O](name: String, o: O)
    extends OCField[LocalDateTime, O](name, o)

class OptCurrencyField[O](name: String, o: O)
    extends OCField[Currency, O](name, o)
class OptLocaleField[O](name: String, o: O) extends OCField[Locale, O](name, o)

class OptInstantField[O](name: String, o: O)
    extends OCField[Instant, O](name, o)
class OptBooleanField[O](name: String, o: O)
    extends OCField[Boolean, O](name, o)
class OptEnumField[T <: Enumeration, O](name: String, o: O, val e:T)
    extends OCField[e.Value, O](name, o) with EnumInstance(e)
class OptEnumIdField[T <: Enumeration, O](name: String, o: O, val e:T)
    extends OCField[e.Value, O](name, o) with EnumInstance(e)
class OptListField[V, O](name: String, o: O)
    extends OCField[List[V], O](name, o)
class OptArrayField[V: ClassTag, O](name: String, o: O)
    extends OCField[Array[V], O](name, o)
class OptVectorField[V, O](name: String, o: O)
    extends OCField[Vector[V], O](name, o)
class OptSeqField[V, O](name: String, o: O) extends OCField[Seq[V], O](name, o)
class OptCClassField[C, MC <: CcMeta[C], O](
    name: String,
    val childMeta: MC,
    owner: O
) extends OCField[C, O](name, owner)
    with HasChildMeta[C, MC]
class OptCClassListField[C, MC <: CcMeta[C], O](
    name: String,
    val childMeta: MC,
    o: O
) extends OCField[List[C], O](name, o)
    with HasChildMeta[C, MC]
class OptCClassArrayField[C: ClassTag, MC <: CcMeta[C], O](
    name: String,
    val childMeta: MC,
    o: O
) extends OCField[Array[C], O](name, o)
    with HasChildMeta[C, MC]
class OptMapField[V, O](name: String, o: O)
    extends OCField[Map[String, V], O](name, o)

/*    
trait CcFields[T] {
  type RecRepr
  def flds: RecRepr
}

trait LowPrioFields {
  object Owner

  abstract class CcFieldFormat[Wrapped, SubRepr](implicit
      tpe: Typeable[Wrapped]
  ) {
    type RecRepr
    def flds: RecRepr
  }

  object CcFieldFormat {
    type Aux[Wrapperd, SubRepr, H] = CcFieldFormat[Wrapperd, SubRepr] {
      type RecRepr = H
    }

    implicit def ccFieldHNil[Wrapped, R](implicit
        tpe: Typeable[Wrapped]
    ): Aux[Wrapped, HNil, HNil] = new CcFieldFormat[Wrapped, HNil] {
      type RecRepr = HNil
      override def flds = HNil
    }

    implicit def hListExtFormat[
        Wrapped,
        Key <: Symbol,
        Value,
        Remaining <: HList,
        RecRemH <: HList
    ](implicit
        t: Typeable[Wrapped],
        key: Witness.Aux[Key],
        remFormat: CcFieldFormat.Aux[Wrapped, Remaining, RecRemH]
    ): CcFieldFormat.Aux[Wrapped, FieldType[Key, Value] :: Remaining, FieldType[
      Key,
      CField[Value, Owner.type]
    ] :: RecRemH] =
      new CcFieldFormat[Wrapped, FieldType[Key, Value] :: Remaining] {
        type RecRepr = FieldType[Key, CField[Value, Owner.type]] :: RecRemH

        override def flds = {
          val cc = new CField[Value, Owner.type](key.value.name, Owner) {}
          val f = field[Key](cc)
          val k = f :: remFormat.flds
          //this does not work too... val k= (key.value.name ->> cc ) :: remFormat.flds
          k
        }
      }

  }

  implicit def ccEncoder[T, Repr, RR](implicit
      gen: LabelledGeneric.Aux[T, Repr],
      sg: CcFieldFormat.Aux[T, Repr, RR],
      tpe: Typeable[T]
  ): CcFields.Aux[T, RR] = new CcFields[T] {
    type RecRepr = sg.RecRepr
    override val flds = sg.flds
  }

}

object CcFields extends LowPrioFields {
  type Aux[T, R] = CcFields[T] { type RecRepr = R }
  def apply[T](implicit f: CcFields[T]): Aux[T, f.RecRepr] = f
}
*/