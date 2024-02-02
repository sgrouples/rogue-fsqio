package me.sgrouples.rogue.cc

import me.sgrouples.rogue._
import org.bson.types.ObjectId

import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.reflect.{ClassTag, api}

import enumeratum.{Enum, EnumEntry}
import me.sgrouples.rogue.map.MapKeyFormat

import scala.concurrent.duration.Duration
import java.util.UUID

trait QueryFieldHelpers[Meta] extends NamesResolver {
  requires: Meta =>

  protected def IntField(name: String): IntField[Meta] =
    named(name)(new IntField[Meta](_, this))

  protected def OptIntField(name: String): OptIntField[Meta] =
    named(name)(new OptIntField[Meta](_, this))

  protected def IntSubtypeField[T <: Int](
      name: String
  ): IntSubtypeField[T, Meta] =
    named(name)(new IntSubtypeField[T, Meta](_, this))

  protected def OptIntSubtypeField[T <: Int](
      name: String
  ): OptIntSubtypeField[T, Meta] =
    named(name)(new OptIntSubtypeField[T, Meta](_, this))

  protected def StringField(name: String): StringField[Meta] =
    named(name)(new StringField[Meta](_, this))

  protected def OptStringField(name: String): OptStringField[Meta] =
    named(name)(new OptStringField[Meta](_, this))

  protected def StringSubtypeField[T <: String](
      name: String
  ): StringSubtypeField[T, Meta] =
    named(name)(new StringSubtypeField[T, Meta](_, this))
  protected def OptStringSubtypeField[T <: String](
      name: String
  ): OptStringSubtypeField[T, Meta] =
    named(name)(new OptStringSubtypeField[T, Meta](_, this))

  protected def LongField(name: String): LongField[Meta] =
    named(name)(new LongField[Meta](_, this))

  protected def OptLongField(name: String): OptLongField[Meta] =
    named(name)(new OptLongField[Meta](_, this))

  protected def BigDecimalField(name: String): BigDecimalField[Meta] =
    named(name)(new BigDecimalField[Meta](_, this))

  protected def OptBigDecimalField(
      name: String
  ): OptBigDecimalField[Meta] =
    named(name)(new OptBigDecimalField[Meta](_, this))

  protected def LongSubtypeField[T <: Long](
      name: String
  ): LongSubtypeField[T, Meta] =
    named(name)(new LongSubtypeField[T, Meta](_, this))

  protected def OptLongSubtypeField[T <: Long](
      name: String
  ): OptLongSubtypeField[T, Meta] =
    named(name)(new OptLongSubtypeField[T, Meta](_, this))

  protected def DoubleField(name: String): DoubleField[Meta] =
    named(name)(new DoubleField[Meta](_, this))

  protected def OptDoubleField(name: String): OptDoubleField[Meta] =
    named(name)(new OptDoubleField[Meta](_, this))

  protected def ObjectIdField(name: String): ObjectIdField[Meta] =
    named(name)(new ObjectIdField[Meta](_, this))

  protected def ObjectIdTaggedField[Tag](
      name: String
  ): ObjectIdTaggedField[Tag, Meta] =
    named(name)(new ObjectIdTaggedField[Tag, Meta](_, this))

  protected def OptObjectIdTaggedField[Tag](
      name: String
  ): OptObjectIdTaggedField[Tag, Meta] =
    named(name)(new OptObjectIdTaggedField[Tag, Meta](_, this))

  protected def OptObjectIdField(
      name: String
  ): OptObjectIdField[Meta] =
    named(name)(new OptObjectIdField[Meta](_, this))

  protected def ObjectIdSubtypeField[T <: ObjectId](
      name: String
  ): ObjectIdSubtypeField[T, Meta] =
    named(name)(new ObjectIdSubtypeField[T, Meta](_, this))

  protected def OptObjectIdSubtypeField[T <: ObjectId](
      name: String
  ): OptObjectIdSubtypeField[T, Meta] =
    named(name)(new OptObjectIdSubtypeField[T, Meta](_, this))

  protected def UUIdField(name: String): UUIDIdField[Meta] =
    named(name)(new UUIDIdField[Meta](_, this))

  protected def OptUUIdField(name: String): OptUUIDIdField[Meta] =
    named(name)(new OptUUIDIdField[Meta](_, this))

  protected def UUIdSubtypeField[T <: UUID](
      name: String
  ): UUIDIdSubtypeField[T, Meta] =
    named(name)(new UUIDIdSubtypeField[T, Meta](_, this))

  protected def OptUUIdSubtypeField[T <: UUID](
      name: String
  ): OptUUIDIdSubtypeField[T, Meta] =
    named(name)(new OptUUIDIdSubtypeField[T, Meta](_, this))

  protected def LocalDateTimeField(
      name: String
  ): LocalDateTimeField[Meta] =
    named(name)(new LocalDateTimeField[Meta](_, this))

  protected def OptLocalDateTimeField(
      name: String
  ): OptLocalDateTimeField[Meta] =
    named(name)(new OptLocalDateTimeField[Meta](_, this))

  protected def CurrencyField(name: String): CurrencyField[Meta] =
    named(name)(new CurrencyField[Meta](_, this))

  protected def OptCurrencyField(
      name: String
  ): OptCurrencyField[Meta] =
    named(name)(new OptCurrencyField[Meta](_, this))

  protected def InstantField(name: String): InstantField[Meta] =
    named(name)(new InstantField[Meta](_, this))

  protected def OptInstantField(name: String): OptInstantField[Meta] =
    named(name)(new OptInstantField[Meta](_, this))

  protected def BooleanField(name: String): BooleanField[Meta] =
    named(name)(new BooleanField[Meta](_, this))

  protected def OptBooleanField(name: String): OptBooleanField[Meta] =
    named(name)(new OptBooleanField[Meta](_, this))

  /** This version of the EnumField method accepts e: E as a param to avoid ugly
    * type parameters like [MyEnum.type] So instead of writing val myEnum =
    * EnumField[MyEnum.type, MyMeta] we can simply write val myEnum =
    * EnumField("name", MyEnum)
    * @param e
    *   @tparam E
    * @return
    */
  protected def EnumField[E <: Enumeration](
      name: String,
      e: E
  ): EnumField[e.Value, Meta] =
    named(name)(new EnumField[e.Value, Meta](_, this, e(0)))

  /** This version of the EnumField method accepts e: E as a param to avoid ugly
    * type parameters like [MyEnum.type] So instead of writting val myEnum =
    * OptEnumField[MyEnum.type, MyMeta] we can simply write val myEnum =
    * OptEnumField("name", MyEnum)
    * @param e
    *   @tparam E
    * @return
    */
  protected def OptEnumField[E <: Enumeration](
      name: String,
      e: E
  ): OptEnumField[e.Value, Meta] =
    named(name)(new OptEnumField[e.Value, Meta](_, this))

  /** This version of the EnumField method accepts e: E as a param to avoid ugly
    * type parameters like [MyEnum.type] So instead of writting val myEnum =
    * EnumIdField[MyEnum.type, MyMeta] we can simply write val myEnum =
    * EnumIdField("name", MyEnum)
    * @param e
    *   @tparam E
    * @return
    */
  protected def EnumIdField[E <: Enumeration](
      name: String,
      e: E
  ): EnumIdField[e.Value, Meta] =
    named(name)(new EnumIdField[e.Value, Meta](_, this, e(0)))

  /** This version of the EnumField method accepts e: E as a param to avoid ugly
    * type parameters like [MyEnum.type] So instead of writing val myEnum =
    * OptEnumIdField[MyEnum.type, MyMeta] we can simply write val myEnum =
    * OptEnumIdField("name", MyEnum)
    * @param e
    *   @tparam E
    * @return
    */
  protected def OptEnumIdField[E <: Enumeration](
      name: String,
      e: E
  ): OptEnumIdField[e.Value, Meta] =
    named(name)(new OptEnumIdField[e.Value, Meta](_, this))

  protected def ListField[V](name: String): ListField[V, Meta] =
    named(name)(new ListField[V, Meta](_, this))

  protected def OptListField[V](name: String): OptListField[V, Meta] =
    named(name)(new OptListField[V, Meta](_, this))

  protected def ArrayField[V: ClassTag](
      name: String
  ): ArrayField[V, Meta] =
    named(name)(new ArrayField[V, Meta](_, this))

  protected def OptArrayField[V: ClassTag](
      name: String
  ): OptArrayField[V, Meta] =
    named(name)(new OptArrayField[V, Meta](_, this))

  protected def VectorField[V](name: String): VectorField[V, Meta] =
    named(name)(new VectorField[V, Meta](_, this))

  protected def OptVectorField[V](
      name: String
  ): OptVectorField[V, Meta] =
    named(name)(new OptVectorField[V, Meta](_, this))

  protected def SeqField[V](name: String): SeqField[V, Meta] =
    named(name)(new SeqField[V, Meta](_, this))

  protected def OptSeqField[V](name: String): OptSeqField[V, Meta] =
    named(name)(new OptSeqField[V, Meta](_, this))

  protected def ClassField[C, MC <: CcMeta[C]](
      name: String,
      mc: MC
  ): CClassField[C, MC, Meta] =
    named(name)(new CClassField[C, MC, Meta](_, mc, this))

  protected def OptClassField[C, MC <: CcMeta[C]](
      name: String,
      mc: MC
  ): OptCClassField[C, MC, Meta] =
    named(name)(new OptCClassField[C, MC, Meta](_, mc, this))

  protected def ClassRequiredField[C, MC <: CcMeta[C]](
      name: String,
      mc: MC,
      default: C
  ): CClassRequiredField[C, MC, Meta] =
    named(name)(new CClassRequiredField(_, mc, default, this))

  protected def ClassListField[C: ClassTag, MC <: CcMeta[C]](
      name: String,
      mc: MC
  ): CClassListField[C, MC, Meta] =
    named(name)(new CClassListField[C, MC, Meta](_, mc, this))

  protected def OptClassListField[C: ClassTag, MC <: CcMeta[C]](
      name: String,
      mc: MC
  ): OptCClassListField[C, MC, Meta] =
    named(name)(new OptCClassListField[C, MC, Meta](_, mc, this))

  protected def ClassArrayField[C: ClassTag, MC <: CcMeta[C]](
      name: String,
      mc: MC
  ): CClassArrayField[C, MC, Meta] =
    named(name)(new CClassArrayField[C, MC, Meta](_, mc, this))

  protected def OptClassArrayField[C: ClassTag, MC <: CcMeta[C]](
      name: String,
      mc: MC
  ): OptCClassArrayField[C, MC, Meta] =
    named(name)(new OptCClassArrayField[C, MC, Meta](_, mc, this))

  protected def MapField[K: MapKeyFormat, V](
      name: String
  ): MapField[K, V, Meta] =
    named(name)(new MapField[K, V, Meta](_, this))

  protected def OptMapField[V](name: String): OptMapField[V, Meta] =
    named(name)(new OptMapField[V, Meta](_, this))

  protected def LocaleField(name: String): LocaleField[Meta] =
    named(name)(new LocaleField[Meta](_, this))

  protected def OptLocaleField(name: String): OptLocaleField[Meta] =
    named(name)(new OptLocaleField[Meta](_, this))

  protected def EnumeratumField[E <: EnumEntry](
      name: String,
      e: Enum[E]
  ): EnumeratumField[E, Meta] =
    named(name)(new EnumeratumField[E, Meta](e, _, this))
}
