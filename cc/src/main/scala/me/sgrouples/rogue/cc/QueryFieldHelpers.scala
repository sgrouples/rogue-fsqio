package me.sgrouples.rogue.cc


import me.sgrouples.rogue._
import org.bson.types.ObjectId

import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.reflect.{ClassTag, api}
import me.sgrouples.rogue.map.MapKeyFormat

import scala.concurrent.duration.Duration
import java.util.UUID


trait NamesResolver {
  protected def named[T <: io.fsq.field.Field[_, _]](name: String)(
      func: String => T
  ): T
  protected def named[T <: io.fsq.field.Field[_, _]](
      func: String => T
  ): T 
}

trait QueryFieldHelpers[Meta] extends NamesResolver {
  requires: Meta =>

  protected def IntField: IntField[Meta] = named(
    new IntField[Meta](_, this)
  )
  protected def IntField(name: String): IntField[Meta]  =
    named(name)(new IntField[Meta](_, this))

  protected def OptIntField: OptIntField[Meta]  = named(
    new OptIntField[Meta](_, this)
  )
  protected def OptIntField(name: String): OptIntField[Meta]  =
    named(name)(new OptIntField[Meta](_, this))

  protected def IntSubtypeField[T<:Int]: IntSubtypeField[T, Meta]  =
    named(new IntSubtypeField[T, Meta](_, this))
  protected def IntSubtypeField[T <: Int](
      name: String
  ): IntSubtypeField[T, Meta]  =
    named(name)(new IntSubtypeField[T, Meta](_, this))

  protected def OptIntSubtypeField[T <: Int]: OptIntSubtypeField[T, Meta]  =
    named(new OptIntSubtypeField[T, Meta](_, this))
  protected def OptIntSubtypeField[T <: Int](
      name: String
  ): OptIntSubtypeField[T, Meta]  =
    named(name)(new OptIntSubtypeField[T, Meta](_, this))

  protected def StringField: StringField[Meta]  = named(
    new StringField[Meta](_, this)
  )
  protected def StringField(name: String): StringField[Meta]  =
    named(name)(new StringField[Meta](_, this))

  protected def OptStringField: OptStringField[Meta]  = named(
    new OptStringField[Meta](_, this)
  )
  protected def OptStringField(name: String): OptStringField[Meta]  =
    named(name)(new OptStringField[Meta](_, this))

  protected def StringSubtypeField[T<:String]: StringSubtypeField[T, Meta]  =
    named(new StringSubtypeField[T, Meta](_, this))
  protected def StringSubtypeField[T<:String](
      name: String
  ): StringSubtypeField[T, Meta]  =
    named(name)(new StringSubtypeField[T, Meta](_, this))

  protected def OptStringSubtypeField[T<:String]
      : OptStringSubtypeField[T, Meta]  = named(
    new OptStringSubtypeField[T, Meta](_, this)
  )
  protected def OptStringSubtypeField[T<:String](
      name: String
  ): OptStringSubtypeField[T, Meta]  =
    named(name)(new OptStringSubtypeField[T, Meta](_, this))

  protected def LongField: LongField[Meta]  = named(
    new LongField[Meta](_, this)
  )
  protected def LongField(name: String): LongField[Meta]  =
    named(name)(new LongField[Meta](_, this))

  protected def OptLongField: OptLongField[Meta]  = named(
    new OptLongField[Meta](_, this)
  )
  protected def OptLongField(name: String): OptLongField[Meta]  =
    named(name)(new OptLongField[Meta](_, this))

  protected def BigDecimalField: BigDecimalField[Meta]  = named(
    new BigDecimalField[Meta](_, this)
  )
  protected def BigDecimalField(name: String): BigDecimalField[Meta]  =
    named(name)(new BigDecimalField[Meta](_, this))

  protected def OptBigDecimalField: OptBigDecimalField[Meta]  = named(
    new OptBigDecimalField[Meta](_, this)
  )
  protected def OptBigDecimalField(
      name: String
  ): OptBigDecimalField[Meta]  =
    named(name)(new OptBigDecimalField[Meta](_, this))

  protected def LongSubtypeField[T<:Long]: LongSubtypeField[T, Meta]  =
    named(new LongSubtypeField[T, Meta](_, this))
  protected def LongSubtypeField[T<:Long](
      name: String
  ): LongSubtypeField[T, Meta]  =
    named(name)(new LongSubtypeField[T, Meta](_, this))

  protected def OptLongSubtypeField[T<:Long]
      : OptLongSubtypeField[T, Meta]  = named(
    new OptLongSubtypeField[T, Meta](_, this)
  )
  protected def OptLongSubtypeField[T<:Long](
      name: String
  ): OptLongSubtypeField[T, Meta]  =
    named(name)(new OptLongSubtypeField[T, Meta](_, this))

  protected def DoubleField: DoubleField[Meta]  = named(
    new DoubleField[Meta](_, this)
  )
  protected def DoubleField(name: String): DoubleField[Meta]  =
    named(name)(new DoubleField[Meta](_, this))

  protected def OptDoubleField: OptDoubleField[Meta]  = named(
    new OptDoubleField[Meta](_, this)
  )
  protected def OptDoubleField(name: String): OptDoubleField[Meta]  =
    named(name)(new OptDoubleField[Meta](_, this))

  protected def ObjectIdField: ObjectIdField[Meta]  = named(
    new ObjectIdField[Meta](_, this)
  )
  protected def ObjectIdField(name: String): ObjectIdField[Meta]  =
    named(name)(new ObjectIdField[Meta](_, this))

  protected def OptObjectIdField: OptObjectIdField[Meta]  = named(
    new OptObjectIdField[Meta](_, this)
  )
  protected def OptObjectIdField(
      name: String
  ): OptObjectIdField[Meta]  =
    named(name)(new OptObjectIdField[Meta](_, this))

  protected def ObjectIdSubtypeField[T<:ObjectId]
      : ObjectIdSubtypeField[T, Meta]  = named(
    new ObjectIdSubtypeField[T, Meta](_, this)
  )
  protected def ObjectIdSubtypeField[T<:ObjectId](
      name: String
  ): ObjectIdSubtypeField[T, Meta]  =
    named(name)(new ObjectIdSubtypeField[T, Meta](_, this))

  protected def OptObjectIdSubtypeField[T<:ObjectId]
      : OptObjectIdSubtypeField[T, Meta]  = named(
    new OptObjectIdSubtypeField[T, Meta](_, this)
  )
  protected def OptObjectIdSubtypeField[T](
      name: String
  ): OptObjectIdSubtypeField[T, Meta]  =
    named(name)(new OptObjectIdSubtypeField[T, Meta](_, this))

  protected def UUIdField: UUIDIdField[Meta]  = named(
    new UUIDIdField[Meta](_, this)
  )
  protected def UUIdField(name: String): UUIDIdField[Meta]  =
    named(name)(new UUIDIdField[Meta](_, this))

  protected def OptUUIdField: OptUUIDIdField[Meta]  = named(
    new OptUUIDIdField[Meta](_, this)
  )
  protected def OptUUIdField(name: String): OptUUIDIdField[Meta]  =
    named(name)(new OptUUIDIdField[Meta](_, this))

  protected def UUIdSubtypeField[T <: UUID]: UUIDIdSubtypeField[T, Meta]  =
    named(new UUIDIdSubtypeField[T, Meta](_, this))
  protected def UUIdSubtypeField[T <: UUID](
      name: String
  ): UUIDIdSubtypeField[T, Meta]  =
    named(name)(new UUIDIdSubtypeField[T, Meta](_, this))

  protected def OptUUIdSubtypeField[T <: UUID]
      : OptUUIDIdSubtypeField[T, Meta]  = named(
    new OptUUIDIdSubtypeField[T, Meta](_, this)
  )
  protected def OptUUIdSubtypeField[T <: UUID](
      name: String
  ): OptUUIDIdSubtypeField[T, Meta]  =
    named(name)(new OptUUIDIdSubtypeField[T, Meta](_, this))

  protected def LocalDateTimeField: LocalDateTimeField[Meta]  = named(
    new LocalDateTimeField[Meta](_, this)
  )
  protected def LocalDateTimeField(
      name: String
  ): LocalDateTimeField[Meta]  =
    named(name)(new LocalDateTimeField[Meta](_, this))

  protected def OptLocalDateTimeField: OptLocalDateTimeField[Meta]  =
    named(new OptLocalDateTimeField[Meta](_, this))
  protected def OptLocalDateTimeField(
      name: String
  ): OptLocalDateTimeField[Meta]  =
    named(name)(new OptLocalDateTimeField[Meta](_, this))

  protected def CurrencyField: CurrencyField[Meta]  = named(
    new CurrencyField[Meta](_, this)
  )
  protected def CurrencyField(name: String): CurrencyField[Meta]  =
    named(name)(new CurrencyField[Meta](_, this))

  protected def OptCurrencyField: OptCurrencyField[Meta]  = named(
    new OptCurrencyField[Meta](_, this)
  )
  protected def OptCurrencyField(
      name: String
  ): OptCurrencyField[Meta]  =
    named(name)(new OptCurrencyField[Meta](_, this))

  protected def InstantField: InstantField[Meta]  = named(
    new InstantField[Meta](_, this)
  )
  protected def InstantField(name: String): InstantField[Meta]  =
    named(name)(new InstantField[Meta](_, this))

  protected def OptInstantField: OptInstantField[Meta]  = named(
    new OptInstantField[Meta](_, this)
  )
  protected def OptInstantField(name: String): OptInstantField[Meta]  =
    named(name)(new OptInstantField[Meta](_, this))

  protected def BooleanField: BooleanField[Meta]  = named(
    new BooleanField[Meta](_, this)
  )
  protected def BooleanField(name: String): BooleanField[Meta]  =
    named(name)(new BooleanField[Meta](_, this))

  protected def OptBooleanField: OptBooleanField[Meta]  = named(
    new OptBooleanField[Meta](_, this)
  )
  protected def OptBooleanField(name: String): OptBooleanField[Meta]  =
    named(name)(new OptBooleanField[Meta](_, this))

  /** This version of the EnumField method accepts e: E as a param to avoid ugly
    * type parameters like [MyEnum.type] So instead of writing val myEnum =
    * EnumField[MyEnum.type, MyMeta] we can simply write val myEnum =
    * EnumField(MyEnum)
    * @param e
    *   @tparam E
    * @return
    */
  protected def EnumField[E <: Enumeration: TypeTag](
      e: E
  ): EnumField[E, Meta]  = named(new EnumField[E, Meta](_, this, e))
  protected def EnumField[E <: Enumeration: TypeTag](
      name: String,
      e: E
  ): EnumField[E, Meta]  = named(name)(new EnumField[E, Meta](_, this, e))

  protected def OptEnumField[E <: Enumeration]
      : OptEnumField[E, Meta]  = named(
    new OptEnumField[E, Meta](_, this)
  )
  protected def OptEnumField[E <: Enumeration](
      name: String
  ): OptEnumField[E, Meta]  =
    named(name)(new OptEnumField[E, Meta](_, this))

  /** This version of the EnumField method accepts e: E as a param to avoid ugly
    * type parameters like [MyEnum.type] So instead of writting val myEnum =
    * EnumField[MyEnum.type, MyMeta] we can simply write val myEnum =
    * EnumField(MyEnum)
    * @param e
    *   @tparam E
    * @return
    */
  protected def OptEnumField[E <: Enumeration](
      e: E
  ): OptEnumField[E, Meta]  = named(new OptEnumField[E, Meta](_, this))
  protected def OptEnumField[E <: Enumeration](
      name: String,
      e: E
  ): OptEnumField[E, Meta]  =
    named(name)(new OptEnumField[E, Meta](_, this))

  /** This version of the EnumField method accepts e: E as a param to avoid ugly
    * type parameters like [MyEnum.type] So instead of writting val myEnum =
    * EnumField[MyEnum.type, MyMeta] we can simply write val myEnum =
    * EnumField(MyEnum)
    * @param e
    *   @tparam E
    * @return
    */
  protected def EnumIdField[E <: Enumeration: TypeTag](
      e: E
  ): EnumIdField[E, Meta]  = named(new EnumIdField[E, Meta](_, this, e))
  protected def EnumIdField[E <: Enumeration: TypeTag](
      name: String,
      e: E
  ): EnumIdField[E, Meta]  =
    named(name)(new EnumIdField[E, Meta](_, this, e))

  protected def OptEnumIdField[E <: Enumeration]
      : OptEnumIdField[E, Meta]  = named(
    new OptEnumIdField[E, Meta](_, this)
  )
  protected def OptEnumIdField[E <: Enumeration](
      name: String
  ): OptEnumIdField[E, Meta]  =
    named(name)(new OptEnumIdField[E, Meta](_, this))

  /** This version of the EnumField method accepts e: E as a param to avoid ugly
    * type parameters like [MyEnum.type] So instead of writing val myEnum =
    * EnumField[MyEnum.type, MyMeta] we can simply write val myEnum =
    * EnumField(MyEnum)
    * @param e
    *   @tparam E
    * @return
    */
  protected def OptEnumIdField[E <: Enumeration](
      e: E
  ): OptEnumIdField[E, Meta]  = named(
    new OptEnumIdField[E, Meta](_, this)
  )
  protected def OptEnumIdField[E <: Enumeration](
      name: String,
      e: E
  ): OptEnumIdField[E, Meta]  =
    named(name)(new OptEnumIdField[E, Meta](_, this))

  protected def ListField[V]: ListField[V, Meta]  = named(
    new ListField[V, Meta](_, this)
  )
  protected def ListField[V](name: String): ListField[V, Meta]  =
    named(name)(new ListField[V, Meta](_, this))

  protected def OptListField[V]: OptListField[V, Meta]  = named(
    new OptListField[V, Meta](_, this)
  )
  protected def OptListField[V](name: String): OptListField[V, Meta]  =
    named(name)(new OptListField[V, Meta](_, this))

  protected def ArrayField[V: ClassTag]: ArrayField[V, Meta]  = named(
    new ArrayField[V, Meta](_, this)
  )
  protected def ArrayField[V: ClassTag](
      name: String
  ): ArrayField[V, Meta]  =
    named(name)(new ArrayField[V, Meta](_, this))

  protected def OptArrayField[V: ClassTag]: OptArrayField[V, Meta]  =
    named(new OptArrayField[V, Meta](_, this))
  protected def OptArrayField[V: ClassTag](
      name: String
  ): OptArrayField[V, Meta]  =
    named(name)(new OptArrayField[V, Meta](_, this))

  protected def VectorField[V]: VectorField[V, Meta]  = named(
    new VectorField[V, Meta](_, this)
  )
  protected def VectorField[V](name: String): VectorField[V, Meta]  =
    named(name)(new VectorField[V, Meta](_, this))

  protected def OptVectorField[V]: OptVectorField[V, Meta]  = named(
    new OptVectorField[V, Meta](_, this)
  )
  protected def OptVectorField[V](
      name: String
  ): OptVectorField[V, Meta]  =
    named(name)(new OptVectorField[V, Meta](_, this))

  protected def SeqField[V]: SeqField[V, Meta]  = named(
    new SeqField[V, Meta](_, this)
  )
  protected def SeqField[V](name: String): SeqField[V, Meta]  =
    named(name)(new SeqField[V, Meta](_, this))

  protected def OptSeqField[V]: OptSeqField[V, Meta]  = named(
    new OptSeqField[V, Meta](_, this)
  )
  protected def OptSeqField[V](name: String): OptSeqField[V, Meta]  =
    named(name)(new OptSeqField[V, Meta](_, this))

  protected def ClassField[C, MC <: CcMeta[C]](
      mc: MC
  ): CClassField[C, MC, Meta]  = named(
    new CClassField[C, MC, Meta](_, mc, this)
  )
  protected def ClassField[C, MC <: CcMeta[C]](
      name: String,
      mc: MC
  ): CClassField[C, MC, Meta]  =
    named(name)(new CClassField[C, MC, Meta](_, mc, this))

  protected def OptClassField[C, MC <: CcMeta[C]](
      mc: MC
  ): OptCClassField[C, MC, Meta]  = named(
    new OptCClassField[C, MC, Meta](_, mc, this)
  )
  protected def OptClassField[C, MC <: CcMeta[C]](
      name: String,
      mc: MC
  ): OptCClassField[C, MC, Meta]  =
    named(name)(new OptCClassField[C, MC, Meta](_, mc, this))

  protected def ClassRequiredField[C, MC <: CcMeta[C]](
      mc: MC,
      default: C
  ): CClassRequiredField[C, MC, Meta]  = named(
    new CClassRequiredField(_, mc, default, this)
  )
  protected def ClassRequiredField[C, MC <: CcMeta[C]](
      name: String,
      mc: MC,
      default: C
  ): CClassRequiredField[C, MC, Meta]  =
    named(name)(new CClassRequiredField(_, mc, default, this))

  protected def ClassListField[C: ClassTag, MC <: CcMeta[C]](
      mc: MC
  ): CClassListField[C, MC, Meta]  = named(
    new CClassListField[C, MC, Meta](_, mc, this)
  )
  protected def ClassListField[C: ClassTag, MC <: CcMeta[C]](
      name: String,
      mc: MC
  ): CClassListField[C, MC, Meta]  =
    named(name)(new CClassListField[C, MC, Meta](_, mc, this))

  protected def OptClassListField[C: ClassTag, MC <: CcMeta[C]](
      mc: MC
  ): OptCClassListField[C, MC, Meta]  = named(
    new OptCClassListField[C, MC, Meta](_, mc, this)
  )
  protected def OptClassListField[C: ClassTag, MC <: CcMeta[C]](
      name: String,
      mc: MC
  ): OptCClassListField[C, MC, Meta]  =
    named(name)(new OptCClassListField[C, MC, Meta](_, mc, this))

  protected def ClassArrayField[C: ClassTag, MC <: CcMeta[C]](
      mc: MC
  ): CClassArrayField[C, MC, Meta]  = named(
    new CClassArrayField[C, MC, Meta](_, mc, this)
  )
  protected def ClassArrayField[C: ClassTag, MC <: CcMeta[C]](
      name: String,
      mc: MC
  ): CClassArrayField[C, MC, Meta]  =
    named(name)(new CClassArrayField[C, MC, Meta](_, mc, this))

  protected def OptClassArrayField[C: ClassTag, MC <: CcMeta[C]](
      mc: MC
  ): OptCClassArrayField[C, MC, Meta]  = named(
    new OptCClassArrayField[C, MC, Meta](_, mc, this)
  )
  protected def OptClassArrayField[C: ClassTag, MC <: CcMeta[C]](
      name: String,
      mc: MC
  ): OptCClassArrayField[C, MC, Meta]  =
    named(name)(new OptCClassArrayField[C, MC, Meta](_, mc, this))

  protected def MapField[K: MapKeyFormat, V]: MapField[K, V, Meta]  =
    named(new MapField[K, V, Meta](_, this))
  protected def MapField[K: MapKeyFormat, V](
      name: String
  ): MapField[K, V, Meta]  =
    named(name)(new MapField[K, V, Meta](_, this))

  protected def OptMapField[V]: OptMapField[V, Meta]  = named(
    new OptMapField[V, Meta](_, this)
  )
  protected def OptMapField[V](name: String): OptMapField[V, Meta]  =
    named(name)(new OptMapField[V, Meta](_, this))

  protected def LocaleField: LocaleField[Meta]  = named(
    new LocaleField[Meta](_, this)
  )
  protected def LocaleField(name: String): LocaleField[Meta]  =
    named(name)(new LocaleField[Meta](_, this))

  protected def OptLocaleField: OptLocaleField[Meta]  = named(
    new OptLocaleField[Meta](_, this)
  )
  protected def OptLocaleField(name: String): OptLocaleField[Meta]  =
    named(name)(new OptLocaleField[Meta](_, this))

}

/*trait NamedQueryFieldHelpers[Meta]
    extends QueryFieldHelpers[Meta]
    with RuntimeNameResolver[Meta] {
  requires: Meta =>
}*/
