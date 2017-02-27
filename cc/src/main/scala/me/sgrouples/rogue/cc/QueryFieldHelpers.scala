package me.sgrouples.rogue.cc

import java.util.concurrent.atomic.AtomicInteger

import me.sgrouples.rogue._

import scala.reflect.{ ClassTag, api }
import scala.collection.mutable
import scala.reflect.runtime.universe._
import shapeless.tag
import shapeless.tag._

private[cc] sealed trait Marker

trait QueryFieldHelpers[Meta] extends {
  requires: Meta =>

  private[this] val names: mutable.Map[Int, String] = mutable.Map.empty

  private[this] val fields: mutable.Map[String, io.fsq.field.Field[_, _]] = mutable.Map.empty

  private[this] val nameId = new AtomicInteger(-1)

  // This one is hacky, we need to find the type tag from Java's getClass method...

  private[this] implicit val typeTag: TypeTag[Meta] = {

    val mirror = runtimeMirror(getClass.getClassLoader)
    val sym = mirror.classSymbol(getClass)
    val tpe = sym.selfType

    TypeTag(mirror, new api.TypeCreator {
      def apply[U <: api.Universe with Singleton](m: api.Mirror[U]): U#Type =
        if (m eq mirror) tpe.asInstanceOf[U#Type]
        else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be migrated to other mirrors.")
    })
  }

  private[this] def nextNameId = nameId.incrementAndGet()

  private[this] def resolve(): Unit = {

    /*
      The idea of automatic name resolution is taken from Scala's Enumeration,
      but imlemented without falling back to Java's reflection api.
     */

    val decode: Symbol => String = _.name.decodedName.toString.trim

    // we are looking for vals accessible from io.fsq.field.Field[_, _] and tagged with Marker

    val returnsMarkedRogueField: Symbol => Boolean = { symbol =>

      val typeArgs = symbol.asMethod.returnType.typeArgs

      typeArgs.exists(_ =:= typeOf[Marker]) &&
        typeArgs.exists(_ <:< typeOf[io.fsq.field.Field[_, _]])
    }

    val valueGettersOnly: Symbol => Boolean = {
      x => x.isMethod && !x.isSynthetic && returnsMarkedRogueField(x)
    }

    val valueGetters: Map[String, MethodSymbol] = {
      typeOf[Meta].baseClasses.foldLeft(Map.empty[String, MethodSymbol]) {
        case (acc, baseClass) =>
          acc ++ appliedType(baseClass).decls.filter(valueGettersOnly).map {
            getter => decode(getter) -> getter.asMethod
          }(scala.collection.breakOut): Map[String, MethodSymbol]
      }
    }

    /*
      We need to have those values in the order of trait linearization,
      but the api doesn't guarantee order for synthetic members, so we just need
      to iterate over declared vals but match them with synthetic getters, easy!
    */

    val valuesOnly: Symbol => Boolean = {
      x => !x.isMethod && valueGetters.contains(decode(x))
    }

    /*
      This reverse here is because traits are initialized in oposite order than declared...
     */

    val values = typeOf[Meta].baseClasses.reverse.flatMap {
      baseClass => appliedType(baseClass).decls.sorted.filter(valuesOnly)
    }.map(decode)

    // name map in order of trait linearization

    names ++= values.zipWithIndex.map(_.swap)

  }

  /*
    This weird tagging by Marker trait is here because we need to filter out fields declared
    directly by calling constructors (like before we had this helper): `val name = new StringField("name", this)`
    They are both vals and do return a type assignable from `io.fsq.field.Field[_, _]`, so to know that those
    are not to be counted for name resolution, we must tag the right ones with a marking trait.
    Its complicated, I know, meta programming usually is... But Miles Sabin's @@ is awesome, don't you think?
   */

  protected def named[T <: io.fsq.field.Field[_, _]](func: String => T): T @@ Marker = {
    if (names.isEmpty) resolve()

    val name = names.getOrElse(nextNameId, throw new IllegalStateException(
      "Something went wrong: couldn't auto-resolve field names, pleace contact author at mikolaj@sgrouples.com"
    ))

    val field = func(name)
    if (fields.contains(name)) throw new IllegalArgumentException(s"Field with name $name is already defined")
    fields += (name -> field)
    tag[Marker][T](field)
  }

  protected def named[T <: io.fsq.field.Field[_, _]](name: String)(func: String => T): T @@ Marker = {
    if (names.isEmpty) resolve()
    names += nextNameId -> name
    val field = func(name)
    if (fields.contains(name)) throw new IllegalArgumentException(s"Field with name $name is already defined")
    fields += (name -> field)
    tag[Marker][T](field)
  }

  // utility methods, not sure if they are usefull...
  def fieldByName[T <: io.fsq.field.Field[_, _]](name: String): T = fields(name).asInstanceOf[T]
  def fieldNames: Iterable[String] = names.values

  protected def IntField: IntField[Meta] @@ Marker = named(new IntField[Meta](_, this))
  protected def IntField(name: String): IntField[Meta] @@ Marker = named(name)(new IntField[Meta](_, this))

  protected def OptIntField: OptIntField[Meta] @@ Marker = named(new OptIntField[Meta](_, this))
  protected def OptIntField(name: String): OptIntField[Meta] @@ Marker = named(name)(new OptIntField[Meta](_, this))

  protected def IntTaggedField[Tag]: IntTaggedField[Tag, Meta] @@ Marker = named(new IntTaggedField[Tag, Meta](_, this))
  protected def IntTaggedField[Tag](name: String): IntTaggedField[Tag, Meta] @@ Marker = named(name)(new IntTaggedField[Tag, Meta](_, this))

  protected def OptIntTaggedField[Tag]: OptIntTaggedField[Tag, Meta] @@ Marker = named(new OptIntTaggedField[Tag, Meta](_, this))
  protected def OptIntTaggedField[Tag](name: String): OptIntTaggedField[Tag, Meta] @@ Marker = named(name)(new OptIntTaggedField[Tag, Meta](_, this))

  protected def StringField: StringField[Meta] @@ Marker = named(new StringField[Meta](_, this))
  protected def StringField(name: String): StringField[Meta] @@ Marker = named(name)(new StringField[Meta](_, this))

  protected def OptStringField: OptStringField[Meta] @@ Marker = named(new OptStringField[Meta](_, this))
  protected def OptStringField(name: String): OptStringField[Meta] @@ Marker = named(name)(new OptStringField[Meta](_, this))

  protected def StringTaggedField[Tag]: StringTaggedField[Tag, Meta] @@ Marker = named(new StringTaggedField[Tag, Meta](_, this))
  protected def StringTaggedField[Tag](name: String): StringTaggedField[Tag, Meta] @@ Marker = named(name)(new StringTaggedField[Tag, Meta](_, this))

  protected def OptStringTaggedField[Tag]: OptStringTaggedField[Tag, Meta] @@ Marker = named(new OptStringTaggedField[Tag, Meta](_, this))
  protected def OptStringTaggedField[Tag](name: String): OptStringTaggedField[Tag, Meta] @@ Marker = named(name)(new OptStringTaggedField[Tag, Meta](_, this))

  protected def LongField: LongField[Meta] @@ Marker = named(new LongField[Meta](_, this))
  protected def LongField(name: String): LongField[Meta] @@ Marker = named(name)(new LongField[Meta](_, this))

  protected def OptLongField: OptLongField[Meta] @@ Marker = named(new OptLongField[Meta](_, this))
  protected def OptLongField(name: String): OptLongField[Meta] @@ Marker = named(name)(new OptLongField[Meta](_, this))

  protected def LongTaggedField[Tag]: LongTaggedField[Tag, Meta] @@ Marker = named(new LongTaggedField[Tag, Meta](_, this))
  protected def LongTaggedField[Tag](name: String): LongTaggedField[Tag, Meta] @@ Marker = named(name)(new LongTaggedField[Tag, Meta](_, this))

  protected def OptLongTaggedField[Tag]: OptLongTaggedField[Tag, Meta] @@ Marker = named(new OptLongTaggedField[Tag, Meta](_, this))
  protected def OptLongTaggedField[Tag](name: String): OptLongTaggedField[Tag, Meta] @@ Marker = named(name)(new OptLongTaggedField[Tag, Meta](_, this))

  protected def DoubleField: DoubleField[Meta] @@ Marker = named(new DoubleField[Meta](_, this))
  protected def DoubleField(name: String): DoubleField[Meta] @@ Marker = named(name)(new DoubleField[Meta](_, this))

  protected def OptDoubleField: OptDoubleField[Meta] @@ Marker = named(new OptDoubleField[Meta](_, this))
  protected def OptDoubleField(name: String): OptDoubleField[Meta] @@ Marker = named(name)(new OptDoubleField[Meta](_, this))

  protected def ObjectIdField: ObjectIdField[Meta] @@ Marker = named(new ObjectIdField[Meta](_, this))
  protected def ObjectIdField(name: String): ObjectIdField[Meta] @@ Marker = named(name)(new ObjectIdField[Meta](_, this))

  protected def OptObjectIdField: OptObjectIdField[Meta] @@ Marker = named(new OptObjectIdField[Meta](_, this))
  protected def OptObjectIdField(name: String): OptObjectIdField[Meta] @@ Marker = named(name)(new OptObjectIdField[Meta](_, this))

  protected def ObjectIdTaggedField[Tag]: ObjectIdTaggedField[Tag, Meta] @@ Marker = named(new ObjectIdTaggedField[Tag, Meta](_, this))
  protected def ObjectIdTaggedField[Tag](name: String): ObjectIdTaggedField[Tag, Meta] @@ Marker = named(name)(new ObjectIdTaggedField[Tag, Meta](_, this))

  protected def OptObjectIdTaggedField[Tag]: OptObjectIdTaggedField[Tag, Meta] @@ Marker = named(new OptObjectIdTaggedField[Tag, Meta](_, this))
  protected def OptObjectIdTaggedField[Tag](name: String): OptObjectIdTaggedField[Tag, Meta] @@ Marker = named(name)(new OptObjectIdTaggedField[Tag, Meta](_, this))

  protected def UUIdField: UUIDIdField[Meta] @@ Marker = named(new UUIDIdField[Meta](_, this))
  protected def UUIdField(name: String): UUIDIdField[Meta] @@ Marker = named(name)(new UUIDIdField[Meta](_, this))

  protected def OptUUIdField: OptUUIDIdField[Meta] @@ Marker = named(new OptUUIDIdField[Meta](_, this))
  protected def OptUUIdField(name: String): OptUUIDIdField[Meta] @@ Marker = named(name)(new OptUUIDIdField[Meta](_, this))

  protected def UUIdTaggedField[Tag]: UUIDIdTaggedField[Tag, Meta] @@ Marker = named(new UUIDIdTaggedField[Tag, Meta](_, this))
  protected def UUIdTaggedField[Tag](name: String): UUIDIdTaggedField[Tag, Meta] @@ Marker = named(name)(new UUIDIdTaggedField[Tag, Meta](_, this))

  protected def OptUUIdTaggedField[Tag]: OptUUIDIdTaggedField[Tag, Meta] @@ Marker = named(new OptUUIDIdTaggedField[Tag, Meta](_, this))
  protected def OptUUIdTaggedField[Tag](name: String): OptUUIDIdTaggedField[Tag, Meta] @@ Marker = named(name)(new OptUUIDIdTaggedField[Tag, Meta](_, this))

  protected def LocalDateTimeField: LocalDateTimeField[Meta] @@ Marker = named(new LocalDateTimeField[Meta](_, this))
  protected def LocalDateTimeField(name: String): LocalDateTimeField[Meta] @@ Marker = named(name)(new LocalDateTimeField[Meta](_, this))

  protected def OptLocalDateTimeField: OptLocalDateTimeField[Meta] @@ Marker = named(new OptLocalDateTimeField[Meta](_, this))
  protected def OptLocalDateTimeField(name: String): OptLocalDateTimeField[Meta] @@ Marker = named(name)(new OptLocalDateTimeField[Meta](_, this))

  protected def InstantField: InstantField[Meta] @@ Marker = named(new InstantField[Meta](_, this))
  protected def InstantField(name: String): InstantField[Meta] @@ Marker = named(name)(new InstantField[Meta](_, this))

  protected def OptInstantField: OptInstantField[Meta] @@ Marker = named(new OptInstantField[Meta](_, this))
  protected def OptInstantField(name: String): OptInstantField[Meta] @@ Marker = named(name)(new OptInstantField[Meta](_, this))

  protected def BooleanField: BooleanField[Meta] @@ Marker = named(new BooleanField[Meta](_, this))
  protected def BooleanField(name: String): BooleanField[Meta] @@ Marker = named(name)(new BooleanField[Meta](_, this))

  protected def OptBooleanField: OptBooleanField[Meta] @@ Marker = named(new OptBooleanField[Meta](_, this))
  protected def OptBooleanField(name: String): OptBooleanField[Meta] @@ Marker = named(name)(new OptBooleanField[Meta](_, this))

  protected def EnumField[E <: Enumeration](implicit e: E): EnumField[E, Meta] @@ Marker = named(new EnumField[E, Meta](_, this)(e))
  protected def EnumField[E <: Enumeration](name: String)(implicit e: E): EnumField[E, Meta] = named(name)(new EnumField[E, Meta](_, this)(e))

  protected def OptEnumField[E <: Enumeration](implicit e: E): OptEnumField[E, Meta] @@ Marker = named(new OptEnumField[E, Meta](_, this)(e))
  protected def OptEnumField[E <: Enumeration](name: String)(implicit e: E): OptEnumField[E, Meta] = named(name)(new OptEnumField[E, Meta](_, this)(e))

  protected def EnumIdField[E <: Enumeration](implicit e: E): EnumIdField[E, Meta] @@ Marker = named(new EnumIdField[E, Meta](_, this)(e))
  protected def EnumIdField[E <: Enumeration](name: String)(implicit e: E): EnumIdField[E, Meta] = named(name)(new EnumIdField[E, Meta](_, this)(e))

  protected def OptEnumIdField[E <: Enumeration](implicit e: E): OptEnumIdField[E, Meta] @@ Marker = named(new OptEnumIdField[E, Meta](_, this)(e))
  protected def OptEnumIdField[E <: Enumeration](name: String)(implicit e: E): OptEnumIdField[E, Meta] = named(name)(new OptEnumIdField[E, Meta](_, this)(e))

  protected def ListField[V]: ListField[V, Meta] @@ Marker = named(new ListField[V, Meta](_, this))
  protected def ListField[V](name: String): ListField[V, Meta] @@ Marker = named(name)(new ListField[V, Meta](_, this))

  protected def OptListField[V]: OptListField[V, Meta] @@ Marker = named(new OptListField[V, Meta](_, this))
  protected def OptListField[V](name: String): OptListField[V, Meta] @@ Marker = named(name)(new OptListField[V, Meta](_, this))

  protected def ArrayField[V: ClassTag]: ArrayField[V, Meta] @@ Marker = named(new ArrayField[V, Meta](_, this))
  protected def ArrayField[V: ClassTag](name: String): ArrayField[V, Meta] @@ Marker = named(name)(new ArrayField[V, Meta](_, this))

  protected def OptArrayField[V: ClassTag]: OptArrayField[V, Meta] @@ Marker = named(new OptArrayField[V, Meta](_, this))
  protected def OptArrayField[V: ClassTag](name: String): OptArrayField[V, Meta] @@ Marker = named(name)(new OptArrayField[V, Meta](_, this))

  protected def ClassField[C, MC <: CcMeta[C]](mc: MC): CClassField[C, MC, Meta] @@ Marker = named(new CClassField[C, MC, Meta](_, mc, this))
  protected def ClassField[C, MC <: CcMeta[C]](name: String, mc: MC): CClassField[C, MC, Meta] @@ Marker = named(name)(new CClassField[C, MC, Meta](_, mc, this))

  protected def OptClassField[C, MC <: CcMeta[C]](mc: MC): OptCClassField[C, MC, Meta] @@ Marker = named(new OptCClassField[C, MC, Meta](_, mc, this))
  protected def OptClassField[C, MC <: CcMeta[C]](name: String, mc: MC): OptCClassField[C, MC, Meta] @@ Marker = named(name)(new OptCClassField[C, MC, Meta](_, mc, this))

  protected def ClassRequiredField[C, MC <: CcMeta[C]](mc: MC, default: C): CClassRequiredField[C, MC, Meta] @@ Marker = named(new CClassRequiredField(_, mc, default, this))
  protected def ClassRequiredField[C, MC <: CcMeta[C]](name: String, mc: MC, default: C): CClassRequiredField[C, MC, Meta] @@ Marker = named(name)(new CClassRequiredField(_, mc, default, this))

  protected def ClassListField[C: ClassTag, MC <: CcMeta[C]](mc: MC): CClassListField[C, MC, Meta] @@ Marker = named(new CClassListField[C, MC, Meta](_, mc, this))
  protected def ClassListField[C: ClassTag, MC <: CcMeta[C]](name: String, mc: MC): CClassListField[C, MC, Meta] @@ Marker = named(name)(new CClassListField[C, MC, Meta](_, mc, this))
  // TODO: OptClassListField?

  protected def ClassArrayField[C: ClassTag, MC <: CcMeta[C]](mc: MC): CClassArrayField[C, MC, Meta] @@ Marker = named(new CClassArrayField[C, MC, Meta](_, mc, this))
  protected def ClassArrayField[C: ClassTag, MC <: CcMeta[C]](name: String, mc: MC): CClassArrayField[C, MC, Meta] @@ Marker = named(name)(new CClassArrayField[C, MC, Meta](_, mc, this))
  // TODO: OptClassArrayField?

  protected def MapField[V]: MapField[V, Meta] @@ Marker = named(new MapField[V, Meta](_, this))
  protected def MapField[V](name: String): MapField[V, Meta] @@ Marker = named(name)(new MapField[V, Meta](_, this))

  protected def OptMapField[V]: OptMapField[V, Meta] @@ Marker = named(new OptMapField[V, Meta](_, this))
  protected def OptMapField[V](name: String): OptMapField[V, Meta] @@ Marker = named(name)(new OptMapField[V, Meta](_, this))
}
