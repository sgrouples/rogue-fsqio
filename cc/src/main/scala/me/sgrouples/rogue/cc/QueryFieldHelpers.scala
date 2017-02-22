package me.sgrouples.rogue.cc

import java.util.concurrent.atomic.AtomicInteger

import me.sgrouples.rogue._

import scala.reflect.{ ClassTag, api }
import scala.collection.mutable
import scala.reflect.runtime.universe._

trait QueryFieldHelpers[Meta] extends {
  requires: Meta =>

  private val names: mutable.Map[Int, String] = mutable.Map.empty

  private val nameId = new AtomicInteger(-1)

  // This one is hacky, we need to find the type tag from Java's getClass method...

  private implicit val typeTag: TypeTag[Meta] = {

    val mirror = runtimeMirror(getClass.getClassLoader)
    val sym = mirror.staticClass(getClass.getName)
    val tpe = sym.selfType

    TypeTag(mirror, new api.TypeCreator {
      def apply[U <: api.Universe with Singleton](m: api.Mirror[U]): U#Type =
        if (m eq mirror) tpe.asInstanceOf[U#Type]
        else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be migrated to other mirrors.")
    })
  }

  private def nextNameId = nameId.incrementAndGet()

  private def resolve(): Unit = {

    /*
      The idea of automatic name resolution is taken from Scala's Enumeration,
      but imlemented without falling back to Java's reflection api.
     */

    val decode: Symbol => String = _.name.decodedName.toString.trim

    // we are looking for vals accessible from io.fsq.field.Field[_, _]

    val returnsRogueField: Symbol => Boolean = {
      _.asMethod.returnType <:< typeOf[io.fsq.field.Field[_, _]]
    }

    val valueGettersOnly: Symbol => Boolean = {
      x => x.isMethod && !x.isSynthetic && returnsRogueField(x)
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

  private def named[T](func: String => T): T = {
    if (names.isEmpty) resolve()
    val name = names(nextNameId)
    func(name)
  }

  private def named[T](name: String)(func: String => T): T = {
    if (names.isEmpty) resolve()
    names += nextNameId -> name
    func(name)
  }

  protected def IntField: IntField[Meta] = named(new IntField[Meta](_, this))
  protected def IntField(name: String): IntField[Meta] = named(name)(new IntField[Meta](_, this))

  protected def OptIntField: OptIntField[Meta] = named(new OptIntField[Meta](_, this))
  protected def OptIntField(name: String): OptIntField[Meta] = named(name)(new OptIntField[Meta](_, this))

  protected def StringField: StringField[Meta] = named(new StringField[Meta](_, this))
  protected def StringField(name: String): StringField[Meta] = named(name)(new StringField[Meta](_, this))

  protected def OptStringField: OptStringField[Meta] = named(new OptStringField[Meta](_, this))
  protected def OptStringField(name: String): OptStringField[Meta] = named(name)(new OptStringField[Meta](_, this))

  protected def LongField: LongField[Meta] = named(new LongField[Meta](_, this))
  protected def LongField(name: String): LongField[Meta] = named(name)(new LongField[Meta](_, this))

  protected def OptLongField: OptLongField[Meta] = named(new OptLongField[Meta](_, this))
  protected def OptLongField(name: String): OptLongField[Meta] = named(name)(new OptLongField[Meta](_, this))

  protected def DoubleField: DoubleField[Meta] = named(new DoubleField[Meta](_, this))
  protected def DoubleField(name: String): DoubleField[Meta] = named(name)(new DoubleField[Meta](_, this))

  protected def OptDoubleField: OptDoubleField[Meta] = named(new OptDoubleField[Meta](_, this))
  protected def OptDoubleField(name: String): OptDoubleField[Meta] = named(name)(new OptDoubleField[Meta](_, this))

  protected def ObjectIdField: ObjectIdField[Meta] = named(new ObjectIdField[Meta](_, this))
  protected def ObjectIdField(name: String): ObjectIdField[Meta] = named(name)(new ObjectIdField[Meta](_, this))

  protected def OptObjectIdField: OptObjectIdField[Meta] = named(new OptObjectIdField[Meta](_, this))
  protected def OptObjectIdField(name: String): OptObjectIdField[Meta] = named(name)(new OptObjectIdField[Meta](_, this))

  protected def UUIDField: UUIDIdField[Meta] = named(new UUIDIdField[Meta](_, this))
  protected def UUIDField(name: String): UUIDIdField[Meta] = named(name)(new UUIDIdField[Meta](_, this))

  protected def OptUUIDField: OptUUIDIdField[Meta] = named(new OptUUIDIdField[Meta](_, this))
  protected def OptUUIDField(name: String): OptUUIDIdField[Meta] = named(name)(new OptUUIDIdField[Meta](_, this))

  protected def LocalDateTimeField: LocalDateTimeField[Meta] = named(new LocalDateTimeField[Meta](_, this))
  protected def LocalDateTimeField(name: String): LocalDateTimeField[Meta] = named(name)(new LocalDateTimeField[Meta](_, this))

  protected def OptLocalDateTimeField: OptLocalDateTimeField[Meta] = named(new OptLocalDateTimeField[Meta](_, this))
  protected def OptLocalDateTimeField(name: String): OptLocalDateTimeField[Meta] = named(name)(new OptLocalDateTimeField[Meta](_, this))

  protected def InstantField: InstantField[Meta] = named(new InstantField[Meta](_, this))
  protected def InstantField(name: String): InstantField[Meta] = named(name)(new InstantField[Meta](_, this))

  protected def OptInstantField: OptInstantField[Meta] = named(new OptInstantField[Meta](_, this))
  protected def OptInstantField(name: String): OptInstantField[Meta] = named(name)(new OptInstantField[Meta](_, this))

  protected def BooleanField: BooleanField[Meta] = named(new BooleanField[Meta](_, this))
  protected def BooleanField(name: String): BooleanField[Meta] = named(name)(new BooleanField[Meta](_, this))

  protected def OptBooleanField: OptBooleanField[Meta] = named(new OptBooleanField[Meta](_, this))
  protected def OptBooleanField(name: String): OptBooleanField[Meta] = named(name)(new OptBooleanField[Meta](_, this))

  protected def EnumField[E <: Enumeration](implicit e: E): EnumField[E, Meta] = named(new EnumField[E, Meta](_, this)(e))
  protected def EnumField[E <: Enumeration](name: String)(implicit e: E): EnumField[E, Meta] = named(name)(new EnumField[E, Meta](_, this)(e))

  protected def OptEnumField[E <: Enumeration](implicit e: E): OptEnumField[E, Meta] = named(new OptEnumField[E, Meta](_, this)(e))
  protected def OptEnumField[E <: Enumeration](name: String)(implicit e: E): OptEnumField[E, Meta] = named(name)(new OptEnumField[E, Meta](_, this)(e))

  protected def EnumIdField[E <: Enumeration](implicit e: E): EnumIdField[E, Meta] = named(new EnumIdField[E, Meta](_, this)(e))
  protected def EnumIdField[E <: Enumeration](name: String)(implicit e: E): EnumIdField[E, Meta] = named(name)(new EnumIdField[E, Meta](_, this)(e))

  protected def OptEnumIdField[E <: Enumeration](implicit e: E): OptEnumIdField[E, Meta] = named(new OptEnumIdField[E, Meta](_, this)(e))
  protected def OptEnumIdField[E <: Enumeration](name: String)(implicit e: E): OptEnumIdField[E, Meta] = named(name)(new OptEnumIdField[E, Meta](_, this)(e))

  protected def ListField[V]: ListField[V, Meta] = named(new ListField[V, Meta](_, this))
  protected def ListField[V](name: String): ListField[V, Meta] = named(name)(new ListField[V, Meta](_, this))

  protected def OptListField[V]: OptListField[V, Meta] = named(new OptListField[V, Meta](_, this))
  protected def OptListField[V](name: String): OptListField[V, Meta] = named(name)(new OptListField[V, Meta](_, this))

  protected def ArrayField[V: ClassTag]: ArrayField[V, Meta] = named(new ArrayField[V, Meta](_, this))
  protected def ArrayField[V: ClassTag](name: String): ArrayField[V, Meta] = named(name)(new ArrayField[V, Meta](_, this))

  protected def OptArrayField[V: ClassTag]: OptArrayField[V, Meta] = named(new OptArrayField[V, Meta](_, this))
  protected def OptArrayField[V: ClassTag](name: String): OptArrayField[V, Meta] = named(name)(new OptArrayField[V, Meta](_, this))

  protected def ClassField[C, MC <: CcMeta[C]](mc: MC): CClassField[C, MC, Meta] = named(new CClassField[C, MC, Meta](_, mc, this))
  protected def ClassField[C, MC <: CcMeta[C]](name: String, mc: MC): CClassField[C, MC, Meta] = named(name)(new CClassField[C, MC, Meta](_, mc, this))

  protected def OptClassField[C, MC <: CcMeta[C]](mc: MC): OptCClassField[C, MC, Meta] = named(new OptCClassField[C, MC, Meta](_, mc, this))
  protected def OptClassField[C, MC <: CcMeta[C]](name: String, mc: MC): OptCClassField[C, MC, Meta] = named(name)(new OptCClassField[C, MC, Meta](_, mc, this))

  protected def ClassRequiredField[C, MC <: CcMeta[C]](mc: MC, default: C): CClassRequiredField[C, MC, Meta] = named(new CClassRequiredField(_, mc, default, this))
  protected def ClassRequiredField[C, MC <: CcMeta[C]](name: String, mc: MC, default: C): CClassRequiredField[C, MC, Meta] = named(name)(new CClassRequiredField(_, mc, default, this))

  protected def ClassListField[C: ClassTag, MC <: CcMeta[C]](mc: MC): CClassListField[C, MC, Meta] = named(new CClassListField[C, MC, Meta](_, mc, this))
  protected def ClassListField[C: ClassTag, MC <: CcMeta[C]](name: String, mc: MC): CClassListField[C, MC, Meta] = named(name)(new CClassListField[C, MC, Meta](_, mc, this))
  // TODO: OptClassListField?

  protected def ClassArrayField[C: ClassTag, MC <: CcMeta[C]](mc: MC): CClassArrayField[C, MC, Meta] = named(new CClassArrayField[C, MC, Meta](_, mc, this))
  protected def ClassArrayField[C: ClassTag, MC <: CcMeta[C]](name: String, mc: MC): CClassArrayField[C, MC, Meta] = named(name)(new CClassArrayField[C, MC, Meta](_, mc, this))
  // TODO: OptClassArrayField?

  protected def MapField[V]: MapField[V, Meta] = named(new MapField[V, Meta](_, this))
  protected def MapField[V](name: String): MapField[V, Meta] = named(name)(new MapField[V, Meta](_, this))

  protected def OptMapField[V]: OptMapField[V, Meta] = named(new OptMapField[V, Meta](_, this))
  protected def OptMapField[V](name: String): OptMapField[V, Meta] = named(name)(new OptMapField[V, Meta](_, this))
}
