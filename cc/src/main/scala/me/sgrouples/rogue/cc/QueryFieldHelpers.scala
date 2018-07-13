package me.sgrouples.rogue.cc

import java.util.concurrent.atomic.{ AtomicBoolean, AtomicInteger }

import me.sgrouples.rogue._
import me.sgrouples.rogue.cc.debug.Debug
import org.bson.types.ObjectId
import shapeless.tag
import shapeless.tag._

import scala.collection.mutable
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.reflect.{ ClassTag, api }
import Debug.DefaultImplicits._
import me.sgrouples.rogue.map.MapKeyFormat

private[cc] sealed trait Marker

private[cc] case class Visited(clazz: Type, fields: Seq[Symbol])

private[cc] case class Ignored(field: Symbol, reason: String)

private[cc] object DebugImplicits {

  implicit class IndentHelper(u: String) {
    def mkIndent: String = u.replaceAll("(?m)^", "  ")
  }

  implicit object DebugType extends Debug[Type] {
    override def show(t: universe.Type): String =
      t.typeSymbol.fullName
  }

  implicit object DebugSymbol extends Debug[Symbol] {
    override def show(t: universe.Symbol): String =
      t.name.toString
  }

  implicit object DebugVisited extends Debug[Visited] {
    override def show(t: Visited): String = t match {
      case Visited(clazz, Nil) => Debug.debug(clazz)
      case Visited(clazz, fields) =>
        s"""${Debug.debug(clazz)}
           |  ${Debug.debug(fields).mkIndent}"""
    }
  }

  implicit object DebugIgnored extends Debug[Ignored] {
    override def show(t: Ignored): String = t match {
      case Ignored(field, cause) =>
        s"""${Debug.debug(field)} ($cause)"""
    }
  }
}

trait NamesResolver {
  protected def named[T <: io.fsq.field.Field[_, _]](name: String)(func: String => T): T @@ Marker
  protected def named[T <: io.fsq.field.Field[_, _]](func: String => T): T @@ Marker
}

trait RuntimeNameResolver[Meta] extends NamesResolver {
  private[this] val lock = new Object

  private[this] val names: mutable.Map[Int, String] = mutable.Map.empty

  private[this] val fields: mutable.Map[String, io.fsq.field.Field[_, _]] = mutable.Map.empty

  private[this] val resolved = new AtomicBoolean(false)

  private[this] val nameId = new AtomicInteger(-1)

  private[this] val visitedClasses: mutable.ArrayBuffer[Visited] = mutable.ArrayBuffer.empty

  private[this] val ignoredFields: mutable.ArrayBuffer[Ignored] = mutable.ArrayBuffer.empty

  // This one is hacky, we need to find the type tag from Java's getClass method...

  private[this] implicit def typeTag: TypeTag[Meta] = {

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

  private[this] def resolve(): Unit = synchronized {

    /*
      The idea of automatic name resolution is taken from Scala's Enumeration,
      but imlemented without falling back to Java's reflection api.
     */

    val decode: Symbol => String = _.name.decodedName.toString.trim

    // we are looking for vals accessible from io.fsq.field.Field[_, _] and tagged with Marker

    def returnsMarkedField(symbol: Symbol): Boolean = {

      val typeArgs = symbol.asMethod.returnType.typeArgs

      if (!typeArgs.exists(_ =:= typeOf[Marker]))
        ignoredFields += Ignored(
          symbol,
          s"!typeArgs.exists(_ =:= typeOf[Marker]), " +
            s"typeArgs: ${typeArgs.mkString("[", ", ", "]")}")

      if (!typeArgs.exists(_ <:< typeOf[io.fsq.field.Field[_, _]]))
        ignoredFields += Ignored(
          symbol,
          s"!typeArgs.exists(_ <:< typeOf[io.fsq.field.Field[_, _]]), " +
            s"typeArgs: ${typeArgs.mkString("[", ", ", "]")}")

      typeArgs.exists(_ =:= typeOf[Marker]) &&
        typeArgs.exists(_ <:< typeOf[io.fsq.field.Field[_, _]])
    }

    /*
      So it seems rewrite of traits encoding in Scala 2.12
      was a good reason to simplify this little piece of code.
      I think this time its self-explanatory.
    */

    val valuesOfMarkedFieldTypeOnly: Symbol => Boolean = {
      case symbol if symbol.isTerm =>
        symbol.asTerm match {
          case term if term.isAccessor =>
            term.getter match {
              case getterSymbol if getterSymbol.isMethod =>
                returnsMarkedField(getterSymbol.asMethod)
              case s => ignoredFields += Ignored(s, "getter is not a method"); false
            }
          case s => ignoredFields += Ignored(s, "term is not an accessor"); false
        }
      case s => ignoredFields += Ignored(s, "symbol is not a term"); false
    }

    /*
      This reverse here is because traits are initialized in opposite order than declared...
     */

    val values = typeOf[Meta](typeTag).baseClasses.reverse.flatMap {
      baseClass =>

        val appType = appliedType(baseClass)

        val fields = appType.decls
          .sorted.filter(valuesOfMarkedFieldTypeOnly)

        visitedClasses += Visited(appType, fields)

        fields
    }.map(decode)

    // name map in order of trait linearization

    names ++= values.zipWithIndex.map(_.swap)
    println(s"names are ${names}")
    resolved.set(true)
  }

  /*
    This weird tagging by Marker trait is here because we need to filter out fields declared
    directly by calling constructors (like before we had this helper): `val name = new StringField("name", this)`
    They are both vals and do return a type assignable from `io.fsq.field.Field[_, _]`, so to know that those
    are not to be counted for name resolution, we must tag the right ones with a marking trait.
    Its complicated, I know, meta programming usually is... But Miles Sabin's @@ is awesome, don't you think?
   */

  override def named[T <: io.fsq.field.Field[_, _]](func: String => T): T @@ Marker = lock.synchronized {
    if (!resolved.get()) resolve() // lets try one more time to find those names

    val nextId = nextNameId

    val name = names.getOrElse(nextId, resolveError(nextId))

    val field = func(name)
    if (fields.contains(name)) throw new IllegalArgumentException(s"Field with name $name is already defined")
    fields += (name -> field)
    tag[Marker][T](field)
  }

  override def named[T <: io.fsq.field.Field[_, _]](name: String)(func: String => T): T @@ Marker = lock.synchronized {
    if (!resolved.get()) resolve()
    names += nextNameId -> name
    val field = func(name)
    if (fields.contains(name)) throw new IllegalArgumentException(s"Field with name $name is already defined")
    fields += (name -> field)
    tag[Marker][T](field)
  }

  private[cc] def debugInfo(id: Int): String = {

    import DebugImplicits._

    s"""Something went wrong: couldn't auto-resolve field names, pleace contact author at mikolaj@sgrouples.com
       | Class is ${this.getClass}, implicit type tag is: ${typeTag.tpe}
       | Was looking for index $id in
       |${Debug.debug(names.toSeq.sortBy(_._1).map(_._2)).mkIndent}
       | Classes visited during field resolution:
       |${Debug.debug(visitedClasses).mkIndent}
       | Fields not matching predicates:
       |${Debug.debug(ignoredFields.distinct).mkIndent}""".stripMargin
  }

  private[this] def resolveError(id: Int): Nothing = throw new IllegalStateException(debugInfo(id))
  def fieldNamesSorted: Seq[String] = Seq(names.toSeq.sortBy(_._1).map(_._2): _*) // making sure its a copy

}

/*
 // utility methods, not sure if they are usefull...
  def fieldByName[T <: io.fsq.field.Field[_, _]](name: String): T = fields(name).asInstanceOf[T]

  def fieldNames: Iterable[String] = Seq(names.values.toSeq: _*) // making sure its a copy
  def fieldNamesSorted: Seq[String] = Seq(names.toSeq.sortBy(_._1).map(_._2): _*) // making sure its a copy
  def fieldNamesWithIndexes: Map[Int, String] = Map(names.toSeq: _*) // making sure its a copy

 */

trait QueryFieldHelpers[Meta] extends NamesResolver {
  requires: Meta =>

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

  protected def BigDecimalField: BigDecimalField[Meta] @@ Marker = named(new BigDecimalField[Meta](_, this))
  protected def BigDecimalField(name: String): BigDecimalField[Meta] @@ Marker = named(name)(new BigDecimalField[Meta](_, this))

  protected def OptBigDecimalField: OptBigDecimalField[Meta] @@ Marker = named(new OptBigDecimalField[Meta](_, this))
  protected def OptBigDecimalField(name: String): OptBigDecimalField[Meta] @@ Marker = named(name)(new OptBigDecimalField[Meta](_, this))

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

  protected def ObjectIdSubtypeField[Subtype <: ObjectId]: ObjectIdSubtypeField[Subtype, Meta] @@ Marker = named(new ObjectIdSubtypeField[Subtype, Meta](_, this))
  protected def ObjectIdSubtypeField[Subtype <: ObjectId](name: String): ObjectIdSubtypeField[Subtype, Meta] @@ Marker = named(name)(new ObjectIdSubtypeField[Subtype, Meta](_, this))

  protected def OptObjectIdSubtypeField[Subtype <: ObjectId]: OptObjectIdSubtypeField[Subtype, Meta] @@ Marker = named(new OptObjectIdSubtypeField[Subtype, Meta](_, this))
  protected def OptObjectIdSubtypeField[Subtype <: ObjectId](name: String): OptObjectIdSubtypeField[Subtype, Meta] @@ Marker = named(name)(new OptObjectIdSubtypeField[Subtype, Meta](_, this))

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

  protected def CurrencyField: CurrencyField[Meta] @@ Marker = named(new CurrencyField[Meta](_, this))
  protected def CurrencyField(name: String): CurrencyField[Meta] @@ Marker = named(name)(new CurrencyField[Meta](_, this))

  protected def OptCurrencyField: OptCurrencyField[Meta] @@ Marker = named(new OptCurrencyField[Meta](_, this))
  protected def OptCurrencyField(name: String): OptCurrencyField[Meta] @@ Marker = named(name)(new OptCurrencyField[Meta](_, this))

  protected def InstantField: InstantField[Meta] @@ Marker = named(new InstantField[Meta](_, this))
  protected def InstantField(name: String): InstantField[Meta] @@ Marker = named(name)(new InstantField[Meta](_, this))

  protected def OptInstantField: OptInstantField[Meta] @@ Marker = named(new OptInstantField[Meta](_, this))
  protected def OptInstantField(name: String): OptInstantField[Meta] @@ Marker = named(name)(new OptInstantField[Meta](_, this))

  protected def BooleanField: BooleanField[Meta] @@ Marker = named(new BooleanField[Meta](_, this))
  protected def BooleanField(name: String): BooleanField[Meta] @@ Marker = named(name)(new BooleanField[Meta](_, this))

  protected def OptBooleanField: OptBooleanField[Meta] @@ Marker = named(new OptBooleanField[Meta](_, this))
  protected def OptBooleanField(name: String): OptBooleanField[Meta] @@ Marker = named(name)(new OptBooleanField[Meta](_, this))

  protected def EnumField[E <: Enumeration: TypeTag]: EnumField[E, Meta] @@ Marker = named(new EnumField[E, Meta](_, this))
  protected def EnumField[E <: Enumeration: TypeTag](name: String): EnumField[E, Meta] @@ Marker = named(name)(new EnumField[E, Meta](_, this))

  /**
   * This version of the EnumField method accepts e: E as a param to avoid ugly type parameters like [MyEnum.type]
   * So instead of writting val myEnum = EnumField[MyEnum.type, MyMeta] we can simply write val myEnum = EnumField(MyEnum)
   * @param e
   * @tparam E
   * @return
   */
  protected def EnumField[E <: Enumeration: TypeTag](e: E): EnumField[E, Meta] @@ Marker = named(new EnumField[E, Meta](_, this))
  protected def EnumField[E <: Enumeration: TypeTag](name: String, e: E): EnumField[E, Meta] @@ Marker = named(name)(new EnumField[E, Meta](_, this))

  protected def OptEnumField[E <: Enumeration]: OptEnumField[E, Meta] @@ Marker = named(new OptEnumField[E, Meta](_, this))
  protected def OptEnumField[E <: Enumeration](name: String): OptEnumField[E, Meta] @@ Marker = named(name)(new OptEnumField[E, Meta](_, this))

  /**
   * This version of the EnumField method accepts e: E as a param to avoid ugly type parameters like [MyEnum.type]
   * So instead of writting val myEnum = EnumField[MyEnum.type, MyMeta] we can simply write val myEnum = EnumField(MyEnum)
   * @param e
   * @tparam E
   * @return
   */
  protected def OptEnumField[E <: Enumeration](e: E): OptEnumField[E, Meta] @@ Marker = named(new OptEnumField[E, Meta](_, this))
  protected def OptEnumField[E <: Enumeration](name: String, e: E): OptEnumField[E, Meta] @@ Marker = named(name)(new OptEnumField[E, Meta](_, this))

  protected def EnumIdField[E <: Enumeration: TypeTag]: EnumIdField[E, Meta] @@ Marker = named(new EnumIdField[E, Meta](_, this))
  protected def EnumIdField[E <: Enumeration: TypeTag](name: String): EnumIdField[E, Meta] @@ Marker = named(name)(new EnumIdField[E, Meta](_, this))

  /**
   * This version of the EnumField method accepts e: E as a param to avoid ugly type parameters like [MyEnum.type]
   * So instead of writting val myEnum = EnumField[MyEnum.type, MyMeta] we can simply write val myEnum = EnumField(MyEnum)
   * @param e
   * @tparam E
   * @return
   */
  protected def EnumIdField[E <: Enumeration: TypeTag](e: E): EnumIdField[E, Meta] @@ Marker = named(new EnumIdField[E, Meta](_, this))
  protected def EnumIdField[E <: Enumeration: TypeTag](name: String, e: E): EnumIdField[E, Meta] @@ Marker = named(name)(new EnumIdField[E, Meta](_, this))

  protected def OptEnumIdField[E <: Enumeration]: OptEnumIdField[E, Meta] @@ Marker = named(new OptEnumIdField[E, Meta](_, this))
  protected def OptEnumIdField[E <: Enumeration](name: String): OptEnumIdField[E, Meta] @@ Marker = named(name)(new OptEnumIdField[E, Meta](_, this))

  /**
   * This version of the EnumField method accepts e: E as a param to avoid ugly type parameters like [MyEnum.type]
   * So instead of writting val myEnum = EnumField[MyEnum.type, MyMeta] we can simply write val myEnum = EnumField(MyEnum)
   * @param e
   * @tparam E
   * @return
   */
  protected def OptEnumIdField[E <: Enumeration](e: E): OptEnumIdField[E, Meta] @@ Marker = named(new OptEnumIdField[E, Meta](_, this))
  protected def OptEnumIdField[E <: Enumeration](name: String, e: E): OptEnumIdField[E, Meta] @@ Marker = named(name)(new OptEnumIdField[E, Meta](_, this))

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

  protected def OptClassListField[C: ClassTag, MC <: CcMeta[C]](mc: MC): OptCClassListField[C, MC, Meta] @@ Marker = named(new OptCClassListField[C, MC, Meta](_, mc, this))
  protected def OptClassListField[C: ClassTag, MC <: CcMeta[C]](name: String, mc: MC): OptCClassListField[C, MC, Meta] @@ Marker = named(name)(new OptCClassListField[C, MC, Meta](_, mc, this))

  protected def ClassArrayField[C: ClassTag, MC <: CcMeta[C]](mc: MC): CClassArrayField[C, MC, Meta] @@ Marker = named(new CClassArrayField[C, MC, Meta](_, mc, this))
  protected def ClassArrayField[C: ClassTag, MC <: CcMeta[C]](name: String, mc: MC): CClassArrayField[C, MC, Meta] @@ Marker = named(name)(new CClassArrayField[C, MC, Meta](_, mc, this))

  protected def OptClassArrayField[C: ClassTag, MC <: CcMeta[C]](mc: MC): OptCClassArrayField[C, MC, Meta] @@ Marker = named(new OptCClassArrayField[C, MC, Meta](_, mc, this))
  protected def OptClassArrayField[C: ClassTag, MC <: CcMeta[C]](name: String, mc: MC): OptCClassArrayField[C, MC, Meta] @@ Marker = named(name)(new OptCClassArrayField[C, MC, Meta](_, mc, this))

  protected def MapField[K: MapKeyFormat, V]: MapField[K, V, Meta] @@ Marker = named(new MapField[K, V, Meta](_, this))
  protected def MapField[K: MapKeyFormat, V](name: String): MapField[K, V, Meta] @@ Marker = named(name)(new MapField[K, V, Meta](_, this))

  protected def OptMapField[V]: OptMapField[V, Meta] @@ Marker = named(new OptMapField[V, Meta](_, this))
  protected def OptMapField[V](name: String): OptMapField[V, Meta] @@ Marker = named(name)(new OptMapField[V, Meta](_, this))

  protected def LocaleField: LocaleField[Meta] @@ Marker = named(new LocaleField[Meta](_, this))
  protected def LocaleField(name: String): LocaleField[Meta] @@ Marker = named(name)(new LocaleField[Meta](_, this))

  protected def OptLocaleField: OptLocaleField[Meta] @@ Marker = named(new OptLocaleField[Meta](_, this))
  protected def OptLocaleField(name: String): OptLocaleField[Meta] @@ Marker = named(name)(new OptLocaleField[Meta](_, this))

}
