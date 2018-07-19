package me.sgrouples.rogue.cc
import java.util.concurrent.atomic.AtomicInteger

import me.sgrouples.rogue.BsonFormat
import me.sgrouples.rogue.cc.macros.MacroCC._
import me.sgrouples.rogue.cc.macros.{ MacroBsonFormat }
import me.sgrouples.rogue.naming.{ LowerCase, NamingStrategy }
import shapeless.tag
import shapeless.tag._

import scala.collection.mutable
import scala.reflect.ClassTag

trait MacroNamesResolver[T] extends NamesResolver {
  private var resolved: Boolean = false
  private[this] val nameId = new AtomicInteger(-1)
  private[this] def nextNameId = nameId.incrementAndGet()
  private[this] def resolveError(id: Int): Nothing = throw new IllegalStateException(debugInfo(id))
  private[cc] def debugInfo(id: Int): String = {
    s"error macro resolving id ${id}"
  }
  private[this] val fields: mutable.Map[String, io.fsq.field.Field[_, _]] = mutable.Map.empty
  private[this] val names: mutable.Map[Int, String] = mutable.Map.empty[Int, String]

  def namesMap(): Vector[(Int, String)]
  //val macroGenProvided: MacroGen[T] = implicitly[MacroGen[T]]
  //implicitly[MacroGen[T]]
  def resolve()(implicit macroGen: MacroBsonFormat[T]): Unit = {
    val x = macroGen.namesMap()
    println(s"names from macro ${x}")
    names ++= x
    resolved = true
    println(s"Names Resolved - ${names}")
  }
  override def named[T <: io.fsq.field.Field[_, _]](name: String)(func: String => T): T @@ Marker = {
    if (!resolved) resolve()
    names += nextNameId -> name
    val field = func(name)
    if (fields.contains(name)) throw new IllegalArgumentException(s"Field with name $name is already defined")
    fields += (name -> field)
    tag[Marker][T](field)
  }
  override def named[T <: io.fsq.field.Field[_, _]](func: String => T): T @@ Marker = {
    if (!resolved) resolve()
    val nextId = nextNameId

    val name = names.getOrElse(nextId, resolveError(nextId))

    val field = func(name)
    if (fields.contains(name)) throw new IllegalArgumentException(s"Field with name $name is already defined")
    fields += (name -> field)
    tag[Marker][T](field)
  }
}
class MCcMetaExt[RecordType, OwnerType <: RCcMeta[RecordType]](collName: String)(implicit formats: BsonFormat[RecordType], macroGen: MacroBsonFormat[RecordType])
  extends RCcMeta[RecordType](collName)(formats)
  with QueryFieldHelpersBase[OwnerType] with MacroNamesResolver[RecordType] { requires: OwnerType =>
  override def namesMap(): Vector[(Int, String)] = macroGen.namesMap()
  def this(
    namingStrategy: NamingStrategy = LowerCase)(implicit formats: BsonFormat[RecordType], classTag: ClassTag[RecordType], macroGen: MacroBsonFormat[RecordType]) {
    this(namingStrategy[RecordType])(formats, macroGen)
  }
}