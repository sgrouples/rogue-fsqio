package me.sgrouples.rogue.cc.macros

import java.util.concurrent.atomic.AtomicInteger

import me.sgrouples.rogue.cc.{ Marker, NamesResolver }
import shapeless.tag
import shapeless.tag.@@

import scala.collection.mutable

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

  protected def macroGen: MacroBsonFormat[T]
  //val macroGenProvided: MacroGen[T] = implicitly[MacroGen[T]]
  //implicitly[MacroGen[T]]
  def resolve(): Unit = {
    val x = macroGen.namesMap()
    names ++= x
    resolved = true
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