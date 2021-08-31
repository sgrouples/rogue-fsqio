package me.sgrouples.rogue.cc.macros

import java.util.concurrent.atomic.AtomicInteger

import me.sgrouples.rogue.cc.{Marker, NamesResolver}
import shapeless.tag
import shapeless.tag.@@

import scala.collection.mutable

trait MacroNamesResolver[T] extends NamesResolver {
  private var resolved: Boolean = false
  private[this] val nameId = new AtomicInteger(-1)
  private[this] def nextNameId = nameId.incrementAndGet()
  private[this] def resolveError(id: Int): Nothing =
    throw new IllegalStateException(debugInfo(id))
  private[cc] def debugInfo(id: Int): String = {
    s"error macro resolving id ${id}"
  }
  private[this] val fields: mutable.Map[String, io.fsq.field.Field[_, _]] =
    mutable.Map.empty
  private[this] val _validNames: mutable.Set[String] = mutable.Set.empty[String]

  protected def macroGen: MacroBsonFormat[T]
  //val macroGenProvided: MacroGen[T] = implicitly[MacroGen[T]]
  //implicitly[MacroGen[T]]
  def resolve(): Unit = {
    // _validNames ++= macroGen.validNames()
    resolved = true
  }
  override def named[T <: io.fsq.field.Field[_, _]](
      name: String
  )(func: String => T): T @@ Marker = {
    if (!resolved) resolve()
    //if (!_validNames.contains(name)) throw new IllegalArgumentException(s"no field named ${name} found in ${this.getClass.getName}")
    val field = func(name)
    if (fields.contains(name))
      throw new IllegalArgumentException(
        s"Field with name $name is already defined"
      )
    fields += (name -> field)

    tag[Marker][T](field)
  }
  override def named[T <: io.fsq.field.Field[_, _]](
      func: String => T
  ): T @@ Marker = {
    val caller = Thread.currentThread().getStackTrace()(1)
    new RuntimeException().printStackTrace()
    throw new IllegalArgumentException(
      s"[${caller.getClassName}:L${caller.getLineNumber}] named without name not supported in macros. use @f [me.sgrouples.rogue.cc.macros.f] or provide name"
    )
  }
}
