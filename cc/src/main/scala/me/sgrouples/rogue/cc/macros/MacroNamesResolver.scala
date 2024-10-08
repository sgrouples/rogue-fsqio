package me.sgrouples.rogue.cc.macros

import java.util.concurrent.atomic.AtomicInteger

import me.sgrouples.rogue.cc.{NamesResolver}

import scala.collection.mutable

trait MacroNamesResolver[T] extends NamesResolver {
  private var resolved: Boolean = false
  private[this] val nameId = new AtomicInteger(-1)
  private[this] def nextNameId = nameId.incrementAndGet()

  private[this] val fields: mutable.Map[String, io.fsq.field.Field[_, _]] =
    mutable.Map.empty
  private[this] val _validNames: mutable.Set[String] = mutable.Set.empty[String]

  protected def bsonFormat: MacroBsonFormat[T]
  def resolve(): Unit = {
    // _validNames ++= macroGen.validNames()
    resolved = true
  }
  override def named[T <: io.fsq.field.Field[?, ?]](
      name: String
  )(func: String => T): T = {
    if (!resolved) resolve()
    //if (!_validNames.contains(name)) throw new IllegalArgumentException(s"no field named ${name} found in ${this.getClass.getName}")
    val field = func(name)
    if (fields.contains(name))
      throw new IllegalArgumentException(
        s"Field with name $name is already defined"
      )
    fields += (name -> field)

    field
  }
}
