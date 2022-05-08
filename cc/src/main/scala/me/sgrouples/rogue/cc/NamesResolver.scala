package me.sgrouples.rogue.cc

trait NamesResolver {
  protected def named[T <: io.fsq.field.Field[?, ?]](name: String)(
      func: String => T
  ): T
  protected def named[T <: io.fsq.field.Field[?, ?]](
      func: String => T
  ): T 
}
