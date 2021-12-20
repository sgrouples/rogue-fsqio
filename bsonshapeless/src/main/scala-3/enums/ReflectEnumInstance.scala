package me.sgrouples.rogue.enums

trait ReflectEnumInstance[T <: Enumeration] {
  protected def enumeration: T = ???
}
