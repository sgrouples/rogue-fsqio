package me.sgrouples.rogue.cc.macros
import scala.deriving.*
import scala.annotation.experimental

trait MacroBsonFormatDeriving:
  transparent implicit inline def derived[A]: MacroBsonFormat[A] = ${
    MacroBsonFormatDerivingImpl.gen[A]
  }
//inline given derived[T](using m: Mirror.Of[T]): MacroBsonFormat[T] = MacroDerivationInline.derived