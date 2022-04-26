package me.sgrouples.rogue.cc.macros
import scala.deriving.*

trait MacroBsonFormatDeriving:
    //implicit inline def derived[A]: MacroBsonFormat[A] = ${ MacroBsonFormatDerivingImpl.gen[A] }
    inline given derived[T](using m: Mirror.Of[T]): MacroBsonFormat[T] = MacroDeriv2.derived
  

