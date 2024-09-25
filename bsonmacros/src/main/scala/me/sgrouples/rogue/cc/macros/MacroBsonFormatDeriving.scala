package me.sgrouples.rogue.cc.macros
import scala.deriving.*
import scala.annotation.experimental

trait MacroBsonFormatDeriving:
  transparent inline def derived[A]: MacroBsonFormat[A] = ${
    MacroBsonFormatDerivingImpl.gen[A]
  }

object MacroBsonFormatAuto:
  transparent implicit inline def derivedAuto[A]: MacroBsonFormat[A] = ${
    MacroBsonFormatDerivingImpl.genAuto[A]
  }
