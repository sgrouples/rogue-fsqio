package me.sgrouples.rogue.cc

import org.scalatest.{ FlatSpec, Matchers }

import me.sgrouples.rogue.cc._

class MacroTest extends FlatSpec with Matchers {

  "Macro" should "bla cclass" in {
    val srcbs = MacroCC.gen[SourceBson]
    //val srcbs = implicitly[MacroCCMeta[SourceBson]]
    // val wr = srcbs.write(SourceBson("Name", "url"))
    //println(s"wr ${wr}")
  }

}
