package me.sgrouples.rogue.cc

import org.scalatest.{ FlatSpec, Matchers }
import me.sgrouples.rogue.cc._
import me.sgrouples.rogue.cc.macros.MacroCC._
import me.sgrouples.rogue.BsonFormats._
import me.sgrouples.rogue.cc.macros.MacroGen

class MacroTest extends FlatSpec with Matchers {

  "Macro" should "bla cclass" in {
    //   val sgen = implicitly[MacroGen[SourceBson]]
    class X extends MCcMetaExt[SourceBson, X] {
      val url = StringField
      val name = StringField

      println("BLA")
      println("OK")

      //    val a = implicitly[MacroNamesResolver[SourceBson]]
      //val srcbs = MacroCC.gen[SourceBson]
      //val srcbs = implicitly[MacroCCMeta[SourceBson]]
      // val wr = srcbs.write(SourceBson("Name", "url"))
      //println(s"wr ${wr}")
    }

    assert(true, "OK")

  }
}
