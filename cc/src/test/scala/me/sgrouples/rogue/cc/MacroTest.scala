package me.sgrouples.rogue.cc

import org.scalatest.{ FlatSpec, Matchers }

import me.sgrouples.rogue.cc._
import MacroCC._

class MacroTest extends FlatSpec with Matchers {

  "Macro" should "bla cclass" in {
    //MacroCC.gen[SourceBson]
    MacroCC.gen[SourceBson]
    class X extends MacroNamesResolver[SourceBson] with QueryFieldHelpers[X] {
      //val url = StringField
      //val name = StringField
      def myNames = names
    }

    val x = new X
    println("BLA")
    println(x.myNames)
    println("OK")

    assert(true, "OK")
    //    val a = implicitly[MacroNamesResolver[SourceBson]]
    //val srcbs = MacroCC.gen[SourceBson]
    //val srcbs = implicitly[MacroCCMeta[SourceBson]]
    // val wr = srcbs.write(SourceBson("Name", "url"))
    //println(s"wr ${wr}")
  }

}
