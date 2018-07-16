package me.sgrouples.rogue.cc

import org.scalatest.{ FlatSpec, Matchers }
import me.sgrouples.rogue.cc._
import me.sgrouples.rogue.cc.macros.MacroCC._
import me.sgrouples.rogue.BsonFormats._
import me.sgrouples.rogue.cc.macros.{ MacroGen }
import me.sgrouples.rogue.cc.macros.BlaDef._
case class InnSrc(innV: Int = 4)
case class SourceBson2(name: String = "Name", xx: Int = 8, inn: InnSrc = InnSrc(5))
class MacroTest extends FlatSpec with Matchers {

  "Macro" should "bla cclass" in {
    import me.sgrouples.rogue.cc.macros.BlaDef._
    //val blaString = implicitly[Bla[String]]
    //println(s"bla string is ${blaString}")

    //val g = implicitly[MacroGen[SourceBson2]]
    //println(g.namesMap())
    //   val sgen = implicitly[MacroGen[SourceBson]]
    class X extends MCcMetaExt[SourceBson2, X] {
      val xx = IntField
      val name = StringField

      //    val a = implicitly[MacroNamesResolver[SourceBson]]
      //val srcbs = MacroCC.gen[SourceBson]
      //val srcbs = implicitly[MacroCCMeta[SourceBson]]
      // val wr = srcbs.write(SourceBson("Name", "url"))
      //println(s"wr ${wr}")
    }

    assert(true, "OK")

  }
}
