package me.sgrouples.rogue.cc

import org.scalatest.{ FlatSpec, Matchers }
import me.sgrouples.rogue.cc._
import me.sgrouples.rogue.cc.macros.MacroCC._
import me.sgrouples.rogue.BsonFormats._
import me.sgrouples.rogue.cc.macros.{ MacroBsonFormat }
// import me.sgrouples.rogue.cc.macros.BlaDef._
import org.bson.types.ObjectId
object Eni extends Enumeration {

}
case class InnSrc(oid: ObjectId, innV: Int = 4)
case class SourceBson2(name: String = "Name", xx: Int = 8,
  inn: InnSrc = InnSrc(new ObjectId(), 5), opt: Option[Int] = None,
  arr: Array[Int],
  outerOid: ObjectId,
  mp: Map[String, Int],
  s: Vector[Int],
  rr: ClaimStatus2.Value = ClaimStatus2.approved)

case class EnumIntCC(int: Int = 0, enum: ClaimStatus2.Value = ClaimStatus2.approved)
case class JustInt(just: Int = 7, other: Int = 0)
class MacroTest extends FlatSpec with Matchers {

  "Macro" should "bla cclass" in {

    val k = implicitly[MacroBsonFormat[JustInt]]
    val bson = k.write(JustInt(8, 4))
    println(s"BSON ${bson}")
    val deser = k.read(bson)
    println(s"Deser ${deser}")

    //val m = implicitly[MacroGen[RejectReason.Value]]
    //val blaString = implicitly[Bla[String]]
    //println(s"bla string is ${blaString}")

    //val g = implicitly[MacroGen[SourceBson2]]
    //println(g.namesMap())
    //   val sgen = implicitly[MacroGen[SourceBson]]
    //class X extends MCcMetaExt[SourceBson2, X] {
    // val xx = IntField
    //val name = StringField

    //    val a = implicitly[MacroNamesResolver[SourceBson]]
    //val srcbs = MacroCC.gen[SourceBson]
    //val srcbs = implicitly[MacroCCMeta[SourceBson]]
    // val wr = srcbs.write(SourceBson("Name", "url"))
    //println(s"wr ${wr}")
    //}

    assert(true, "OK")

  }
}
