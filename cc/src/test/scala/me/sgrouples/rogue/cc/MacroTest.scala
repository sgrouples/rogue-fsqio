package me.sgrouples.rogue.cc

import org.scalatest.{ FlatSpec, Matchers }
import me.sgrouples.rogue.cc._
import me.sgrouples.rogue.cc.macros.MacroCC._
import me.sgrouples.rogue.cc.macros.MacroBsonFormat
import org.bson.{ BsonDocument, BsonDocumentWriter }
// import me.sgrouples.rogue.cc.macros.BlaDef._
import org.bson.types.ObjectId
import shapeless.tag._
import shapeless.tag
import me.sgrouples.rogue.map.MapKeyFormats._
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

trait CounterTag
object Counter {
  type Id = ObjectId @@ CounterTag
}

case class CounterCC(_id: Counter.Id, i: Int)

class MacroTest extends FlatSpec with Matchers {

  "Macro" should "bla cclass" in {

    val k = implicitly[MacroBsonFormat[EnumIntCC]]
    val bson = k.write(EnumIntCC(8, ClaimStatus2.pending))
    println(s"BSON ${bson}")
    val deser = k.read(bson)
    println(s"Deser ${deser}")

    val s2 = implicitly[MacroBsonFormat[SourceBson2]]
    val bs2V = SourceBson2(arr = Array(2, 3, 4), outerOid = new ObjectId(), mp = Map("ala" -> 7), s = Vector(3, 4, 6))
    val bson2 = s2.write(bs2V)
    println(s"BS2 = ${bson2}")

    val inV2 = s2.read(bson2)
    println(s"I: ${bs2V}")
    println(s"${bs2V.arr.toSeq}")
    println(s"O: ${inV2}")
    println(s"${inV2.arr.toSeq}")
    val counterF = implicitly[MacroBsonFormat[CounterCC]]
    val tagCounter = tag[CounterTag](new ObjectId())
    val counterC = CounterCC(tagCounter, 5)
    val counterBson = counterF.write(counterC)
    val counterR = counterF.read(counterBson)
    println(s"Counter bson ${counterBson}")
    counterC should ===(counterR)

    val doc = new BsonDocument()
    val wr = new BsonDocumentWriter(doc)
    s2.append(wr, bs2V)
    println(s"Wrote doc ${doc}")

    //inV2 should ===(bs2V)
    //won't because arrays :(

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
