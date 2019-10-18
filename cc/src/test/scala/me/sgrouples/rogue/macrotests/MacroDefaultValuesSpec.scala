package me.sgrouples.rogue.macrotests

import me.sgrouples.rogue.cc.macros._
import org.bson.BsonDocument
import org.scalatest.{ FlatSpec, Matchers }

case class InnerModel(s: String)
case class OuterModel(i: Int = 1, inner: InnerModel = InnerModel("ala"))
trait OuterMetas {
  class InnerMeta extends MCcMeta[InnerModel, InnerMeta]("i")
  implicit val InnerM = new InnerMeta
  class OuterMeta extends MCcMeta[OuterModel, OuterMeta]("x")
}

class MacroDefaultValuesSpec extends FlatSpec with Matchers with OuterMetas {
  "default values for case classes" should "be respected" in {
    println(5)
    val M = new OuterMeta
    M.read(new BsonDocument()) should ===(OuterModel(1, InnerModel("ala")))
  }
}
