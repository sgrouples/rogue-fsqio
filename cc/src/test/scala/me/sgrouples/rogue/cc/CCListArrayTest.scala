package me.sgrouples.rogue.cc

import me.sgrouples.rogue._
import me.sgrouples.rogue.cc.CcRogue._
import org.bson.types.ObjectId
import org.scalatest.{ FlatSpec, MustMatchers }
import me.sgrouples.rogue.BsonFormats._

case class Inner(a: Int, b: String, c: Array[String] = Array.empty)

case class Outer(
  _id: ObjectId,
  innerList: List[Inner],
  innerArray: Array[Inner],
  innerOptArray: Option[Array[Inner]],
  innerOptList: Option[List[Inner]]
)
object InnerR extends RCcMeta[Inner]("") {
  val a = new IntField("a", this)
  val b = new StringField("b", this)
  val c = new ArrayField[String, InnerR.type]("c", this)
  val d = new ListField[String, InnerR.type]("d", this)
}

object OuterR extends RCcMeta[Outer] {
  val id = new ObjectIdField("_id", this)
  val innerList = new CClassListField[Inner, InnerR.type, OuterR.type]("innerList", InnerR, this)
  val innerArray = new CClassArrayField[Inner, InnerR.type, OuterR.type]("innerArray", InnerR, this)
  val innerOptArray = new OptCClassArrayField[Inner, InnerR.type, OuterR.type]("innerOptArray", InnerR, this)
  val innerOptList = new OptCClassListField[Inner, InnerR.type, OuterR.type]("innerOptList", InnerR, this)
}

class CCListArrayTest extends FlatSpec with MustMatchers {
  "CCListField" must "produce queries" in {
    val ql = OuterR.where(_.innerList.subfield(_.b) eqs "a")
    ql.toString must ===("""db.outer.find({ "innerList.b" : "a"})""")

    val qa = OuterR.where(_.innerArray.subfield(_.b) eqs "a")
    qa.toString must ===("""db.outer.find({ "innerArray.b" : "a"})""")

    val qal = OuterR.where(_.innerList.subfield(_.a) eqs 1).modify(_.innerList.$.subfield(_.b) setTo "blah")
    qal.toString must ===("""db.outer.update({ "innerList.a" : 1}, { "$set" : { "innerList.$.b" : "blah"}}, false, false)""")

    val qua = OuterR.where(_.innerArray.subfield(_.a) eqs 1).modify(_.innerArray.$.subfield(_.b) setTo "blah")
    qua.toString must ===("""db.outer.update({ "innerArray.a" : 1}, { "$set" : { "innerArray.$.b" : "blah"}}, false, false)""")

    val upl = OuterR.where(_.innerList.subfield(_.a) eqs 1).modify(_.innerList.$.subfield(_.c) setTo List("blah", "bah"))
    upl.toString must ===("""db.outer.update({ "innerList.a" : 1}, { "$set" : { "innerList.$.c" : [ "blah" , "bah"]}}, false, false)""")

    val upa = OuterR.where(_.innerArray.subfield(_.a) eqs 1).modify(_.innerArray.$.subfield(_.d) setTo List("blah", "bah"))
    upa.toString must ===("""db.outer.update({ "innerArray.a" : 1}, { "$set" : { "innerArray.$.d" : [ "blah" , "bah"]}}, false, false)""")

    val qoa = OuterR.where(_.innerOptArray.subfield(_.b) eqs "a")
    qoa.toString must ===("""db.outer.find({ "innerOptArray.b" : "a"})""")

    val qol = OuterR.where(_.innerOptList.subfield(_.a) eqs 1).modify(_.innerOptList.$.subfield(_.b) setTo "blah")
    qol.toString must ===("""db.outer.update({ "innerOptList.a" : 1}, { "$set" : { "innerOptList.$.b" : "blah"}}, false, false)""")

    val uoa = OuterR.where(_.innerOptArray.subfield(_.a) eqs 1).modify(_.innerOptArray.$.subfield(_.d) setTo List("blah", "bah"))
    uoa.toString must ===("""db.outer.update({ "innerOptArray.a" : 1}, { "$set" : { "innerOptArray.$.d" : [ "blah" , "bah"]}}, false, false)""")

  }

}
