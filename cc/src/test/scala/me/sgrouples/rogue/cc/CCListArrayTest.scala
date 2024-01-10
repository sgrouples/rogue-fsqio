package me.sgrouples.rogue.cc

import me.sgrouples.rogue.*
import me.sgrouples.rogue.cc.CcRogue.{given, *}
import me.sgrouples.rogue.cc.macros.*
import org.bson.types.ObjectId
import munit.FunSuite
import me.sgrouples.rogue.QueryParser.*

case class Inner(a: Int, b: String, c: Array[String] = Array.empty) derives MacroBsonFormat

case class Outer(
    _id: ObjectId,
    innerList: List[Inner],
    innerArray: Array[Inner],
    innerOptArray: Option[Array[Inner]],
    innerOptList: Option[List[Inner]]
) derives MacroBsonFormat
object InnerR extends MCcMeta[Inner, InnerR.type]("") {
  val a = new IntField("a", this)
  val b = new StringField("b", this)
  val c = new ArrayField[String, InnerR.type]("c", this)
  val d = new ListField[String, InnerR.type]("d", this)
}

object OuterR extends MCcMeta[Outer, OuterR.type] {
  val id = new ObjectIdField("_id", this)
  val innerList = ClassListField("innerList", InnerR)
  val innerArray = ClassArrayField("innerArray", InnerR)
  val innerOptArray = OptClassArrayField("innerOptArray", InnerR)
  val innerOptList = OptClassListField("innerOptList", InnerR)
}

class CCListArrayTest extends FunSuite {
  test("CCListField must produce queries") {
    val ql = OuterR.where(_.innerList.subfield(_.b) eqs "a")
    assertEquals(ql.q, pq("""db.outer.find({"innerList.b": "a"})"""))

    val qa = OuterR.where(_.innerArray.subfield(_.b) eqs "a")
    assertEquals(qa.q, pq(("""db.outer.find({"innerArray.b": "a"})""")))

    val qal = OuterR
      .where(_.innerList.subfield(_.a) eqs 1)
      .modify(_.innerList.$.subfield(_.b) setTo "blah")
    assertEquals(
      qal.q,
      pq(
        ("""db.outer.update({"innerList.a": 1}, {"$set": {"innerList.$.b": "blah"}}, false, false)""")
      )
    )

    val qua = OuterR
      .where(_.innerArray.subfield(_.a) eqs 1)
      .modify(_.innerArray.$.subfield(_.b) setTo "blah")
    assertEquals(
      qua.q,
      pq(
        ("""db.outer.update({"innerArray.a": 1}, {"$set": {"innerArray.$.b": "blah"}}, false, false)""")
      )
    )

    val upl = OuterR
      .where(_.innerList.subfield(_.a) eqs 1)
      .modify(_.innerList.$.subfield(_.c) setTo List("blah", "bah"))
    assertEquals(
      upl.q,
      pq(
        ("""db.outer.update({"innerList.a": 1}, {"$set": {"innerList.$.c": ["blah", "bah"]}}, false, false)""")
      )
    )

    val upa = OuterR
      .where(_.innerArray.subfield(_.a) eqs 1)
      .modify(_.innerArray.$.subfield(_.d) setTo List("blah", "bah"))
    assertEquals(
      upa.q,
      pq(
        ("""db.outer.update({"innerArray.a": 1}, {"$set": {"innerArray.$.d": ["blah", "bah"]}}, false, false)""")
      )
    )

    val qoa = OuterR.where(_.innerOptArray.subfield(_.b) eqs "a")
    assertEquals(qoa.q, pq(("""db.outer.find({"innerOptArray.b": "a"})""")))

    val qol = OuterR
      .where(_.innerOptList.subfield(_.a) eqs 1)
      .modify(_.innerOptList.$.subfield(_.b) setTo "blah")
    assertEquals(
      qol.q,
      pq(
        ("""db.outer.update({"innerOptList.a": 1}, {"$set": {"innerOptList.$.b": "blah"}}, false, false)""")
      )
    )

    val uoa = OuterR
      .where(_.innerOptArray.subfield(_.a) eqs 1)
      .modify(_.innerOptArray.$.subfield(_.d) setTo List("blah", "bah"))
    assertEquals(
      uoa.q,
      pq(
        ("""db.outer.update({"innerOptArray.a": 1}, {"$set": {"innerOptArray.$.d": ["blah", "bah"]}}, false, false)""")
      )
    )

  }

}
