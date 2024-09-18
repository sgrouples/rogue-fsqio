package me.sgrouples

import munit.FunSuite
import me.sgrouples.rogue.cc.macros.MacroBsonFormat
import me.sgrouples.rogue.cc.macros.MacroBsonFormat.*
import org.bson.types.ObjectId
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import testtags.*
import org.bson.BsonDocumentWriter

case class UX(id: UX.Id, vv: Int)
object UX:
  type Id = String @@ UX
  object Id:
    def apply(s: String): Id = tag[UX](s)

case class OX(id: OX.Id, se: Seq[String])
object OX:
  type Id = ObjectId @@ OX
  object Id:
    def apply(s: ObjectId): Id = tag[OX](s)

case class InnerC(i: Int, v: Option[String])
case class OuterC(i: InnerC, o: Seq[InnerC])

case class DerivesC(i: Int, v: Option[String], b: IndexedSeq[String])
    derives MacroBsonFormat

class MacroDerivationTests extends FunSuite:
  val oid = ObjectId()
  test("basic types") {
    roundTrip[Int](1)
    roundTrip[Long](2L)
    roundTrip[String]("ala")
    roundTrip[LocalDateTime](LocalDateTime.now.truncatedTo(ChronoUnit.MILLIS))
  }
  test("String & objectId subtypes") {
    roundTrip[UX.Id](UX.Id("uxid"))
    roundTrip[OX.Id](OX.Id(ObjectId()))
  }
  test("Options") {
    roundTrip[Option[Int]](Some(7))
  }
  test("Seq, List, Vector") {
    roundTrip[Seq[Int]](Seq(7, 2, 8))
    roundTrip[List[Int]](List(7, 2, 8))
    roundTrip[Vector[Int]](Vector(7, 2, 8))
  }
  test("Maps") {
    roundTrip[Map[String, Int]](Map("a" -> 7, "b" -> 2, "c" -> 8))
  }
  /*test("Enum") {

    }*/
  test("case class") {
    roundTrip[InnerC](InnerC(8, Some("hiyah")))
  }
  test("nested case class") {
    roundTrip[OuterC](
      OuterC(
        InnerC(8, Some("hiyah")),
        Seq(InnerC(1, Some("a")), InnerC(2, None))
      )
    )
  }

  test("case class with derives") {
    roundTrip[DerivesC](DerivesC(2, Some("b"), IndexedSeq("c")))
  }

  inline def roundTrip[T](v: T)(using t: MacroBsonFormat[T]) =
    val obj = t.write(v)
    /*println(s"write ${v}")
        println(s"wrote ${obj}")
        println(s"valid names ${t.validNames()}")*/
    val r = t.read(obj)
    assertEquals(v, r, s" for type ${v.getClass}")
