package me.sgrouples.rogue
import me.sgrouples.rogue.cc.macros.*
import me.sgrouples.rogue.cc.*
import org.bson.BsonDocument
import org.bson.types.ObjectId
import munit.FunSuite
import java.util.{Locale, TimeZone}
import scala.language.implicitConversions

case class OidTypedCC(_id: ObjectId, name: String, value: Int)

case class OptCC(
    _id: Long,
    name: Option[String],
    hashes: List[String],
    map: Map[String, Int]
)

case class RootC(id: Int, nest: Nest)
case class Nest(name: String)
case class OneEnum(one: String, en: VenueStatus.Value)
case class TwoEnums(one: String, en: VenueStatus.Value, x: ClaimStatus.Value)

class BsonFormatsTests extends FunSuite {
  test("basicSerializeTest") {
    val o = new ObjectId()
    val cc = OidTypedCC(o, "Ala", 10)
    val f = summon[MacroBsonFormat[OidTypedCC]]
    val bson = f.write(cc)
    val deserialized = f.read(bson)
    assertEquals(cc, deserialized)
  }

  test("optionalSerializeTest") {
    val opt = OptCC(
      1L,
      Some("opt"),
      List("one1", "two", "three"),
      Map("four" -> 4, "five" -> 5)
    )
    val f = summon[MacroBsonFormat[OptCC]]
    val bson = f.write(opt)
    //println(s"bson ${bson}")
    val d = f.read(bson)
    assertEquals(opt, d)
  }

  test("nestedCCTest") {
    val r = RootC(1, Nest("nest"))
    val f = summon[MacroBsonFormat[RootC]]
    val bson = f.write(r)
    assertEquals(f.read(bson), r)
  }

  test("enumerationValueTest") {
    implicit val ev = VenueStatus
    val r = OneEnum("a", VenueStatus.open)
    val f = summon[MacroBsonFormat[OneEnum]]
    val bson = f.write(r)
    //  println(s"Bson root ${bson}")
    assertEquals(f.read(bson), r)
  }

  test("twoEnumTest") {
    implicit val ev = VenueStatus
    implicit val ev2 = ClaimStatus
    val r = TwoEnums("a", VenueStatus.open, ClaimStatus.approved)
    val f = summon[MacroBsonFormat[TwoEnums]]
    val bson = f.write(r)
    //println(s"Bson root ${bson}")
    assertEquals(f.read(bson), r)
  }

  test("defaultCaseClass") {
    case class W(a: Int = 1)
    case class B(w: W = W(1), x: String = "a")
    val f = summon[MacroBsonFormat[B]]
    val b = f.read(new BsonDocument)
    //println(s"B ${b}")
    //really want that
    //b must_== B()
    //but don't know how to provide default values in case class field parameters
    //assertEquals(b, B(W(1), ""))
    //assert(true)
  }

  test("localeTest") {
    val f = summon[MacroBsonFormat[Locale]]

    val locales = Locale.getAvailableLocales

    locales.foreach { locale =>
      val serialized = f.write(locale)
      val deserialized = f.read(serialized)
      if (locale.toString.isEmpty) {
        assertEquals(deserialized, f.defaultValue)
      } else {
        assertEquals(deserialized, locale)
      }
    }
  }

  test("timeZoneTest") {
    val f = summon[MacroBsonFormat[TimeZone]]
    val timezones = TimeZone.getAvailableIDs
    timezones.foreach { tzId =>
      val timeZone = TimeZone.getTimeZone(tzId)
      val serialized = f.write(timeZone)
      val deserialized = f.read(serialized)
      assertEquals(deserialized, timeZone)
    }
  }
}
