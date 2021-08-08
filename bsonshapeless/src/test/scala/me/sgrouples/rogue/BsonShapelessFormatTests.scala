package me.sgrouples.rogue
import me.sgrouples.rogue.BsonFormats._
import me.sgrouples.rogue.EnumNameFormats._
import org.bson.BsonDocument
import org.bson.types.ObjectId
import munit.FunSuite
import java.util.{Locale, TimeZone}
import scala.language.{higherKinds, implicitConversions}

object VenueStatus extends Enumeration {
  val open = Value("Open")
  val closed = Value("Closed")
}
object ClaimStatus extends Enumeration {
  val pending = Value("Pending approval")
  val approved = Value("Approved")
}
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

case class W(a: Int = 1)
case class B(w: W = W(1), x: String = "a")

class BsonShapelessFormatTests extends FunSuite {

  test("basicSerializeTest") {
    val o = new ObjectId()
    val cc = OidTypedCC(o, "Ala", 10)
    val f = LazyBsonFormat[OidTypedCC]
    val bson = f.write(cc)
    //println(bson)
    val deserialized = f.read(bson)
    //println(s"DES s${deserialized}")
    assertEquals(cc, deserialized)
  }

  test("optionalSerializeTest") {
    val opt = OptCC(
      1L,
      Some("opt"),
      List("one1", "two", "three"),
      Map("four" -> 4, "five" -> 5)
    )
    val f = LazyBsonFormat[OptCC]
    val bson = f.write(opt)
    //println(s"bson ${bson}")
    val d = f.read(bson)
    assertEquals(opt, d)
  }

  test("nestedCCTest") {
    val r = RootC(1, Nest("nest"))
    val f = LazyBsonFormat[RootC]
    val bson = f.write(r)
    //  println(s"Bson root ${bson}")
    assertEquals(f.read(bson), r)
  }

  test("enumerationValueTest") {
    implicit val ev = VenueStatus
    val r = OneEnum("a", VenueStatus.open)
    val f = LazyBsonFormat[OneEnum]
    val bson = f.write(r)
    //  println(s"Bson root ${bson}")
    assertEquals(f.read(bson), r)
  }

  test("twoEnumTest") {
    implicit val ev = VenueStatus
    implicit val ev2 = ClaimStatus
    val r = TwoEnums("a", VenueStatus.open, ClaimStatus.approved)
    val f = LazyBsonFormat[TwoEnums]
    val bson = f.write(r)
    //println(s"Bson root ${bson}")
    assertEquals(f.read(bson), r)
  }

  test("defaultCaseClass") {
    val f = LazyBsonFormat[B]
    val b = f.read(new BsonDocument)
    //println(s"B ${b}")
    //really want that
    //but don't know how to provide default values in case class field parameters
    assertEquals(b, B(W(1), ""))
    //assert(true)
  }

  test("localeTest") {
    val f = LazyBsonFormat[Locale]

    val locales = Locale.getAvailableLocales

    locales.foreach { locale =>
      val serialized = f.write(locale)
      val deserialized = f.read(serialized)
      if (locale.toString.isEmpty) {
        assertEquals(deserialized, LocaleBsonFormat.defaultValue)
      } else {
        assertEquals(deserialized, locale)
      }
    }
  }

  test("timeZoneTest") {
    val f = LazyBsonFormat[TimeZone]
    val timezones = TimeZone.getAvailableIDs

    timezones.foreach { tzId =>
      val timeZone = TimeZone.getTimeZone(tzId)
      val serialized = f.write(timeZone)
      val deserialized = f.read(serialized)

      assertEquals(deserialized, timeZone)
    }
  }

}
