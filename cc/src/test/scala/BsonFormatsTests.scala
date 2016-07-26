package me.sgrouples.rogue
import org.bson.types.ObjectId
import org.junit.Test
import BsonFormats._
import me.sgrouples.rogue.cc.{ ClaimStatus, VenueStatus }
import org.specs2.matcher.JUnitMustMatchers

import scala.language.implicitConversions
import scala.language.higherKinds

case class OidTypedCC(_id: ObjectId, name: String, value: Int)

case class OptCC(_id: Long, name: Option[String], hashes: List[String], map: Map[String, Int])

case class RootC(id: Int, nest: Nest)
case class Nest(name: String)
case class OneEnum(one: String, en: VenueStatus.Value)
case class TwoEnums(one: String, en: VenueStatus.Value, x: ClaimStatus.Value)

class BsonFormatsTests extends JUnitMustMatchers {

  @Test
  def basicSerializeTest(): Unit = {
    val o = new ObjectId()
    val cc = OidTypedCC(o, "Ala", 10)
    val f = BsonFormat[OidTypedCC]
    val bson = f.write(cc)
    //println(bson)
    val deserialized = f.read(bson)
    //println(s"DES s${deserialized}")
    cc must_== deserialized
  }

  @Test
  def optionalSerializeTest(): Unit = {
    val opt = OptCC(1L, Some("opt"), List("one1", "two", "three"), Map("four" -> 4, "five" -> 5))
    val f = BsonFormat[OptCC]
    val bson = f.write(opt)
    //println(s"bson ${bson}")
    val d = f.read(bson)
    opt must_== d
  }

  @Test
  def nestedCCTest(): Unit = {
    val r = RootC(1, Nest("nest"))
    val f = BsonFormat[RootC]
    val bson = f.write(r)
    //  println(s"Bson root ${bson}")
    f.read(bson) must_== r
  }

  @Test
  def enumerationTest: Unit = {
    implicit val ev = VenueStatus
    val r = OneEnum("a", VenueStatus.open)
    val f = BsonFormat[OneEnum]
    val bson = f.write(r)
    //  println(s"Bson root ${bson}")
    f.read(bson) must_== r
  }

  @Test
  def twoEnumTest: Unit = {
    implicit val ev = VenueStatus
    implicit val ev2 = ClaimStatus
    val r = TwoEnums("a", VenueStatus.open, ClaimStatus.approved)
    val f = BsonFormat[TwoEnums]
    val bson = f.write(r)
    //println(s"Bson root ${bson}")
    f.read(bson) must_== r
  }

}
