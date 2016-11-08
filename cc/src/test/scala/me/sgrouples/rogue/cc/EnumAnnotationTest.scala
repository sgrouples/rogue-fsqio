package me.sgrouples.rogue.cc

import me.sgrouples.rogue._
import org.bson.types.ObjectId
import org.junit.Test
import org.specs2.matcher.JUnitMustMatchers
import BsonFormats._
import EnumAnnotatedFormats._
import org.bson.{ BsonInt32, BsonString }
object VenueStatus1 extends Enumeration {
  val open = Value("Open")
  val closed = Value("Closed")
}
@EnumSerializeValue object ClaimStatus2 extends Enumeration {
  val pending = Value("Pending approval")
  val approved = Value("Approved")
}
case class Statuses(enumName: VenueStatus1.Value, enumValue: ClaimStatus2.Value)

class EnumAnnotationTest extends JUnitMustMatchers {

  @Test
  def basicExtFormatTest(): Unit = {
    implicit val vsE = VenueStatus1
    implicit val ecsE = ClaimStatus2
    val f = BsonFormat[Statuses]
    val e = Statuses(VenueStatus1.open, ClaimStatus2.approved)
    val bson = f.write(e)
    bson.asDocument().getString("enumName") must_== new BsonString("Open")
    bson.asDocument().getInt32("enumValue") must_== new BsonInt32(1)
  }
  object EName extends Enumeration { val v1 = Value("V1"); val v2 = Value("V2") }
  @EnumSerializeValue object EValue extends Enumeration { val v1 = Value("bla1"); val v2 = Value("bla2") }
  case class C(e: EName.Value, v: EValue.Value)

}

