package me.sgrouples.rogue.cc

import me.sgrouples.rogue._
import org.bson.{ BsonInt32, BsonString }
import org.scalatest.{ FlatSpec, MustMatchers }

object VenueStatus1 extends Enumeration {
  val open = Value("Open")
  val closed = Value("Closed")
}

@EnumSerializeValue object ClaimStatus2 extends Enumeration {
  val pending = Value("Pending approval")
  val approved = Value("Approved")
}

case class Statuses(enumName: VenueStatus1.Value, enumValue: ClaimStatus2.Value)

class EnumAnnotationTest extends FlatSpec with MustMatchers {

  object EName extends Enumeration { val v1 = Value("V1"); val v2 = Value("V2") }
  @EnumSerializeValue object EValue extends Enumeration { val v1 = Value("bla1"); val v2 = Value("bla2") }
  case class C(e: EName.Value, v: EValue.Value)

  "EnumAnnotatedFormats" should "work as expected" in {

    import BsonFormats._
    import EnumAnnotatedFormats._

    val f = BsonFormat[Statuses]
    val e = Statuses(VenueStatus1.open, ClaimStatus2.approved)
    val bson = f.write(e)
    bson.asDocument().getString("enumName") mustBe new BsonString("Open")
    bson.asDocument().getInt32("enumValue") mustBe new BsonInt32(1)
  }
}

