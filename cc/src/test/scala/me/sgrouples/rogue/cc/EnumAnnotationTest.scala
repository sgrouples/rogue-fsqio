package me.sgrouples.rogue.cc

import me.sgrouples.rogue._
import org.bson.{BsonInt32, BsonString}
import munit.FunSuite
object VenueStatus1 extends Enumeration {
  val open = Value("Open")
  val closed = Value("Closed")
}

@EnumSerializeValue object ClaimStatus2 extends Enumeration {
  val pending = Value("Pending approval")
  val approved = Value("Approved")
}

case class Statuses(enumName: VenueStatus1.Value, enumValue: ClaimStatus2.Value)

class EnumAnnotationTest extends FunSuite {

  object EName extends Enumeration {
    val v1 = Value("V1"); val v2 = Value("V2")
  }
  @EnumSerializeValue object EValue extends Enumeration {
    val v1 = Value("bla1"); val v2 = Value("bla2")
  }
  case class C(e: EName.Value, v: EValue.Value)

  test("EnumAnnotatedFormats".ignore) {
    assert(false,"Fixme - annotations requires quoted macros")
    //import BsonFormats._
    //import EnumAnnotatedFormats._

    /*val f = LazyBsonFormat[Statuses]
    val e = Statuses(VenueStatus1.open, ClaimStatus2.approved)
    val bson = f.write(e)
    assertEquals(
      bson.asDocument().getString("enumName"),
      new BsonString("Open")
    )
    assertEquals(bson.asDocument().getInt32("enumValue"), new BsonInt32(1))
    */
  }
}
