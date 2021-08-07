package me.sgrouples.rogue.cc

import org.bson.types.ObjectId
import munit.FunSuite
import shapeless.tag._

case class IdOneEnum(_id: ObjectId, one: String, en: VenueStatus.Value)

class CcMetaSpec extends FunSuite {

  test("Meta R should work as expected") {
    //reguired implicits for implicit call of BsonFormat[IdOneEnum] inside constructor of RCcMeta[IdOneEnum]
    implicit val ev = VenueStatus
    import me.sgrouples.rogue.BsonFormats._
    import me.sgrouples.rogue.EnumNameFormats._
    object IdOneEnumR extends RCcMeta[IdOneEnum]("idoneenum")
    val elem = IdOneEnum(new ObjectId(), "One", VenueStatus.closed)
    val bson = IdOneEnumR.write(elem)
    assertEquals(IdOneEnumR.read(bson), elem)

    //println(classOf[IdOneEnumR.R])
    assertEquals(classOf[IdOneEnumR.R], classOf[IdOneEnum])
  }

  test("it should find implicit format fo tagged ObjectId") {

    import me.sgrouples.rogue.BsonFormats._

    trait Tag

    type TaggedObjectId = ObjectId @@ Tag

    case class A(
      id: TaggedObjectId,
      name: String @@ Tag,
      age: Long @@ Tag)

    val _ = new RCcMeta[A]

    assert(true,"compiled, so all is good fine")
  }

}
