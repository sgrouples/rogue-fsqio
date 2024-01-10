package me.sgrouples.rogue.cc

import org.bson.types.ObjectId
import munit.FunSuite
import me.sgrouples.rogue.cc.macros.*
import com.softwaremill.tagging.*

case class IdOneEnum(_id: ObjectId, one: String, en: VenueStatus.Value) derives MacroBsonFormat

class CcMetaSpec extends FunSuite {

  test("Meta R should work as expected") {
    object IdOneEnumR extends MCcMeta[IdOneEnum, IdOneEnumR.type]("idoneenum")
    val elem = IdOneEnum(new ObjectId(), "One", VenueStatus.closed)
    val bson = IdOneEnumR.write(elem)
    assertEquals(IdOneEnumR.read(bson), elem)

    //println(classOf[IdOneEnumR.R])
    assertEquals(classOf[IdOneEnumR.R], classOf[IdOneEnum])
  }

  test("it should find implicit format fo tagged ObjectId") {

    import me.sgrouples.rogue.cc.macros.*

    trait Tag

    type TaggedObjectId = ObjectId @@ Tag

    case class A(id: TaggedObjectId, name: String @@ Tag, age: Long @@ Tag)

    //val _ = new MCcMeta[A]

    assert(true, "compiled, so all is good fine")
  }

}
