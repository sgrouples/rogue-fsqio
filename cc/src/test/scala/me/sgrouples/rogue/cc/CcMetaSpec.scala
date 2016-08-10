package me.sgrouples.rogue.cc

import org.bson.types.ObjectId
import org.scalatest.{ FlatSpec, MustMatchers }
import shapeless.tag._

case class IdOneEnum(_id: ObjectId, one: String, en: VenueStatus.Value)

class CcMetaSpec extends FlatSpec with MustMatchers {

  "Meta R" should "work as expected" in {
    //reguired implicits for implicit call of BsonFormat[IdOneEnum] inside constructor of RCcMeta[IdOneEnum]
    implicit val ev = VenueStatus
    import me.sgrouples.rogue.BsonFormats._

    object IdOneEnumR extends RCcMeta[IdOneEnum]("idoneenum")
    val elem = IdOneEnum(new ObjectId(), "One", VenueStatus.closed)
    val bson = IdOneEnumR.write(elem)
    IdOneEnumR.read(bson) mustBe elem

    //println(classOf[IdOneEnumR.R])
    classOf[IdOneEnumR.R] mustBe classOf[IdOneEnum]
  }

  it should "find implicit format fo tagged ObjectId" in {

    import me.sgrouples.rogue.BsonFormats._

    trait Tag

    type TaggedObjectId = ObjectId @@ Tag

    case class A(id: TaggedObjectId)

    val _ = new RCcMeta[A]

    succeed
  }
}
