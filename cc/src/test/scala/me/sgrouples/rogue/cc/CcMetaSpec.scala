package me.sgrouples.rogue.cc

import com.mongodb.util.JSON
import org.bson.{ BsonDocument, Document }
import org.bson.types.ObjectId
import org.scalatest.{ FlatSpec, MustMatchers }
import shapeless.tag._

import scala.util.Try

case class IdOneEnum(_id: ObjectId, one: String, en: VenueStatus.Value)

class CcMetaSpec extends FlatSpec with MustMatchers {

  import me.sgrouples.rogue.BsonFormats._

  "Meta R" should "work as expected" in {
    //reguired implicits for implicit call of BsonFormat[IdOneEnum] inside constructor of RCcMeta[IdOneEnum]
    implicit val ev = VenueStatus

    object IdOneEnumR extends RCcMeta[IdOneEnum]("idoneenum")
    val elem = IdOneEnum(new ObjectId(), "One", VenueStatus.closed)
    val bson = IdOneEnumR.write(elem)
    IdOneEnumR.read(bson) mustBe elem

    //println(classOf[IdOneEnumR.R])
    classOf[IdOneEnumR.R] mustBe classOf[IdOneEnum]
  }

  it should "find implicit format fo tagged ObjectId" in {

    trait Tag

    type TaggedObjectId = ObjectId @@ Tag

    case class A(id: TaggedObjectId)

    val _ = new RCcMeta[A]

    succeed
  }

  it should "treat id of ObjectId field as document _id" in {

    object CaseClassWithIdMeta extends RCcMeta[CaseClassWithId]

    trait Tag

    case class CaseClassWithId(id: ObjectId @@ Tag)

    val id = shapeless.tag[Tag][ObjectId](new ObjectId())

    val bson = CaseClassWithIdMeta.write(CaseClassWithId(id))

    bson.asDocument().getObjectId("_id").getValue mustBe id

    Try(bson.asDocument().getObjectId("id").getValue) mustBe 'failure

    CaseClassWithIdMeta.read {
      BsonDocument.parse(bson.asDocument().toJson)
    } mustBe CaseClassWithId(id)
  }
}
