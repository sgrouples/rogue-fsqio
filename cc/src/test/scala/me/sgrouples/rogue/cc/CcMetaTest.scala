package me.sgrouples.rogue.cc

import org.bson.types.ObjectId
import org.junit.Test
import org.specs2.matcher.JUnitMustMatchers

case class IdOneEnum(_id:ObjectId, one: String, en : VenueStatus.Value)

class CcMetaTest extends JUnitMustMatchers {

  @Test
  def testMetaR():Unit = {
    //reguired implicits for implicit call of BsonFormat[IdOneEnum] inside constructor of RCcMeta[IdOneEnum]
    implicit val ev = VenueStatus
    import me.sgrouples.rogue.BsonFormats._

    object IdOneEnumR extends RCcMeta[IdOneEnum]("idoneenum")
    val elem = IdOneEnum(new ObjectId(), "One", VenueStatus.closed)
    val bson = IdOneEnumR.write(elem)
    IdOneEnumR.read(bson) must_== elem


  }
}
