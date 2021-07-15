package me.sgrouples.rogue
import org.bson.types.ObjectId
import org.junit.Test
import org.specs2.matcher.JUnitMustMatchers

//import BsonExtFormats._

class BsonExtFormatTests extends JUnitMustMatchers {

  @Test
  def basicExtFormatTest(): Unit = {
    val o = new ObjectId()
    val cc = OidTypedCC(o, "Ala", 10)
    val f = CcFields[OidTypedCC]
    //println(f.flds("_id"))
    //f.flds.
    //println(s"FLDS ${f.flds} ${f.flds.getClass}")

    ()
  }

}

