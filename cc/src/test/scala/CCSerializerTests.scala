package me.sgrouples.rogue
import org.bson.types.ObjectId
import org.junit.Test
import BsonFormats._
import shapeless.LabelledGeneric

import scala.language.implicitConversions
import scala.language.higherKinds

case class OidTypedCC(_id: ObjectId, name: String, value: Int)

case class A(a:Int)
class CCSerializerTests {

  @Test
  def basicSerializeTest() = {

    /*def ser[T]:BsonFormat[T] = {
def jsonWriter[T](implicit writer: JsonWriter[T]) = writer

    }*/

    val o = new ObjectId()
    val v = OidTypedCC(o, "Ala", 10)

    //val gen = LabelledGeneric[A]
    //val fmt = hListFormat(t, key, headStr, remFmt)

    val f = implicitly[BsonFormat[OidTypedCC]]
    //val f = DefaultJsonEncoder[OidTypedCC]
    //val f  = BsonEncoder[OidTypedCC]
    val bson = f.write(v)
    //val intF = implicitly[BsonFormat[Int]]

    //val sF = implicitly[BsonFormat[A]]
    //val bson = sF.write(A(10))
    //intF.write(10)
    println(bson)


  }

}
