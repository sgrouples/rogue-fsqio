package me.sgrouples.rogue.cc

import munit.FunSuite
import me.sgrouples.rogue.cc.macros.*
import me.sgrouples.rogue.map.MapKeyFormat
import org.bson.types.ObjectId
import com.softwaremill.tagging.*
import me.sgrouples.rogue.cc.CcRogue.*

case class CustomKey(value: Long) extends AnyVal
case class StringMap(value: Map[String, Long])
case class ObjectIdMap(value: Map[ObjectId, Long])
class StringMapMeta extends MCcMeta[StringMap, StringMapMeta]

object MTypes {
  trait M
  type ObjectIdSubtype = ObjectId @@ M
}
case class ObjectIdSubtypeMap(value: Map[MTypes.ObjectIdSubtype, Long])
case class CustomKeyMap(value: Map[CustomKey, Long])

class MapFormatSpec extends FunSuite {

  test("MapFormat should write/read string keyed map") {

    val meta = new StringMapMeta
    val v = StringMap(Map("Hi" -> 1))
    val bson = meta.write(v)
    assertEquals(meta.read(bson), v)

  }

  class ObjectIdMapMeta extends MCcMeta[ObjectIdMap, ObjectIdMapMeta]

  test("it should write/read objectId keyed map") {

    val meta = new ObjectIdMapMeta
    val v = ObjectIdMap(Map(ObjectId.get() -> 1))
    val bson = meta.write(v)
    assertEquals(meta.read(bson), v)
  }

  class ObjectIdSubtypeMapMeta
      extends MCcMeta[ObjectIdSubtypeMap, ObjectIdSubtypeMapMeta]

  test("it should write/read objectId subtype keyed map") {

    val meta = new ObjectIdSubtypeMapMeta
    val v = ObjectIdSubtypeMap(Map(ObjectId.get().taggedWith[MTypes.M] -> 1))
    val bson = meta.write(v)
    assertEquals(meta.read(bson), v)
  }

  implicit val customKeyMapFormat: MapKeyFormat[CustomKey] =
    MapKeyFormat[CustomKey](s => CustomKey(s.toLong), _.value.toString)

  class CustomKeyMapMeta extends MCcMeta[CustomKeyMap, CustomKeyMapMeta]

  test("it should write/read custom keyed map") {

    val meta = new CustomKeyMapMeta
    val v = CustomKeyMap(Map(CustomKey(1L) -> 1L))
    val bson = meta.write(v)
    assertEquals(meta.read(bson), v)
  }
}
