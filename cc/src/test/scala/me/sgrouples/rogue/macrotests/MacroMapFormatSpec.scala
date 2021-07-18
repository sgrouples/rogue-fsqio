package me.sgrouples.rogue.macrotests

import me.sgrouples.rogue.cc.CustomKey
import me.sgrouples.rogue.cc.macros._
import me.sgrouples.rogue.macrotests.Domain.StrLongMapT
import org.bson.types.ObjectId
import shapeless.tag
import shapeless.tag.@@
import munit.FunSuite
object Domain {
  type StrLongMapT = Map[String, Long]
}
class MacroMapFormatSpec extends FunSuite {

  case class StringMap(value: Map[String, Long])
  case class ObjectIdMap(value: Map[ObjectId, Long])
  case class OptMapStrLong(expanded: Option[Map[String, Long]], aliased: Option[StrLongMapT])
  case class MapStrLong(expanded: Map[String, Long], aliased: StrLongMapT)

  trait M

  type ObjectIdSubtype = ObjectId @@ M

  case class ObjectIdSubtypeMap(value: Map[ObjectIdSubtype, Long])
  case class CustomKeyMap(value: Map[CustomKey, Long])

  class StringMapMeta extends MCcMeta[StringMap, StringMapMeta]("smm")

  test("MapFormat should write/read string keyed map") {
    val meta = new StringMapMeta
    val v = StringMap(Map("Hi" -> 1))
    val bson = meta.write(v)
    assertEquals(meta.read(bson), v)
  }

  class ObjectIdMapMeta extends MCcMeta[ObjectIdMap, ObjectIdMapMeta]("oidm")

  test("write/read objectId keyed map") {
    val meta = new ObjectIdMapMeta
    val v = ObjectIdMap(Map(ObjectId.get() -> 1))
    val bson = meta.write(v)
    assertEquals(meta.read(bson),v)
  }

  class ObjectIdSubtypeMapMeta extends MCcMeta[ObjectIdSubtypeMap, ObjectIdSubtypeMapMeta]("oidsub")

  test("write/read objectId subtype keyed map"){

    val meta = new ObjectIdSubtypeMapMeta
    val v = ObjectIdSubtypeMap(Map(tag[M](ObjectId.get()) -> 1))
    val bson = meta.write(v)
    assertEquals(meta.read(bson), v)
  }

  //TODO - no support for custom map key formats at the moment
  /*implicit val customKeyMapFormat: MapKeyFormat[CustomKey] =
    MapKeyFormat[CustomKey](s => CustomKey(s.toLong), _.value.toString)

  class CustomKeyMapMeta extends MCcMeta[CustomKeyMap, CustomKeyMapMeta]("asd")

  it should "write/read custom keyed map" in {

    val meta = new CustomKeyMapMeta
    val v = CustomKeyMap(Map(CustomKey(1L) -> 1L))
    val bson = meta.write(v)
    meta.read(bson) shouldBe v
  }*/

  class OptMapMeta extends MCcMeta[OptMapStrLong, OptMapMeta]("omm") {
    @f val value = OptMapField[Long]
  }
  test("deal with type aliases inside options") {
    val meta = new OptMapMeta
    val v = OptMapStrLong(Some(Map("ab" -> 4L)), Some(Map("xy" -> 2L)))
    val bson = meta.write(v)
    assertEquals(meta.read(bson), v)
  }

  class MapMeta extends MCcMeta[MapStrLong, MapMeta]("omm") {
    val value = MapField("value")(StringMapKeyFormat)
  }
  test("deal with type aliases") {
    val meta = new MapMeta
    val v = MapStrLong(Map("ab" -> 4L), Map("xy" -> 2L))
    val bson = meta.write(v)
    assertEquals(meta.read(bson), v)
  }
}
