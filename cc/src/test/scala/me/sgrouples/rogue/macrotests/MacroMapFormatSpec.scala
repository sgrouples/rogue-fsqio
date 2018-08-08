package me.sgrouples.rogue.macrotests

import me.sgrouples.rogue.cc.CustomKey
import me.sgrouples.rogue.cc.macros._

import org.bson.types.ObjectId
import org.scalatest.{ FlatSpec, Matchers }
import shapeless.tag
import shapeless.tag.@@
class MacroMapFormatSpec extends FlatSpec with Matchers {

  case class StringMap(value: Map[String, Long])
  case class ObjectIdMap(value: Map[ObjectId, Long])

  trait M

  type ObjectIdSubtype = ObjectId @@ M

  case class ObjectIdSubtypeMap(value: Map[ObjectIdSubtype, Long])
  case class CustomKeyMap(value: Map[CustomKey, Long])

  class StringMapMeta extends MCcMeta[StringMap, StringMapMeta]("smm")

  "MapFormat" should "write/read string keyed map" in {

    val meta = new StringMapMeta
    val v = StringMap(Map("Hi" -> 1))
    val bson = meta.write(v)
    meta.read(bson) shouldBe v

  }

  class ObjectIdMapMeta extends MCcMeta[ObjectIdMap, ObjectIdMapMeta]("oidm")

  it should "write/read objectId keyed map" in {

    val meta = new ObjectIdMapMeta
    val v = ObjectIdMap(Map(ObjectId.get() -> 1))
    val bson = meta.write(v)
    meta.read(bson) shouldBe v
  }

  class ObjectIdSubtypeMapMeta extends MCcMeta[ObjectIdSubtypeMap, ObjectIdSubtypeMapMeta]("oidsub")

  it should "write/read objectId subtype keyed map" in {

    val meta = new ObjectIdSubtypeMapMeta
    val v = ObjectIdSubtypeMap(Map(tag[M](ObjectId.get()) -> 1))
    val bson = meta.write(v)
    meta.read(bson) shouldBe v
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
}
