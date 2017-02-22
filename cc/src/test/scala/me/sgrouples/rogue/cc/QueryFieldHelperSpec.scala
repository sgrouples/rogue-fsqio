package me.sgrouples.rogue.cc

import me.sgrouples.rogue.{ BsonFormats, EnumNameFormats }
import org.bson.types.ObjectId
import org.scalatest.{ FlatSpec, MustMatchers }
import me.sgrouples.rogue._
import BsonFormats._
import EnumNameFormats._
import me.sgrouples.rogue.cc.Metas.VenueRMeta

case class TestDomainObject(id: ObjectId)

trait TestQueryTraitA[OwnerType] {
  requires: OwnerType with QueryFieldHelpers[OwnerType] =>

  val int = IntField
  val int_name = IntField("int_name")

}

trait TestQueryTraitB[OwnerType] {
  requires: OwnerType with QueryFieldHelpers[OwnerType] =>

  val OptInt = OptIntField
  val OptInt_name = OptIntField("OptInt_name")

}

class TestDomainObjectMeta extends RCcMeta[TestDomainObject]
    with QueryFieldHelpers[TestDomainObjectMeta] {

  val claims = ListField[String]

  val string = StringField
  val string_name = StringField("string_name")

  val OptString = OptStringField
  val OptString_name = OptStringField("OptString_name")

  val long = LongField
  val long_name = LongField("long_name")

  val OptLong = OptLongField
  val OptLong_name = OptLongField("OptLong_name")

  val double = DoubleField
  val double_name = DoubleField("double_name")

  val OptDouble = OptDoubleField
  val OptDouble_name = OptDoubleField("OptDouble_name")

  val objectId = ObjectIdField
  val objectId_name = ObjectIdField("objectId_name")

  val OptObjectId = OptObjectIdField
  val OptObjectId_name = OptObjectIdField("OptObjectId_name")

  val randomSomething = 42

  val backwardCompatibilityCheck = new StringField("foo", this)

  val uuid = UUIDField
  val uuid_name = UUIDField("uuid_name")

  val OptUUID = OptUUIDField
  val OptUUID_name = OptUUIDField("OptUUID_name")

  val localDateTime = LocalDateTimeField
  val localDateTime_name = LocalDateTimeField("localDateTime_name")

  val OptLocalDateTime = OptLocalDateTimeField
  val OptLocalDateTime_name = OptLocalDateTimeField("OptLocalDateTime_name")

  val instant = InstantField
  val instant_name = InstantField("instant_name")

  val OptInstant = OptInstantField
  val OptInstant_name = OptInstantField("OptInstant_name")

  val boolean = BooleanField
  val boolean_name = BooleanField("boolean_name")

  val OptBoolean = OptBooleanField
  val OptBoolean_name = OptBooleanField("OptBoolean_name")

}

class QueryFieldHelperSpec extends FlatSpec with MustMatchers {

  val TestDomainObjects = new TestDomainObjectMeta
  val V = new VenueRMeta

  "QueryFieldHelper" should "auto-resolve field names" in {

    //    TestDomainObjects.int.name mustBe "int"
    //    TestDomainObjects.int_name.name mustBe "int_name"
    //
    //    TestDomainObjects.OptInt.name mustBe "OptInt"
    //    TestDomainObjects.OptInt_name.name mustBe "OptInt_name"

    TestDomainObjects.string.name mustBe "string"
    TestDomainObjects.string_name.name mustBe "string_name"

    TestDomainObjects.OptString.name mustBe "OptString"
    TestDomainObjects.OptString_name.name mustBe "OptString_name"

    TestDomainObjects.long.name mustBe "long"
    TestDomainObjects.long_name.name mustBe "long_name"

    TestDomainObjects.OptLong.name mustBe "OptLong"
    TestDomainObjects.OptLong_name.name mustBe "OptLong_name"

    TestDomainObjects.double.name mustBe "double"
    TestDomainObjects.double_name.name mustBe "double_name"

    TestDomainObjects.OptDouble.name mustBe "OptDouble"
    TestDomainObjects.OptDouble_name.name mustBe "OptDouble_name"

    TestDomainObjects.objectId.name mustBe "objectId"
    TestDomainObjects.objectId_name.name mustBe "objectId_name"

    TestDomainObjects.OptObjectId.name mustBe "OptObjectId"
    TestDomainObjects.OptObjectId_name.name mustBe "OptObjectId_name"

    TestDomainObjects.uuid.name mustBe "uuid"
    TestDomainObjects.uuid_name.name mustBe "uuid_name"

    TestDomainObjects.OptUUID.name mustBe "OptUUID"
    TestDomainObjects.OptUUID_name.name mustBe "OptUUID_name"

    TestDomainObjects.localDateTime.name mustBe "localDateTime"
    TestDomainObjects.localDateTime_name.name mustBe "localDateTime_name"

    TestDomainObjects.OptLocalDateTime.name mustBe "OptLocalDateTime"
    TestDomainObjects.OptLocalDateTime_name.name mustBe "OptLocalDateTime_name"

    TestDomainObjects.instant.name mustBe "instant"
    TestDomainObjects.instant_name.name mustBe "instant_name"

    TestDomainObjects.OptInstant.name mustBe "OptInstant"
    TestDomainObjects.OptInstant_name.name mustBe "OptInstant_name"

    TestDomainObjects.boolean.name mustBe "boolean"
    TestDomainObjects.boolean_name.name mustBe "boolean_name"

    TestDomainObjects.OptBoolean.name mustBe "OptBoolean"
    TestDomainObjects.OptBoolean_name.name mustBe "OptBoolean_name"

  }
}
