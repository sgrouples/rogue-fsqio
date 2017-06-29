package me.sgrouples.rogue.cc

import me.sgrouples.rogue.{ BsonFormats, EnumNameFormats }
import org.bson.types.ObjectId
import org.scalatest.{ FlatSpec, MustMatchers }
import me.sgrouples.rogue._
import BsonFormats._
import EnumNameFormats._
import me.sgrouples.rogue.cc.Metas.VenueRMeta
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Seconds, Span }

import scala.concurrent.Future
import scala.util.Try

case class TestDomainObject(id: ObjectId)

trait TestQueryTraitA[OwnerType] {
  requires: OwnerType with QueryFieldHelpers[OwnerType] =>

  val int = IntField
  val int_named = IntField("int_custom_name")

}

trait TestQueryTraitB[OwnerType] {
  requires: OwnerType with QueryFieldHelpers[OwnerType] =>

  val optInt = OptIntField
  val optInt_named = OptIntField("optInt_custom_name")

}

class TestDomainObjectMeta extends RCcMetaExt[TestDomainObject, TestDomainObjectMeta]
    with TestQueryTraitA[TestDomainObjectMeta]
    with TestQueryTraitB[TestDomainObjectMeta] {

  val claims = ListField[String]

  val string = StringField
  val string_named = StringField("string_custom_name")

  val optString = OptStringField
  val optString_named = OptStringField("optString_custom_name")

  val long = LongField
  val long_named = LongField("long_custom_name")

  val optLong = OptLongField
  val optLong_named = OptLongField("optLong_custom_name")

  val double = DoubleField
  val double_named = DoubleField("double_custom_name")

  val optDouble = OptDoubleField
  val optDouble_named = OptDoubleField("optDouble_custom_name")

  val objectId = ObjectIdField
  val objectId_named = ObjectIdField("objectId_custom_name")

  val optObjectId = OptObjectIdField
  val optObjectId_named = OptObjectIdField("optObjectId_custom_name")

  val randomSomething = 42

  val backwardCompatibilityCheck = new StringField("foo", this)

  val uuid = UUIdField
  val uuid_named = UUIdField("uuid_custom_name")

  val optUUID = OptUUIdField
  val optUUID_named = OptUUIdField("optUUID_custom_name")

  val localDateTime = LocalDateTimeField
  val localDateTime_named = LocalDateTimeField("localDateTime_custom_name")

  val optLocalDateTime = OptLocalDateTimeField
  val optLocalDateTime_named = OptLocalDateTimeField("optLocalDateTime_custom_name")

  val instant = InstantField
  val instant_named = InstantField("instant_custom_name")

  val optInstant = OptInstantField
  val optInstant_named = OptInstantField("optInstant_custom_name")

  val boolean = BooleanField
  val boolean_named = BooleanField("boolean_custom_name")

  val optBoolean = OptBooleanField
  val optBoolean_named = OptBooleanField("optBoolean_custom_name")

}

class QueryFieldHelperSpec extends FlatSpec with MustMatchers with ScalaFutures {

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val patience = PatienceConfig(scaled(Span(13, Seconds)))

  val TestDomainObjects = new TestDomainObjectMeta

  "QueryFieldHelper" should "auto-resolve field names" in {

    TestDomainObjects.int.name mustBe "int"
    TestDomainObjects.int_named.name mustBe "int_custom_name"

    TestDomainObjects.optInt.name mustBe "optInt"
    TestDomainObjects.optInt_named.name mustBe "optInt_custom_name"

    TestDomainObjects.string.name mustBe "string"
    TestDomainObjects.string_named.name mustBe "string_custom_name"

    TestDomainObjects.optString.name mustBe "optString"
    TestDomainObjects.optString_named.name mustBe "optString_custom_name"

    TestDomainObjects.long.name mustBe "long"
    TestDomainObjects.long_named.name mustBe "long_custom_name"

    TestDomainObjects.optLong.name mustBe "optLong"
    TestDomainObjects.optLong_named.name mustBe "optLong_custom_name"

    TestDomainObjects.double.name mustBe "double"
    TestDomainObjects.double_named.name mustBe "double_custom_name"

    TestDomainObjects.optDouble.name mustBe "optDouble"
    TestDomainObjects.optDouble_named.name mustBe "optDouble_custom_name"

    TestDomainObjects.objectId.name mustBe "objectId"
    TestDomainObjects.objectId_named.name mustBe "objectId_custom_name"

    TestDomainObjects.optObjectId.name mustBe "optObjectId"
    TestDomainObjects.optObjectId_named.name mustBe "optObjectId_custom_name"

    TestDomainObjects.uuid.name mustBe "uuid"
    TestDomainObjects.uuid_named.name mustBe "uuid_custom_name"

    TestDomainObjects.optUUID.name mustBe "optUUID"
    TestDomainObjects.optUUID_named.name mustBe "optUUID_custom_name"

    TestDomainObjects.localDateTime.name mustBe "localDateTime"
    TestDomainObjects.localDateTime_named.name mustBe "localDateTime_custom_name"

    TestDomainObjects.optLocalDateTime.name mustBe "optLocalDateTime"
    TestDomainObjects.optLocalDateTime_named.name mustBe "optLocalDateTime_custom_name"

    TestDomainObjects.instant.name mustBe "instant"
    TestDomainObjects.instant_named.name mustBe "instant_custom_name"

    TestDomainObjects.optInstant.name mustBe "optInstant"
    TestDomainObjects.optInstant_named.name mustBe "optInstant_custom_name"

    TestDomainObjects.boolean.name mustBe "boolean"
    TestDomainObjects.boolean_named.name mustBe "boolean_custom_name"

    TestDomainObjects.optBoolean.name mustBe "optBoolean"
    TestDomainObjects.optBoolean_named.name mustBe "optBoolean_custom_name"

  }

  case class AnotherValue(a: String)

  class AnotherTestMeta extends RCcMetaExt[AnotherValue, AnotherTestMeta] {
    val a = StringField
    val b = StringField("a")
  }

  class MultiThreadedTestMeta extends RCcMetaExt[AnotherValue, MultiThreadedTestMeta] {
    val a = StringField
    val b = StringField
    val c = StringField
    val d = StringField
    val e = StringField
    val f = StringField
  }

  it should "fail when name is duplicated" in {
    Try(new AnotherTestMeta).toString mustBe "Failure(java.lang.IllegalArgumentException: Field with name a is already defined)"
  }

  case class DifferentValue(a: String)

  class DifferentTestMeta extends RCcMetaExt[DifferentValue, DifferentTestMeta] {
    val a = StringField
  }

  it should "not fail when resolving an inner meta class" in {
    (new DifferentTestMeta).a.name mustBe "a"
  }

  it should "not fail in multi threaded env" in {

    Future.sequence {
      for (_ <- 1 to 1000) yield Future(new MultiThreadedTestMeta)
    }.futureValue
  }
}
