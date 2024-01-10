package me.sgrouples.rogue.cc

import org.bson.types.ObjectId
import me.sgrouples.rogue._
import me.sgrouples.rogue.cc.macros.*
import me.sgrouples.rogue.cc.Metas.VenueRMeta
import me.sgrouples.rogue.cc.CcRogue.*
import munit.FunSuite
import scala.concurrent.Future
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global
case class TestDomainObject(id: ObjectId) derives MacroBsonFormat

trait BaseTraitA[OwnerType] {
  requires: OwnerType with QueryFieldHelpers[OwnerType] =>

  val baseA = IntField("baseA")
}

trait BaseTraitB[OwnerType] {
  requires: OwnerType with QueryFieldHelpers[OwnerType] =>

  val baseB = IntField("baseB")
}

trait TestQueryTraitA[OwnerType]
    extends BaseTraitA[OwnerType]
    with BaseTraitB[OwnerType] {
  requires: OwnerType with QueryFieldHelpers[OwnerType] =>
  val int = IntField("int")
  val int_named = IntField("int_custom_name")
}

trait TestQueryTraitB[OwnerType] {
  requires: OwnerType with QueryFieldHelpers[OwnerType] =>
  val optInt = OptIntField("optInt")
  val optInt_named = OptIntField("optInt_custom_name")

}

case class AnotherValue(a: String) derives MacroBsonFormat
case class DifferentValue(a: String) derives MacroBsonFormat

class TestDomainObjectMeta
    extends MCcMeta[TestDomainObject, TestDomainObjectMeta]
    with TestQueryTraitA[TestDomainObjectMeta]
    with TestQueryTraitB[TestDomainObjectMeta] {

  val claims = ListField[String]("claims")

  val string = StringField("string")
  val string_named = StringField("string_custom_name")

  val optString = OptStringField("optString")
  val optString_named = OptStringField("optString_custom_name")

  val long = LongField("long")
  val long_named = LongField("long_custom_name")

  val optLong = OptLongField("optLong")
  val optLong_named = OptLongField("optLong_custom_name")

  val double = DoubleField("double")
  val double_named = DoubleField("double_custom_name")

  val optDouble = OptDoubleField("optDouble")
  val optDouble_named = OptDoubleField("optDouble_custom_name")

  val objectId = ObjectIdField("objectId")
  val objectId_named = ObjectIdField("objectId_custom_name")

  val optObjectId = OptObjectIdField("optObjectId")
  val optObjectId_named = OptObjectIdField("optObjectId_custom_name")

  val randomSomething = 42

  val backwardCompatibilityCheck = new StringField("foo", this)

  val uuid = UUIdField("uuid")
  val uuid_named = UUIdField("uuid_custom_name")

  val optUUID = OptUUIdField("optUUID")
  val optUUID_named = OptUUIdField("optUUID_custom_name")

  val localDateTime = LocalDateTimeField("localDateTime")
  val localDateTime_named = LocalDateTimeField("localDateTime_custom_name")

  val optLocalDateTime = OptLocalDateTimeField("optLocalDateTime")
  val optLocalDateTime_named = OptLocalDateTimeField(
    "optLocalDateTime_custom_name"
  )

  val instant = InstantField("instant")
  val instant_named = InstantField("instant_custom_name")

  val optInstant = OptInstantField("optInstant")
  val optInstant_named = OptInstantField("optInstant_custom_name")

  val boolean = BooleanField("boolean")
  val boolean_named = BooleanField("boolean_custom_name")

  val optBoolean = OptBooleanField("optBoolean")
  val optBoolean_named = OptBooleanField("optBoolean_custom_name")

}

class QueryFieldHelperSpec extends FunSuite {

  import scala.concurrent.ExecutionContext.Implicits.global

  val TestDomainObjects = new TestDomainObjectMeta

  test("QueryFieldHelper should auto-resolve field names") {

    assertEquals(TestDomainObjects.baseA.name, "baseA")
    assertEquals(TestDomainObjects.baseB.name, "baseB")

    assertEquals(TestDomainObjects.int.name, "int")
    assertEquals(TestDomainObjects.int_named.name, "int_custom_name")

    assertEquals(TestDomainObjects.optInt.name, "optInt")
    assertEquals(TestDomainObjects.optInt_named.name, "optInt_custom_name")

    assertEquals(TestDomainObjects.string.name, "string")
    assertEquals(TestDomainObjects.string_named.name, "string_custom_name")

    assertEquals(TestDomainObjects.optString.name, "optString")
    assertEquals(
      TestDomainObjects.optString_named.name,
      "optString_custom_name"
    )

    assertEquals(TestDomainObjects.long.name, "long")
    assertEquals(TestDomainObjects.long_named.name, "long_custom_name")

    assertEquals(TestDomainObjects.optLong.name, "optLong")
    assertEquals(TestDomainObjects.optLong_named.name, "optLong_custom_name")

    assertEquals(TestDomainObjects.double.name, "double")
    assertEquals(TestDomainObjects.double_named.name, "double_custom_name")

    assertEquals(TestDomainObjects.optDouble.name, "optDouble")
    assertEquals(
      TestDomainObjects.optDouble_named.name,
      "optDouble_custom_name"
    )

    assertEquals(TestDomainObjects.objectId.name, "objectId")
    assertEquals(TestDomainObjects.objectId_named.name, "objectId_custom_name")

    assertEquals(TestDomainObjects.optObjectId.name, "optObjectId")
    assertEquals(
      TestDomainObjects.optObjectId_named.name,
      "optObjectId_custom_name"
    )

    assertEquals(TestDomainObjects.uuid.name, "uuid")
    assertEquals(TestDomainObjects.uuid_named.name, "uuid_custom_name")

    assertEquals(TestDomainObjects.optUUID.name, "optUUID")
    assertEquals(TestDomainObjects.optUUID_named.name, "optUUID_custom_name")

    assertEquals(TestDomainObjects.localDateTime.name, "localDateTime")
    assertEquals(
      TestDomainObjects.localDateTime_named.name,
      "localDateTime_custom_name"
    )

    assertEquals(TestDomainObjects.optLocalDateTime.name, "optLocalDateTime")
    assertEquals(
      TestDomainObjects.optLocalDateTime_named.name,
      "optLocalDateTime_custom_name"
    )

    assertEquals(TestDomainObjects.instant.name, "instant")
    assertEquals(TestDomainObjects.instant_named.name, "instant_custom_name")

    assertEquals(TestDomainObjects.optInstant.name, "optInstant")
    assertEquals(
      TestDomainObjects.optInstant_named.name,
      "optInstant_custom_name"
    )

    assertEquals(TestDomainObjects.boolean.name, "boolean")
    assertEquals(TestDomainObjects.boolean_named.name, "boolean_custom_name")

    assertEquals(TestDomainObjects.optBoolean.name, "optBoolean")
    assertEquals(
      TestDomainObjects.optBoolean_named.name,
      "optBoolean_custom_name"
    )

  }

  class AnotherTestMeta extends MCcMeta[AnotherValue, AnotherTestMeta] {
    val a = StringField("a")
    val b = StringField("a")
  }

  class MultiThreadedTestMeta
      extends MCcMeta[AnotherValue, MultiThreadedTestMeta] {
    val a = StringField("a")
    val b = StringField("b")
    val c = StringField("c")
    val d = StringField("d")
    val e = StringField("e")
    val f = StringField("f")
  }

  test("it should fail when name is duplicated") {
    assertEquals(
      Try(new AnotherTestMeta).toString,
      "Failure(java.lang.IllegalArgumentException: Field with name a is already defined)"
    )
  }

  class DifferentTestMeta
      extends MCcMeta[DifferentValue, DifferentTestMeta] {
      val a = StringField("a")
  }

  test("it should not fail when resolving an inner meta class") {
    assertEquals((new DifferentTestMeta).a.name, "a")
  }

  test("it should not fail in multi threaded env") {

    Future.sequence {
      for (_ <- 1 to 1000) yield Future(new MultiThreadedTestMeta)
    }
  }

}
