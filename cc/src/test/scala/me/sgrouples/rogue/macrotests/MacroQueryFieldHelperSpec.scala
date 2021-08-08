package me.sgrouples.rogue.macrotests

import me.sgrouples.rogue.cc.{QueryFieldHelpers, TestDomainObject}
import me.sgrouples.rogue.cc.macros._
import munit.FunSuite
import scala.concurrent.Future
import scala.util.Try
trait TestQueryTraitA[OwnerType] {
  requires: OwnerType with QueryFieldHelpers[OwnerType] =>
  @f val int = IntField
  @f val int_named = IntField("int_custom_name")
}

trait TestQueryTraitB[OwnerType] {
  requires: OwnerType with QueryFieldHelpers[OwnerType] =>
  @f val optInt = OptIntField
  @f val optInt_named = OptIntField("optInt_custom_name")

}

trait X {

  class TestDomainObjectMeta
      extends MCcMeta[TestDomainObject, TestDomainObjectMeta]("abc")
      with TestQueryTraitA[TestDomainObjectMeta]
      with TestQueryTraitB[TestDomainObjectMeta] {

    @f val claims = ListField[String]

    @f val string = StringField
    @f val string_named = StringField("string_custom_name")

    @f val optString = OptStringField
    @f val optString_named = OptStringField("optString_custom_name")

    @f val long = LongField
    @f val long_named = LongField("long_custom_name")

    @f val optLong = OptLongField
    @f val optLong_named = OptLongField("optLong_custom_name")

    @f val double = DoubleField
    @f val double_named = DoubleField("double_custom_name")

    @f val optDouble = OptDoubleField
    @f val optDouble_named = OptDoubleField("optDouble_custom_name")

    @f val objectId = ObjectIdField
    @f val objectId_named = ObjectIdField("objectId_custom_name")

    @f val optObjectId = OptObjectIdField
    @f val optObjectId_named = OptObjectIdField("optObjectId_custom_name")

    val randomSomething = 42

    //val backwardCompatibilityCheck = new StringField("foo", this)

    @f val uuid = UUIdField
    @f val uuid_named = UUIdField("uuid_custom_name")

    @f val optUUID = OptUUIdField
    @f val optUUID_named = OptUUIdField("optUUID_custom_name")

    @f val localDateTime = LocalDateTimeField
    @f val localDateTime_named = LocalDateTimeField("localDateTime_custom_name")

    @f val optLocalDateTime = OptLocalDateTimeField
    @f val optLocalDateTime_named = OptLocalDateTimeField(
      "optLocalDateTime_custom_name"
    )

    @f val instant = InstantField
    @f val instant_named = InstantField("instant_custom_name")

    @f val optInstant = OptInstantField
    @f val optInstant_named = OptInstantField("optInstant_custom_name")

    @f val boolean = BooleanField
    @f val boolean_named = BooleanField("boolean_custom_name")

    @f val optBoolean = OptBooleanField
    @f val optBoolean_named = OptBooleanField("optBoolean_custom_name")

  }

}

class MacroQueryFieldHelperSpec extends FunSuite with X {

  import scala.concurrent.ExecutionContext.Implicits.global

  val TestDomainObjects = new TestDomainObjectMeta

  test("QueryFieldHelper auto-resolve field names") {

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

  case class AnotherValue(a: String)

  class AnotherTestMeta extends MCcMeta[AnotherValue, AnotherTestMeta] {
    @f val a = StringField
    @f val b = StringField("a")
  }

  class MultiThreadedTestMeta
      extends MCcMeta[AnotherValue, MultiThreadedTestMeta] {
    @f val a = StringField
    @f val b = StringField
    @f val c = StringField
    @f val d = StringField
    @f val e = StringField
    @f val f = StringField
  }

  test("it should fail when name is duplicated") {
    assertEquals(
      Try(new AnotherTestMeta).toString,
      "Failure(java.lang.IllegalArgumentException: Field with name a is already defined)"
    )
  }

  case class DifferentValue(a: String)

  class DifferentTestMeta extends MCcMeta[DifferentValue, DifferentTestMeta] {
    @f val a = StringField
  }

  test("not fail when resolving an inner meta class") {
    assertEquals((new DifferentTestMeta).a.name, "a")
  }

  test("not fail in multi threaded env") {

    Future.sequence {
      for (_ <- 1 to 1000) yield Future(new MultiThreadedTestMeta)
    }
  }

}
