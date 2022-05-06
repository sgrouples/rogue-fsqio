package me.sgrouples.rogue.cc

import munit.FunSuite
import me.sgrouples.rogue.cc.macros.*
import me.sgrouples.rogue.tagsfortest.*
import me.sgrouples.rogue.map.MapKeyFormats.{given, *}
//import me.sgrouples.rogue.EnumNameFormats._

case class CaseClass1(id: Long)

case class CaseClass2(id: Long)

object SubtypedClass extends TypedObjectId[SubtypedClass, SubtypedClass]
case class SubtypedClass(id: SubtypedClass.Id)

trait Tag

object ExampleEnum extends Enumeration {
  val one = Value
  val two = Value
}

class MCcSpec extends FunSuite {

  final val suffix = "_with_custom_name"

  private class CaseClass2Meta extends MCcMeta[CaseClass2, CaseClass2Meta]

  private val CaseClasses2 = new CaseClass2Meta

  private class CaseClass1Meta extends MCcMeta[CaseClass1, CaseClass1Meta] {

    val intField = IntField("intField")
    val intField_named = IntField(s"intField$suffix")

    val optIntField = OptIntField("optIntField")
    val optIntField_named = OptIntField(s"optIntField$suffix")

    val intSubtypeField = IntSubtypeField[Int @@ Tag]("intSubtypeField")
    val intSubtypeField_named =
      IntSubtypeField[Int @@ Tag](s"intSubtypeField$suffix")

    val optIntSubtypeField = OptIntSubtypeField[String @@ Tag]("optIntSubtypeField")
    val optIntSubtypeField_named =
      OptIntSubtypeField[Int @@ Tag](s"optIntSubtypeField$suffix")

    val stringField = StringField("stringField")
    val stringField_named = StringField(s"stringField$suffix")

    val optStringField = OptStringField("optStringField")
    val optStringField_named = OptStringField(s"optStringField$suffix")

    val stringSubtypeField = StringSubtypeField("stringSubtypeField")
    val stringSubtypeField_named = StringSubtypeField(s"stringSubtypeField$suffix")

    val optStringSubtypeField = OptStringSubtypeField[String @@ Tag]("optStringSubtypeField")
    val optStringSubtypeField_named =
      OptStringSubtypeField[String @@ Tag](s"optStringSubtypeField$suffix")

    val longField = LongField("longField")
    val longField_named = LongField(s"longField$suffix")

    val optLongField = OptLongField("optLongField")
    val optLongField_named = OptLongField(s"optLongField$suffix")

    val longSubtypeField = LongSubtypeField[Long @@ Tag]("longSubtypeField")
    val longSubtypeField_named = LongSubtypeField[Long @@ Tag](s"longSubtypeField$suffix")

    val optLongSubtypeField = OptLongSubtypeField[Long @@ Tag]("optLongSubtypeField")
    val optLongSubtypeField_named =
      OptLongSubtypeField[Long @@ Tag](s"optLongSubtypeField$suffix")

    val doubleField = DoubleField("doubleField")
    val doubleField_named = DoubleField(s"doubleField$suffix")

    val optDoubleField = OptDoubleField("optDoubleField")
    val optDoubleField_named = OptDoubleField(s"optDoubleField$suffix")

    val objectIdField = ObjectIdField("objectIdField")
    val objectIdField_named = ObjectIdField(s"objectIdField$suffix")

    val optObjectIdField = OptObjectIdField("optObjectIdField")
    val optObjectIdField_named = OptObjectIdField(s"optObjectIdField$suffix")

    val ObjectIdSubtypeField = ObjectIdSubtypeField[ObjectId @@ Tag]("ObjectIdSubtypeField")
    val ObjectIdSubtypeField_named =
      ObjectIdSubtypeField[ObjectId @@Tag](s"ObjectIdSubtypeField$suffix")

    val optObjectIdSubtypeField = OptObjectIdSubtypeField[ObjectId @@ Tag]
    val optObjectIdSubtypeField_named =
      OptObjectIdSubtypeField[ObjectId @@ Tag](s"optObjectIdSubtypeField$suffix")

    val uuIdField = UUIdField("uuIdField")
    val uuIdField_named = UUIdField(s"uuIdField$suffix")

    val optUUIdField = OptUUIdField("optUUIdField")
    val optUUIdField_named = OptUUIdField(s"optUUIdField$suffix")

    val uuIdSubtypeField = UUIdSubtypeField[UUID @@ Tag]("uuIdSubtypeField")
    val uuIdSubtypeField_named = UUIdSubtypeField[UUID @@ Tag](s"uuIdSubtypeField$suffix")

    val optUUIdSubtypeField = OptUUIdSubtypeField[UUID @@ Tag]("optUUIdSubtypeField")
    val optUUIdSubtypeField_named =
      OptUUIdSubtypeField[UUID @@ Tag](s"optUUIdSubtypeField$suffix")

    val localDateTimeField = LocalDateTimeField("localDateTimeField")
    val localDateTimeField_named = LocalDateTimeField(
      s"localDateTimeField$suffix"
    )

    val optLocalDateTimeField = OptLocalDateTimeField("optLocalDateTimeField")
    val optLocalDateTimeField_named = OptLocalDateTimeField(
      s"optLocalDateTimeField$suffix"
    )

    val instantField = InstantField("instantField")
    val instantField_named = InstantField(s"instantField$suffix")

    val optInstantField = OptInstantField("optInstantField")
    val optInstantField_named = OptInstantField(s"optInstantField$suffix")

    val booleanField = BooleanField("booleanField")
    val booleanField_named = BooleanField(s"booleanField$suffix")

    val optBooleanField = OptBooleanField("optBooleanField")
    val optBooleanField_named = OptBooleanField(s"optBooleanField$suffix")

    val enumField = EnumField("enumField", ExampleEnum)
    val enumField_named = EnumField("enumField_named", ExampleEnum)

    val enumFieldByParam = EnumField("enumFieldByParam", ExampleEnum)
    val enumFieldByParam_named =
      EnumField(s"enumFieldByParam$suffix", ExampleEnum)

    val optEnumField = OptEnumField("optEnumField", ExampleEnum)
    val optEnumField_named =
      OptEnumField(s"optEnumField$suffix", ExampleEnum)

    val optEnumFieldByParams = OptEnumField("optEnumFieldByParams", ExampleEnum)
    val optEnumFieldByParam_named =
      OptEnumField(s"optEnumFieldByParams$suffix", ExampleEnum)

    val enumIdField = EnumIdField("enumIdField", ExampleEnum)
    val enumIdField_named = EnumIdField(s"enumIdField$suffix", ExampleEnum)

    val enumIdFieldByParam = EnumIdField("enumIdFieldByParam", ExampleEnum)
    val enumIdFieldByParam_named =
      EnumIdField(s"enumIdFieldByParam$suffix", ExampleEnum)

    val optEnumIdField = OptEnumIdField("optEnumIdField", ExampleEnum)
    val optEnumIdField_named =
      OptEnumIdField(s"optEnumIdField$suffix", ExampleEnum)

    val optEnumIdFieldByParam = OptEnumIdField("optEnumIdFieldByParam", ExampleEnum)
    val optEnumIdFieldByParam_named =
      OptEnumIdField(s"optEnumIdFieldByParam$suffix", ExampleEnum)

    val listField = ListField[Int]("listField")
    val listField_named = ListField[Int](s"listField$suffix")

    val optListField = OptListField[Int]("optListField")
    val optListField_named = OptListField[Int](s"optListField$suffix")

    val arrayField = ArrayField[String]("arrayField")
    val arrayField_named = ArrayField[String](s"arrayField$suffix")

    val optArrayField = OptArrayField[String]("optArrayField")
    val optArrayField_named = OptArrayField[String](s"optArrayField$suffix")

    val classField = ClassField[CaseClass2, CaseClass2Meta]("classField", CaseClasses2)
    val classField_named =
      ClassField[CaseClass2, CaseClass2Meta](s"classField$suffix", CaseClasses2)

    val optClassField = OptClassField[CaseClass2, CaseClass2Meta]("optClassField", CaseClasses2)
    val optClassField_named = OptClassField[CaseClass2, CaseClass2Meta](
      s"optClassField$suffix",
      CaseClasses2
    )

    val classRequiredField = ClassRequiredField[CaseClass2, CaseClass2Meta]("classRequiredField",
      CaseClasses2,
      CaseClass2(0L)
    )
    val classRequiredField_named =
      ClassRequiredField[CaseClass2, CaseClass2Meta](
        s"classRequiredField$suffix",
        CaseClasses2,
        CaseClass2(0L)
      )

    val classListField =
      ClassListField[CaseClass2, CaseClass2Meta]("classListField", CaseClasses2)
    val classListField_named = ClassListField[CaseClass2, CaseClass2Meta](
      s"classListField$suffix",
      CaseClasses2
    )

    val classArrayField =
      ClassArrayField[CaseClass2, CaseClass2Meta]("classArrayField", CaseClasses2)
    val classArrayField_named = ClassArrayField[CaseClass2, CaseClass2Meta](
      s"classArrayField$suffix",
      CaseClasses2
    )

    val mapField = MapField[String, String]("mapField")
    val mapField_named = MapField[String, String](s"mapField$suffix")

    val optMapField = OptMapField[Long]("optMapField")
    val optMapField_named = OptMapField[Long](s"optMapField$suffix")

  }

  private val CaseClasses1 = new CaseClass1Meta

  test("MCc fields should be properly initialized") {

    assertEquals(
      CaseClasses1.fieldNamesSorted,
      Seq(
        "intField",
        "intField_with_custom_name",
        "optIntField",
        "optIntField_with_custom_name",
        "intSubtypeField",
        "intSubtypeField_with_custom_name",
        "optIntSubtypeField",
        "optIntSubtypeField_with_custom_name",
        "stringField",
        "stringField_with_custom_name",
        "optStringField",
        "optStringField_with_custom_name",
        "stringSubtypeField",
        "stringSubtypeField_with_custom_name",
        "optStringSubtypeField",
        "optStringSubtypeField_with_custom_name",
        "longField",
        "longField_with_custom_name",
        "optLongField",
        "optLongField_with_custom_name",
        "longSubtypeField",
        "longSubtypeField_with_custom_name",
        "optLongSubtypeField",
        "optLongSubtypeField_with_custom_name",
        "doubleField",
        "doubleField_with_custom_name",
        "optDoubleField",
        "optDoubleField_with_custom_name",
        "objectIdField",
        "objectIdField_with_custom_name",
        "optObjectIdField",
        "optObjectIdField_with_custom_name",
        "ObjectIdSubtypeField",
        "ObjectIdSubtypeField_with_custom_name",
        "optObjectIdSubtypeField",
        "optObjectIdSubtypeField_with_custom_name",
        "uuIdField",
        "uuIdField_with_custom_name",
        "optUUIdField",
        "optUUIdField_with_custom_name",
        "uuIdSubtypeField",
        "uuIdSubtypeField_with_custom_name",
        "optUUIdSubtypeField",
        "optUUIdSubtypeField_with_custom_name",
        "localDateTimeField",
        "localDateTimeField_with_custom_name",
        "optLocalDateTimeField",
        "optLocalDateTimeField_with_custom_name",
        "instantField",
        "instantField_with_custom_name",
        "optInstantField",
        "optInstantField_with_custom_name",
        "booleanField",
        "booleanField_with_custom_name",
        "optBooleanField",
        "optBooleanField_with_custom_name",
        "enumField",
        "enumField_with_custom_name",
        "enumFieldByParam",
        "enumFieldByParam_with_custom_name",
        "optEnumField",
        "optEnumField_with_custom_name",
        "optEnumFieldByParams",
        "optEnumFieldByParams_with_custom_name",
        "enumIdField",
        "enumIdField_with_custom_name",
        "enumIdFieldByParam",
        "enumIdFieldByParam_with_custom_name",
        "optEnumIdField",
        "optEnumIdField_with_custom_name",
        "optEnumIdFieldByParam",
        "optEnumIdFieldByParam_with_custom_name",
        "listField",
        "listField_with_custom_name",
        "optListField",
        "optListField_with_custom_name",
        "arrayField",
        "arrayField_with_custom_name",
        "optArrayField",
        "optArrayField_with_custom_name",
        "classField",
        "classField_with_custom_name",
        "optClassField",
        "optClassField_with_custom_name",
        "classRequiredField",
        "classRequiredField_with_custom_name",
        "classListField",
        "classListField_with_custom_name",
        "classArrayField",
        "classArrayField_with_custom_name",
        "mapField",
        "mapField_with_custom_name",
        "optMapField",
        "optMapField_with_custom_name"
      )
    )
  }

  private class SubtypedMeta extends MCcMeta[SubtypedClass, SubtypedMeta]() {
    val id = ObjectIdSubtypeField[SubtypedClass.Id]("id")
    val idField_named = IntField(s"idField$suffix")
  }
  private val subtypedMeta = new SubtypedMeta
  assertEquals(
    subtypedMeta.fieldNamesSorted,
    Seq("id", "idField_with_custom_name")
  )

}
