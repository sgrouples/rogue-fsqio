package me.sgrouples.rogue.cc

import org.scalatest.{ FlatSpec, Matchers }
import me.sgrouples.rogue.BsonFormats._
import me.sgrouples.rogue.EnumNameFormats._

case class CaseClass1(id: Long)

case class CaseClass2(id: Long)

case class SubtypedClass(id: SubtypedClass.Id)

object SubtypedClass extends TypedObjectId[SubtypedClass, SubtypedClass]

trait Tag

object ExampleEnum extends Enumeration {
  val one = Value
  val two = Value
}

class RCcMetaExtSpec extends FlatSpec with Matchers {

  final val suffix = "_with_custom_name"

  private class CaseClass2Meta extends RCcMetaExt[CaseClass2, CaseClass2Meta]

  private val CaseClasses2 = new CaseClass2Meta

  private class CaseClass1Meta extends RCcMetaExt[CaseClass1, CaseClass1Meta] {

    val intField = IntField
    val intField_named = IntField(s"intField$suffix")

    val optIntField = OptIntField
    val optIntField_named = OptIntField(s"optIntField$suffix")

    val intTaggedField = IntTaggedField[Tag]
    val intTaggedField_named = IntTaggedField[CaseClass1Meta](s"intTaggedField$suffix")

    val optIntTaggedField = OptIntTaggedField[Tag]
    val optIntTaggedField_named = OptIntTaggedField[Tag](s"optIntTaggedField$suffix")

    val stringField = StringField
    val stringField_named = StringField(s"stringField$suffix")

    val optStringField = OptStringField
    val optStringField_named = OptStringField(s"optStringField$suffix")

    val stringTaggedField = StringTaggedField
    val stringTaggedField_named = StringTaggedField(s"stringTaggedField$suffix")

    val optStringTaggedField = OptStringTaggedField[Tag]
    val optStringTaggedField_named = OptStringTaggedField[Tag](s"optStringTaggedField$suffix")

    val longField = LongField
    val longField_named = LongField(s"longField$suffix")

    val optLongField = OptLongField
    val optLongField_named = OptLongField(s"optLongField$suffix")

    val longTaggedField = LongTaggedField[Tag]
    val longTaggedField_named = LongTaggedField(s"longTaggedField$suffix")

    val optLongTaggedField = OptLongTaggedField[Tag]
    val optLongTaggedField_named = OptLongTaggedField[Tag](s"optLongTaggedField$suffix")

    val doubleField = DoubleField
    val doubleField_named = DoubleField(s"doubleField$suffix")

    val optDoubleField = OptDoubleField
    val optDoubleField_named = OptDoubleField(s"optDoubleField$suffix")

    val objectIdField = ObjectIdField
    val objectIdField_named = ObjectIdField(s"objectIdField$suffix")

    val optObjectIdField = OptObjectIdField
    val optObjectIdField_named = OptObjectIdField(s"optObjectIdField$suffix")

    val objectIdTaggedField = ObjectIdTaggedField[Tag]
    val objectIdTaggedField_named = ObjectIdTaggedField[Tag](s"objectIdTaggedField$suffix")

    val optObjectIdTaggedField = OptObjectIdTaggedField[Tag]
    val optObjectIdTaggedField_named = OptObjectIdTaggedField[Tag](s"optObjectIdTaggedField$suffix")

    val uuIdField = UUIdField
    val uuIdField_named = UUIdField(s"uuIdField$suffix")

    val optUUIdField = OptUUIdField
    val optUUIdField_named = OptUUIdField(s"optUUIdField$suffix")

    val uuIdTaggedField = UUIdTaggedField[Tag]
    val uuIdTaggedField_named = UUIdTaggedField[Tag](s"uuIdTaggedField$suffix")

    val optUUIdTaggedField = OptUUIdTaggedField[Tag]
    val optUUIdTaggedField_named = OptUUIdTaggedField[Tag](s"optUUIdTaggedField$suffix")

    val localDateTimeField = LocalDateTimeField
    val localDateTimeField_named = LocalDateTimeField(s"localDateTimeField$suffix")

    val optLocalDateTimeField = OptLocalDateTimeField
    val optLocalDateTimeField_named = OptLocalDateTimeField(s"optLocalDateTimeField$suffix")

    val instantField = InstantField
    val instantField_named = InstantField(s"instantField$suffix")

    val optInstantField = OptInstantField
    val optInstantField_named = OptInstantField(s"optInstantField$suffix")

    val booleanField = BooleanField
    val booleanField_named = BooleanField(s"booleanField$suffix")

    val optBooleanField = OptBooleanField
    val optBooleanField_named = OptBooleanField(s"optBooleanField$suffix")

    val enumField = EnumField[ExampleEnum.type]
    val enumField_named = EnumField[ExampleEnum.type](s"enumField$suffix")

    val enumFieldByParam = EnumField(ExampleEnum)
    val enumFieldByParam_named = EnumField(s"enumFieldByParam$suffix", ExampleEnum)

    val optEnumField = OptEnumField[ExampleEnum.type]
    val optEnumField_named = OptEnumField[ExampleEnum.type](s"optEnumField$suffix")

    val optEnumFieldByParams = OptEnumField(ExampleEnum)
    val optEnumFieldByParam_named = OptEnumField(s"optEnumFieldByParams$suffix", ExampleEnum)

    val enumIdField = EnumIdField[ExampleEnum.type]
    val enumIdField_named = EnumIdField[ExampleEnum.type](s"enumIdField$suffix")

    val enumIdFieldByParam = EnumIdField(ExampleEnum)
    val enumIdFieldByParam_named = EnumIdField(s"enumIdFieldByParam$suffix", ExampleEnum)

    val optEnumIdField = OptEnumIdField[ExampleEnum.type]
    val optEnumIdField_named = OptEnumIdField[ExampleEnum.type](s"optEnumIdField$suffix")

    val optEnumIdFieldByParam = OptEnumIdField(ExampleEnum)
    val optEnumIdFieldByParam_named = OptEnumIdField(s"optEnumIdFieldByParam$suffix", ExampleEnum)

    val listField = ListField[Int]
    val listField_named = ListField[Int](s"listField$suffix")

    val optListField = OptListField[Int]
    val optListField_named = OptListField[Int](s"optListField$suffix")

    val arrayField = ArrayField[String]
    val arrayField_named = ArrayField[String](s"arrayField$suffix")

    val optArrayField = OptArrayField[String]
    val optArrayField_named = OptArrayField[String](s"optArrayField$suffix")

    val classField = ClassField[CaseClass2, CaseClass2Meta](CaseClasses2)
    val classField_named = ClassField[CaseClass2, CaseClass2Meta](s"classField$suffix", CaseClasses2)

    val optClassField = OptClassField[CaseClass2, CaseClass2Meta](CaseClasses2)
    val optClassField_named = OptClassField[CaseClass2, CaseClass2Meta](s"optClassField$suffix", CaseClasses2)

    val classRequiredField = ClassRequiredField[CaseClass2, CaseClass2Meta](CaseClasses2, CaseClass2(0L))
    val classRequiredField_named = ClassRequiredField[CaseClass2, CaseClass2Meta](s"classRequiredField$suffix", CaseClasses2, CaseClass2(0L))

    val classListField = ClassListField[CaseClass2, CaseClass2Meta](CaseClasses2)
    val classListField_named = ClassListField[CaseClass2, CaseClass2Meta](s"classListField$suffix", CaseClasses2)

    val classArrayField = ClassArrayField[CaseClass2, CaseClass2Meta](CaseClasses2)
    val classArrayField_named = ClassArrayField[CaseClass2, CaseClass2Meta](s"classArrayField$suffix", CaseClasses2)

    val mapField = MapField[String]
    val mapField_named = MapField[String](s"mapField$suffix")

    val optMapField = OptMapField[Long]
    val optMapField_named = OptMapField[Long](s"optMapField$suffix")

  }

  private val CaseClasses1 = new CaseClass1Meta

  CaseClasses1.fieldNamesSorted shouldBe Seq(
    "intField",
    "intField_with_custom_name",
    "optIntField",
    "optIntField_with_custom_name",
    "intTaggedField",
    "intTaggedField_with_custom_name",
    "optIntTaggedField",
    "optIntTaggedField_with_custom_name",
    "stringField",
    "stringField_with_custom_name",
    "optStringField",
    "optStringField_with_custom_name",
    "stringTaggedField",
    "stringTaggedField_with_custom_name",
    "optStringTaggedField",
    "optStringTaggedField_with_custom_name",
    "longField",
    "longField_with_custom_name",
    "optLongField",
    "optLongField_with_custom_name",
    "longTaggedField",
    "longTaggedField_with_custom_name",
    "optLongTaggedField",
    "optLongTaggedField_with_custom_name",
    "doubleField",
    "doubleField_with_custom_name",
    "optDoubleField",
    "optDoubleField_with_custom_name",
    "objectIdField",
    "objectIdField_with_custom_name",
    "optObjectIdField",
    "optObjectIdField_with_custom_name",
    "objectIdTaggedField",
    "objectIdTaggedField_with_custom_name",
    "optObjectIdTaggedField",
    "optObjectIdTaggedField_with_custom_name",
    "uuIdField",
    "uuIdField_with_custom_name",
    "optUUIdField",
    "optUUIdField_with_custom_name",
    "uuIdTaggedField",
    "uuIdTaggedField_with_custom_name",
    "optUUIdTaggedField",
    "optUUIdTaggedField_with_custom_name",
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

  private class SubtypedMeta extends RCcMetaExt[SubtypedClass, SubtypedMeta]() {
    val id = ObjectIdSubtypeField[SubtypedClass.Id]
    val idField_named = IntField(s"idField$suffix")
  }
  private val subtypedMeta = new SubtypedMeta
  subtypedMeta.fieldNamesSorted shouldBe Seq("id", "idField_with_custom_name")

}
