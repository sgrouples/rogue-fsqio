package me.sgrouples.rogue.cc

import me.sgrouples.rogue.EnumSerializeValue
import munit.FunSuite
import CcRogue._
import me.sgrouples.rogue.BsonFormats._
import me.sgrouples.rogue.EnumAnnotatedFormats._

@EnumSerializeValue
object EnumIdFieldEnum extends Enumeration {
  val one: Value = Value
  val two: Value = Value
}

case class EnumIdFieldClass(enum: EnumIdFieldEnum.Value)

class EnumIdFieldSpecMeta
    extends RCcMetaExt[EnumIdFieldClass, EnumIdFieldSpecMeta] {

  val `enum` = OptEnumIdField(EnumIdFieldEnum)

}

class EnumIdFieldSpec extends FunSuite {

  test("EnumIdField should translate to query by id") {

    val meta = new EnumIdFieldSpecMeta

    assertEquals(
      meta.where(_.`enum` eqs EnumIdFieldEnum.one).toString,
      """db.enumidfieldclass.find({"enum": 0})"""
    )
  }
}
