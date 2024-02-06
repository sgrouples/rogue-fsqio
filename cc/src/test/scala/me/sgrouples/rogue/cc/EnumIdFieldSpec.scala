package me.sgrouples.rogue.cc

import me.sgrouples.rogue.EnumSerializeValue
import munit.FunSuite
import CcRogue._
import me.sgrouples.rogue.cc.macros.*
import me.sgrouples.rogue.cc.CcRogue.*
//import me.sgrouples.rogue.EnumAnnotatedFormats._

@EnumSerializeValue
object EnumIdFieldEnum extends Enumeration {
  val one: Value = Value
  val two: Value = Value
  given MacroBsonFormat[EnumIdFieldEnum.Value] = EnumMacroFormats.enumValueMacroFormat(EnumIdFieldEnum)
}

case class EnumIdFieldClass(`enum`: EnumIdFieldEnum.Value)

class EnumIdFieldSpecMeta
    extends MCcMeta[EnumIdFieldClass, EnumIdFieldSpecMeta] {

  val `enum` = OptEnumIdField("enum", EnumIdFieldEnum)

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
