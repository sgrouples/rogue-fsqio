package me.sgrouples.rogue.naming

import me.sgrouples.rogue.cc.macros._
import me.sgrouples.rogue.cc.macros.MacroBsonFormatAuto.*
import munit.FunSuite

/** Created by mwielocha on 09/08/16.
  */
class NamingStrategySpec extends FunSuite {
  case class CaseClassWithVeryLongAwkwardName(id: Long)
  
  object DefaultMeta extends MCcMeta[CaseClassWithVeryLongAwkwardName, DefaultMeta.type]
  object LowerCaseMeta extends MCcMeta[CaseClassWithVeryLongAwkwardName, LowerCaseMeta.type](LowerCase)
  object UpperCaseMeta extends MCcMeta[CaseClassWithVeryLongAwkwardName, UpperCaseMeta.type](UpperCase)
  object SnakeCaseMeta extends MCcMeta[CaseClassWithVeryLongAwkwardName, SnakeCaseMeta.type](SnakeCase)

  test("Naming Startegies") {
    assertEquals(
      DefaultMeta.collectionName,
      "caseclasswithverylongawkwardname"
    )
    assertEquals(
      LowerCaseMeta.collectionName,
      "caseclasswithverylongawkwardname"
    )
    assertEquals(
      UpperCaseMeta.collectionName,
      "CASECLASSWITHVERYLONGAWKWARDNAME"
    )
    assertEquals(
      SnakeCaseMeta.collectionName,
      "case_class_with_very_long_awkward_name"
    )
  }
}
