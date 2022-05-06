package me.sgrouples.rogue.naming

import me.sgrouples.rogue.cc.macros._
import munit.FunSuite

/** Created by mwielocha on 09/08/16.
  */
class NamingStrategySpec extends FunSuite {
  case class CaseClassWithVeryLongAwkwardName(id: Long)
  test("Naming Startegies") {
    assertEquals(
      new MCcMeta[CaseClassWithVeryLongAwkwardName, this.type].collectionName,
      "caseclasswithverylongawkwardname"
    )
    assertEquals(
      new MCcMeta[CaseClassWithVeryLongAwkwardName, this.type](LowerCase).collectionName,
      "caseclasswithverylongawkwardname"
    )
    assertEquals(
      new MCcMeta[CaseClassWithVeryLongAwkwardName, this.type](UpperCase).collectionName,
      "CASECLASSWITHVERYLONGAWKWARDNAME"
    )
    assertEquals(
      new MCcMeta[CaseClassWithVeryLongAwkwardName, this.type](SnakeCase).collectionName,
      "case_class_with_very_long_awkward_name"
    )
  }
}
