package me.sgrouples.rogue.naming

import me.sgrouples.rogue.cc.RCcMeta
import munit.FunSuite
import me.sgrouples.rogue.BsonFormats._

/**
 * Created by mwielocha on 09/08/16.
 */
class NamingStrategySpec extends FunSuite {
  case class CaseClassWithVeryLongAwkwardName(id: Long)
  test("Naming Startegies") {
    assertEquals(new RCcMeta[CaseClassWithVeryLongAwkwardName].collectionName, "caseclasswithverylongawkwardname")
    assertEquals(new RCcMeta[CaseClassWithVeryLongAwkwardName](LowerCase).collectionName, "caseclasswithverylongawkwardname")
    assertEquals(new RCcMeta[CaseClassWithVeryLongAwkwardName](UpperCase).collectionName, "CASECLASSWITHVERYLONGAWKWARDNAME")
    assertEquals(new RCcMeta[CaseClassWithVeryLongAwkwardName](SnakeCase).collectionName, "case_class_with_very_long_awkward_name")
  }
}
