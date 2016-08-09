package me.sgrouples.rogue.naming

import me.sgrouples.rogue.cc.RCcMeta
import org.scalatest.{ FlatSpec, MustMatchers }
import me.sgrouples.rogue.BsonFormats._

/**
 * Created by mwielocha on 09/08/16.
 */
class NamingStrategySpec extends FlatSpec with MustMatchers {

  case class CaseClassWithVeryLongAwkwardName(id: Long)

  "Naming Startegies" should "produce proper names for case classes" in {

    new RCcMeta[CaseClassWithVeryLongAwkwardName].collectionName mustBe "caseclasswithverylongawkwardname"
    new RCcMeta[CaseClassWithVeryLongAwkwardName](LowerCase).collectionName mustBe "caseclasswithverylongawkwardname"
    new RCcMeta[CaseClassWithVeryLongAwkwardName](UpperCase).collectionName mustBe "CASECLASSWITHVERYLONGAWKWARDNAME"
    new RCcMeta[CaseClassWithVeryLongAwkwardName](SnakeCase).collectionName mustBe "case_class_with_very_long_awkward_name"
  }
}
