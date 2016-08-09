package me.sgrouples.rogue.naming

import me.sgrouples.rogue.cc.RCcMeta
import org.scalatest.{ FlatSpec, MustMatchers }
import me.sgrouples.rogue.BsonFormats._

/**
 * Created by mwielocha on 09/08/16.
 */
class NamingStrategySpec extends FlatSpec with MustMatchers {

  case class CaseClassWithWierdName(id: Long)

  "Naming Startegies" should "produce proper names for case classes" in {

    new RCcMeta[CaseClassWithWierdName].collectionName mustBe "caseclasswithwierdname"
    new RCcMeta[CaseClassWithWierdName](LowerCase).collectionName mustBe "caseclasswithwierdname"
    new RCcMeta[CaseClassWithWierdName](UpperCase).collectionName mustBe "CASECLASSWITHWIERDNAME"
    new RCcMeta[CaseClassWithWierdName](SnakeCase).collectionName mustBe "case_class_with_wierd_name"
  }
}
