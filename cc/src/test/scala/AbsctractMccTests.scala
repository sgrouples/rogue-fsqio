package me.sgrouples.rogue.cc

import me.sgrouples.rogue.cc.macros._
import org.scalatest.{ FlatSpec, Matchers }

import me.sgrouples.rogue.cc.MFInstance._

case class MoneyHolder(m: Money)

class AbsctractMccTests extends FlatSpec with Matchers {

  "cc test" should "compile" in {
    implicit val mf = new MoneyFormat

    class MoneyHolderMeta extends MCcMeta[MoneyHolder, MoneyHolderMeta]("")
    val mh = new MoneyHolderMeta
  }
}
