package me.sgrouples.rogue.macrotests

import java.time.Instant

import me.sgrouples.rogue.cc._
import me.sgrouples.rogue.cc.macros._
import org.bson.types.ObjectId
import org.scalatest.{ FlatSpec, Matchers }
import shapeless._

class MacroComplexCaseClassSpec extends FlatSpec with Matchers {

  class GroupMeta extends MCcMeta[Group, GroupMeta]("") {
  }

  val Groups = new GroupMeta

  "RCcMeta of a complex case class" should "properly read/write the given instance" in {

    val now = Instant.now

    val id = new ObjectId()

    val group = Group(
      id,
      "Group",
      None,
      GroupModelType.one,
      tag[AnyTag][String]("Ala"),
      tag[User][ObjectId](id),
      List(
        tag[User][ObjectId](id),
      tag[User][ObjectId](id)),
      None,
      "color", None,
      List(
        Role(
          RoleName.one,
          Set(Permission.one, Permission.two),
          origin = None),
        Role(
          RoleName.two,
          Set(Permission.two),
          origin = Some(RoleName.one))),
      List(0L, 1L, 2L),
      None, None,
      false, None,
      false, None,
      None, None,
      None, None,
      None, now,
      None)

    val bson = Groups.write(group)

    Groups.read(bson) shouldBe group
  }

}
