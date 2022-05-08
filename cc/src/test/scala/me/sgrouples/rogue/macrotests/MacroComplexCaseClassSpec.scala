package me.sgrouples.rogue.macrotests

import java.time.Instant
import java.time.temporal.ChronoUnit

import me.sgrouples.rogue.cc._
import me.sgrouples.rogue.cc.macros._
import org.bson.types.ObjectId
import munit.FunSuite
import me.sgrouples.rogue.tags.*

class MacroComplexCaseClassSpec extends FunSuite {

  class GroupMeta extends MCcMeta[Group, GroupMeta]("") {}

  val Groups = new GroupMeta

  test(
    "RCcMeta of a complex case class should properly read/write the given instance"
  ) {

    val now = Instant.now.truncatedTo(ChronoUnit.MILLIS)
    val id = new ObjectId()

    val group = Group(
      id,
      "Group",
      None,
      GroupModelType.one,
      tag[AnyTag][String]("Ala"),
      tag[User][ObjectId](id),
      List(tag[User][ObjectId](id), tag[User][ObjectId](id)),
      None,
      "color",
      None,
      List(
        Role(RoleName.one, Set(Permission.one, Permission.two), origin = None),
        Role(RoleName.two, Set(Permission.two), origin = Some(RoleName.one))
      ),
      List(0L, 1L, 2L),
      None,
      None,
      false,
      None,
      false,
      None,
      None,
      None,
      None,
      None,
      None,
      now,
      None
    )

    val bson = Groups.write(group)

    assertEquals(Groups.read(bson), group)
  }

}
