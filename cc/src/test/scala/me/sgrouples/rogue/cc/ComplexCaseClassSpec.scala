package me.sgrouples.rogue.cc

import java.time.Instant
import java.time.temporal.{ChronoUnit}

import org.bson.types.ObjectId
import munit.FunSuite
import shapeless.tag.@@
import shapeless.tag
import me.sgrouples.rogue.BsonFormats._
import me.sgrouples.rogue.EnumNameFormats._

trait User

object User {
  type Id = ObjectId @@ User
}

object GroupModelType extends Enumeration {
  type GroupModelType = Value
  val one = Value
  val two = Value
}

object RoleName extends Enumeration {
  type RoleName = Value
  val one = Value
  val two = Value
}

object Permission extends Enumeration {
  type Permission = Value
  val one = Value
  val two = Value
}

trait AnyTag

trait Team

object Team {
  type Id = ObjectId @@ Team
}

object Dependencies {
  type GroupThematicType = String @@ AnyTag
}

import Dependencies._

case class Role(
    name: RoleName.Value,
    permissions: Set[Permission.Value],
    origin: Option[RoleName.Value] = None
)

case class Group(
    id: ObjectId,
    name: String,
    photoId: Option[ObjectId],
    groupModelType: GroupModelType.Value,
    groupThematicType: GroupThematicType,
    ownerId: User.Id,
    adminIds: List[User.Id],
    publicUrlId: Option[String],
    color: String,
    description: Option[String],
    groupDefaultRoles: List[Role],
    membersCount: List[Long],
    inviteeDefaultRole: Option[Role],
    publicApplyQuestions: Option[List[String]],
    mandatoryQuestions: Boolean,
    showInPublicDirectory: Option[Boolean],
    showGroupTweets: Boolean,
    lastGroupTweetsRefresh: Option[Instant],
    lastPostDate: Option[Instant],
    reportedAsOffensive: Option[Boolean],
    bannedAsOffensive: Option[Boolean],
    searchId: Option[String],
    pdFeaturedPosition: Option[Int],
    createdAt: Instant,
    teamId: Option[Team.Id]
)

class ComplexCaseClassSpec extends FunSuite {

  class GroupMeta extends RCcMetaExt[Group, GroupMeta] {}

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
