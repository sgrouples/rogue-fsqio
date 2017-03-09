package me.sgrouples.rogue.cc

import java.time.Instant

import me.sgrouples.rogue.{ BsonFormats, EnumAnnotatedFormats, EnumSerializeValue }
import org.bson.types.ObjectId
import org.scalatest.{ FlatSpec, Matchers }
import shapeless.tag.@@

@EnumSerializeValue object Permission extends Enumeration {
  type Permission = Value
  val Post = Value(0, "post")
  val Comment = Value(1, "comment")
  val Reshare = Value(2, "reshare")
  val Invite = Value(3, "invite")
  val ReqModeration = Value(4, "req_moderation")
}

object GroupModelType extends Enumeration {
  type GroupModelType = Value

  val Private = Value(0, "Private")
  val Public = Value(1, "Public")
  val PublicApply = Value(2, "PublicApply")
  val Universal = Value(3, "Universal")
}

object RoleName extends Enumeration {
  type RoleName = Value

  val NotAssigned = Value("NotAssigned")
  val Owner = Value("Owner")
  val Admin = Value("Admin")
  val Contributor = Value("Contributor")
  val Viewer = Value("Viewer")
  val Limited = Value("Limited")
  val Custom = Value("Custom")
}

case class PDFeatureInfo(
  position: Int,
  isFeatured: Boolean
)

object Group {
  type Id = ObjectId @@ Group
}

object User {
  type Id = ObjectId @@ User
}

trait User

trait ThematicType

object Misc {
  type GroupThematicType = String @@ ThematicType
}

import Misc.GroupThematicType

case class Role(
  name: RoleName.Value,
  permissions: List[Permission.Value],
  origin: Option[RoleName.Value] = None
)

case class Group(
  _id: Group.Id,
  name: String,
  refPhotoId: Option[ObjectId],
  groupModelType: GroupModelType.Value,
  groupThematicType: String,
  userPk: User.Id,
  adminIds: List[ObjectId],
  publicUrlId: Option[String],
  color: String,
  description: Option[String],
  groupDefaultRoles: Seq[Role],
  membersCount: Long,
  publicDefaultRole: Option[Role],
  publicApplyQuestions: Option[List[String]],
  showInPublicDirectory: Option[Boolean],
  showGroupTweets: Boolean,
  lastGroupTweetsRefresh: Option[Instant],
  lastPostDate: Option[Instant],
  isReportedAsOffensive: Option[Boolean],
  isBannedAsOffensive: Option[Boolean],
  searchId: Option[String],
  pdFeaturedInfo: Option[PDFeatureInfo],
  createdAt: Instant
)

case class GroupModerationData(
  _id: Group.Id,
  name: String,
  description: String,
  groupModelType: GroupModelType.Value,
  userPk: User.Id,
  refPhotoId: Option[ObjectId],
  publicUrlId: String,
  showInPublicDirectory: Boolean,
  reportedDate: Instant,
  offensiveReporters: List[User.Id],
  isBannedAsOffensive: Boolean,
  isReportedAsOffensive: Boolean,
  isSpamDetected: Option[Boolean],
  spamResultReason: String,
  membersCount: Long
)

trait QueryByTaggedId[M, Tag] {
  requires: QueryFieldHelpers[M] =>

  val id = ObjectIdTaggedField[Tag]("_id")

}

trait QueryByUserPk[M] {
  requires: QueryFieldHelpers[M] =>

  val userId = ObjectIdTaggedField[User]("userPk")

}

trait QueryByGroupId[M] {
  requires: QueryFieldHelpers[M] =>

  val groupId = ObjectIdTaggedField[Group]

}

trait QueryByName[M] {
  requires: QueryFieldHelpers[M] =>

  val name = StringField

}

trait QueryByCreatedUpdatedAt[M] {
  requires: QueryFieldHelpers[M] =>

  val createdAt = InstantField
  val updatedAt = OptInstantField

}

trait QueryBySearchIndexerEvent[M] {
  requires: QueryFieldHelpers[M] =>

  val indexedAt = OptInstantField
  val isNotIndexed = OptBooleanField
}

class FrozenCompilationSpec extends FlatSpec with Matchers {

  import BsonFormats._
  import EnumAnnotatedFormats._
  implicit val evRoleName: RoleName.type = RoleName
  implicit val evPermission: Permission.type = Permission
  implicit val evGroupModelType: GroupModelType.type = GroupModelType

  class RoleMeta extends RCcMetaExt[Role, RoleMeta] {
    val name = EnumField(RoleName)
    val permissions = EnumListField(Permission)
    val origin = OptEnumField(RoleName)
  }

  class PDFeatureInfoMeta extends RCcMetaExt[PDFeatureInfo, PDFeatureInfoMeta] {

    val position = IntField
    val isFeatured = BooleanField
  }

  val PDFeatureInfos = new PDFeatureInfoMeta

  val Roles = new RoleMeta

  trait GroupMetaLike[M]
      extends QueryByTaggedId[M, Group]
      with QueryByUserPk[M]
      with QueryByCreatedUpdatedAt[M]
      with QueryByName[M]
      with QueryBySearchIndexerEvent[M] { requires: QueryFieldHelpers[M] =>

    val nameLC = StringField
    val searchId = StringField
    val publicUrlId = OptStringField
    val groupModelType = EnumField(GroupModelType)
    val color = StringField
    val description = StringField
    val refPhotoId = OptObjectIdField
    val groupDefaultRoles = ClassListField[Role, RoleMeta](Roles)
    val groupThematicType = StringField
    val membersCount = LongField
    val publicDefaultRole = OptClassField[Role, RoleMeta](Roles)
    val publicApplyQuestions = OptListField[String]
    val showInPublicDirectory = OptBooleanField
    val showGroupTweets = BooleanField
    val isReportedAsOffensive = BooleanField
    val reportedDate = OptInstantField
    val offsensiveReporters = OptListField[ObjectId]
    val isBannedAsOffensive = OptBooleanField
    val adminIds = ListField[ObjectId]
    val lastGroupTweetsRefresh = OptInstantField
    val isSpamDetected = OptBooleanField
    val spamResultReason = OptStringField
    val advancedEventsDisabled = OptBooleanField
    val lastPostDate = OptInstantField
    val informedAboutPublicDirectoryRemoval = OptBooleanField
    val pdFeatureInfo = OptClassField[PDFeatureInfo, PDFeatureInfoMeta](PDFeatureInfos)

    //val _publicUrlId = StringField("publicUrlId")
  }

  class GroupMeta extends RCcMetaExt[Group, GroupMeta]("privategroups")
    with GroupMetaLike[GroupMeta]

  class GroupWithModDataMeta extends RCcMetaExt[GroupModerationData, GroupWithModDataMeta]("privategroups")
    with GroupMetaLike[GroupWithModDataMeta]

  val Groups: GroupMeta = new GroupMeta

  val GroupsWithModData: GroupWithModDataMeta = new GroupWithModDataMeta

  "This spec" should "compile itself" in {
    1 shouldBe 1
  }
}
