package me.sgrouples.rogue.cc
import java.time.{ Instant, LocalDateTime }
import java.util.UUID

import io.fsq.rogue.EnumerationListModifyField
import me.sgrouples.rogue._
import org.bson.types.ObjectId
import BsonFormats._
import EnumNameFormats._
import me.sgrouples.rogue.naming.PluralLowerCase
import shapeless.tag.@@
import shapeless.tag

object VenueStatus extends Enumeration {
  val open = Value("Open")
  val closed = Value("Closed")
}
object ClaimStatus extends Enumeration {
  val pending = Value("Pending approval")
  val approved = Value("Approved")
}

object RejectReason extends Enumeration {
  val tooManyClaims = Value("too many claims")
  val cheater = Value("cheater")
  val wrongCode = Value("wrong code")
}

case class V1(legacyid: Long)

case class V2(legacyid: Long, userid: Long)

case class V3(legacyid: Long, userid: Long, mayor: Long)

case class V4(legacyid: Long, userid: Long, mayor: Long, mayor_count: Long)

case class V5(legacyid: Long, userid: Long, mayor: Long, mayor_count: Long, closed: Boolean)

case class V6(legacyid: Long, userid: Long, mayor: Long, mayor_count: Long, closed: Boolean, tags: List[String])

case class SourceBson(name: String, url: String)

object VenueClaimBson {
  val default = VenueClaimBson(
    -1L, ClaimStatus.pending
  )
}

case class VenueClaimBson(
  uid: Long,
  status: ClaimStatus.Value,
  source: Option[SourceBson] = None,
  date: LocalDateTime = LocalDateTime.now(),
  otherReasons: List[RejectReason.Value] = Nil,
  optOtherReasons: Option[List[RejectReason.Value]] = None
)

object VenueClaim {
  def newId = tag[VenueClaim](new ObjectId())
}

case class VenueClaim(
  _id: ObjectId @@ VenueClaim,
  vid: ObjectId,
  uid: Long,
  status: ClaimStatus.Value,
  reason: Option[RejectReason.Value] = None,
  date: LocalDateTime = LocalDateTime.now(),
  otherReasons: Option[List[RejectReason.Value]] = None
)

object Venue {
  def newId = tag[Venue](new ObjectId())
}

case class Venue(
  _id: ObjectId @@ Venue,
  legId: Long,
  userId: Long,
  venuename: String,
  mayor: Long,
  mayor_count: Long,
  closed: Boolean,
  tags: List[String],
  popularity: List[Long],
  categories: List[ObjectId],
  last_updated: LocalDateTime,
  status: VenueStatus.Value,
  claims: List[VenueClaimBson],
  lastClaim: Option[VenueClaimBson],
  firstClaim: VenueClaimBson = VenueClaimBson.default
)

case class Tip(_id: ObjectId, legid: Long, counts: Map[String, Long], userId: Option[Long] = None)

case class OneComment(timestamp: String, userid: Long, comment: String)

case class Comment(comments: List[OneComment])

object ConsumerPrivilege extends Enumeration {
  val awardBadges = Value("Award badges")
}

case class OAuthConsumer(privileges: List[ConsumerPrivilege.Value])

case class OptValCC(_id: ObjectId = new ObjectId(), maybes: Option[String] = None, maybeid: Option[ObjectId] = None, realString: String)

object Metas {

  object SourceBsonR extends RCcMeta[SourceBson]("_") {
    val name = new StringField("name", this)
    val url = new StringField("url", this)
  }

  implicit val evClaimStatus = ClaimStatus
  implicit val evRejectReason = RejectReason

  class VenueClaimBsonRMeta extends RCcMeta[VenueClaimBson]("_") with QueryFieldHelpers[VenueClaimBsonRMeta] {
    val uid = LongField("uid")
    val status = EnumField[ClaimStatus.type]("status")
    val source = OptClassField[SourceBson, SourceBsonR.type]("source", SourceBsonR)
    val date = LocalDateTimeField("date")
    val otherReasons = EnumListField[RejectReason.type]("otherReasons")(evRejectReason)
    val optOtherReasons = OptEnumListField[RejectReason.type]("optOtherReasons")(evRejectReason)
  }

  val VenueClaimBsonR = new VenueClaimBsonRMeta

  implicit val evVenueStatus = VenueStatus

  class VenueRMeta extends RCcMetaExt[Venue, VenueRMeta](PluralLowerCase) {

    val id = ObjectIdTaggedField[Venue]("_id")
    val mayor = LongField
    val venuename = StringField
    val closed = BooleanField
    val status = EnumField[VenueStatus.type]
    val mayor_count = LongField
    val legacyid = LongField("legId")
    val userId = LongField
    val tags = ListField[String]

    val claims = ClassListField[VenueClaimBson, VenueClaimBsonRMeta](VenueClaimBsonR)
    val lastClaim = OptClassField[VenueClaimBson, VenueClaimBsonRMeta](VenueClaimBsonR)
    val firstClaim = ClassRequiredField(VenueClaimBsonR, VenueClaimBson.default)

    val last_updated = LocalDateTimeField
    val popularity = ListField[Long]
    val categories = ListField[ObjectId]
  }

  val VenueR = new VenueRMeta

  class VenueClaimRMeta extends RCcMeta[VenueClaim]("venueclaims") with QueryFieldHelpers[VenueClaimRMeta] {
    val venueid = ObjectIdTaggedField[Venue]("vid")
    val status = EnumField[ClaimStatus.type]
    val reason = OptEnumField[RejectReason.type]
    val userId = LongField("uid")
  }

  val VenueClaimR = new VenueClaimRMeta

  object TipR extends RCcMeta[Tip]("tips") {
    val id = new ObjectIdField("_id", this)
    val legacyid = new LongField("legid", this)
    val userId = new OptLongField("userId", this)
    val counts = new MapField[Long, TipR.type]("counts", this)
  }

  implicit val consumerPrivilegeV = ConsumerPrivilege
  object OAuthConsumerR extends RCcMeta[OAuthConsumer]("oauthconsumers") {
    val privileges = new ListField[ConsumerPrivilege.Value, OAuthConsumerR.type]("privileges", this)
  }

  object CommentR extends RCcMeta[Comment]("comments") {
    val comments = new ListField[OneComment.type, CommentR.type]("comments", this)
  }

  object OptValCCR extends RCcMeta[OptValCC]("optvalcc") {
    val id = new ObjectIdField("_id", this)
    val ms = new OptStringField("maybes", this)
    val mi = new OptObjectIdField("maybeid", this)
    val rs = new StringField("realString", this)
  }

  case class UuidCc(_id: UUID, s: String, i: Instant = Instant.now())
  object UuidCcR extends RCcMeta[UuidCc]("uuidcc") {
    val id = new UUIDIdField("_id", this)
    val s = new StringField("s", this)
    val dt = new InstantField("i", this)
  }

}

