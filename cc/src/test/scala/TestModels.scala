package me.sgrouples.rogue.cc
import java.time.{ Instant, LocalDateTime }
import java.util.UUID

import io.fsq.rogue.EnumerationListModifyField
import me.sgrouples.rogue._
import org.bson.types.ObjectId
import BsonFormats._
import me.sgrouples.rogue.naming.PluralLowerCase

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

case class VenueClaimBson(uid: Long, status: ClaimStatus.Value, source: Option[SourceBson] = None, date: LocalDateTime = LocalDateTime.now())

case class VenueClaim(_id: ObjectId, vid: ObjectId, uid: Long, status: ClaimStatus.Value, reason: Option[RejectReason.Value] = None, date: LocalDateTime = LocalDateTime.now())

case class Venue(_id: ObjectId, legId: Long, userId: Long, venuename: String, mayor: Long, mayor_count: Long, closed: Boolean, tags: List[String],
  popularity: List[Long], categories: List[ObjectId], last_updated: LocalDateTime, status: VenueStatus.Value, claims: List[VenueClaimBson],
  lastClaim: Option[VenueClaimBson])

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

  object VenueClaimBsonR extends RCcMeta[VenueClaimBson]("_") {
    val uid = new LongField("uid", this)
    val status = new EnumField[ClaimStatus.type, VenueClaimBsonR.type]("status", this)
    val source = new OptCClassField[SourceBson, SourceBsonR.type, VenueClaimBsonR.type]("source", SourceBsonR, this)
    val date = new LocalDateTimeField("date", this)
  }

  implicit val evVenueStatus = VenueStatus

  object VenueR extends RCcMeta[Venue](PluralLowerCase) {
    val id = new ObjectIdField("_id", this)
    val mayor = new LongField("mayor", this)
    val venuename = new StringField("venuename", this)
    val closed = new BooleanField("closed", this)
    val status = new EnumField[VenueStatus.type, VenueR.type]("status", this)
    val mayor_count = new LongField("mayor_count", this)
    val legacyid = new LongField("legId", this)
    val userid = new LongField("userId", this)
    val tags = new ListField[String, VenueR.type]("tags", this)
    val claims = new CClassListField[VenueClaimBson, VenueClaimBsonR.type, VenueR.type]("claims", VenueClaimBsonR, this)
    val lastClaim = new OptCClassField[VenueClaimBson, VenueClaimBsonR.type, VenueR.type]("lastClaim", VenueClaimBsonR, this)
    val last_updated = new LocalDateTimeField("last_updated", this)
    val popularity = new ListField[Long, VenueR.type]("popularity", this)
    val categories = new ListField[ObjectId, VenueR.type]("categories", this)
  }

  implicit val evRejReason = RejectReason

  object VenueClaimR extends RCcMeta[VenueClaim]("venueclaims") {
    val venueid = new ObjectIdField("vid", this)
    val status = new EnumField[ClaimStatus.type, VenueClaimR.type]("status", this)
    val reason = new OptEnumField[RejectReason.type, VenueClaimR.type]("reason", this)
    val userId = new LongField("uid", this)
  }

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

