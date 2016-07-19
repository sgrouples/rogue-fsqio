package me.sgrouples.rogue.cc
import java.time.LocalDateTime

import io.fsq.rogue.LatLong
import me.sgrouples.rogue._
import org.bson.types.ObjectId


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


case class SourceBson(name:String, url:String)

case class VenueClaimBson(uid: Long, status: ClaimStatus.Value, source: Option[SourceBson] = None, date: LocalDateTime = LocalDateTime.now())

case class VenueClaim(_id: ObjectId, uid:Long, status: ClaimStatus.Value, reason: Option[RejectReason.Value] = None, date:LocalDateTime = LocalDateTime.now())


case class Venue(_id: ObjectId, legId: Long, userId: Long, venuename: String, mayor: Long, mayor_count: Long, closed: Boolean, tags: List[String],
                 popularity: List[Long], categories: List[ObjectId], latlng: LatLong, last_updated: LocalDateTime, status: VenueStatus.Value, claims: List[VenueClaimBson],
                 lastClaim: VenueClaimBson)

case class Tip(_id: ObjectId, legid:Long, counts: Map[String, Long], userId:Option[Long] = None)

object Metas {

  import me.sgrouples.rogue.BsonFormats._

  implicit val evVenueStatus = VenueStatus

  object VenueR extends RCcMeta[Venue]("venues") {
    val id = new ObjectIdField("_id", this)
    val mayor = new LongField("mayor", this)
    val venuename = new StringField("venuename", this)
    val closed = new BooleanField("closed", this)
    val status = new EnumField[VenueStatus.type, VenueR.type]("status", this)
    val mayor_count = new LongField("mayor_count", this)
    val legacyid = new LongField("legId", this)
    val userid = new LongField("userId", this)
    val tags = new ListField[String, VenueR.type]("tags", this)

  }

  implicit val evClaimStatus = ClaimStatus
  implicit val evRejReason = RejectReason

  object VenueClaimR extends RCcMeta[VenueClaim]("venueclaims") {
    val venueid = new ObjectIdField("_id", this)
    val status = new EnumField[ClaimStatus.type, VenueClaimR.type]("status", this)
  }

  object TipR extends RCcMeta[Tip]("tips") {
    val id = new ObjectIdField("_id", this)
    val legacyid = new LongField("legid", this)

  }
}


