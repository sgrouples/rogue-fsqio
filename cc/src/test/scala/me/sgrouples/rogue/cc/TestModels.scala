package me.sgrouples.rogue.cc
import io.fsq.rogue.index.{Asc, Desc, IndexBuilder, Text}
import me.sgrouples.rogue.BsonFormats._
import me.sgrouples.rogue.EnumNameFormats._
import me.sgrouples.rogue._
import me.sgrouples.rogue.naming.PluralLowerCase
import org.bson.types.ObjectId
import com.softwaremill.tagging._

import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalDateTime}
import java.util.{Currency, Locale, UUID}

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

case class V5(
    legacyid: Long,
    userid: Long,
    mayor: Long,
    mayor_count: Long,
    closed: Boolean
)

case class V6(
    legacyid: Long,
    userid: Long,
    mayor: Long,
    mayor_count: Long,
    closed: Boolean,
    tags: List[String]
)

case class SourceBson(name: String, url: String)

object VenueClaimBson {
  val default = VenueClaimBson(-1L, ClaimStatus.pending)
}

case class VenueClaimBson(
    uid: Long = -1L,
    status: ClaimStatus.Value = ClaimStatus.pending,
    source: Option[SourceBson] = None,
    date: LocalDateTime = LocalDateTime.now().truncatedTo(ChronoUnit.MILLIS)
)

object VenueClaim {
  def newId = new ObjectId().taggedWith[VenueClaim]
}

case class VenueClaim(
    _id: ObjectId @@ VenueClaim,
    vid: ObjectId,
    uid: Long,
    status: ClaimStatus.Value,
    reason: Option[RejectReason.Value] = None,
    date: LocalDateTime = LocalDateTime.now().truncatedTo(ChronoUnit.MILLIS)
)

object Venue {
  def newId = new ObjectId().taggedWith[Venue]
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

case class Tip(
    _id: ObjectId,
    legid: Long,
    counts: Map[String, Long],
    userId: Option[Long] = None
)

case class OneComment(timestamp: String, userid: Long, comment: String)

case class Comment(comments: List[OneComment])

object ConsumerPrivilege extends Enumeration {
  val awardBadges = Value("Award badges")
}

case class OAuthConsumer(privileges: List[ConsumerPrivilege.Value])

case class OptValCC(
    _id: ObjectId = new ObjectId(),
    maybes: Option[String] = None,
    maybeid: Option[ObjectId] = None,
    realString: String
)

object Metas {

  object SourceBsonR extends RCcMeta[SourceBson]("_") {
    val name = new StringField("name", this)
    val url = new StringField("url", this)
  }

  class VenueClaimBsonRMeta
      extends RCcMeta[VenueClaimBson]("_")
      with QueryFieldHelpers[VenueClaimBsonRMeta]
      with RuntimeNameResolver[VenueClaimBsonRMeta] {
    val uid = LongField("uid")
    val status = EnumField("status", ClaimStatus)
    val source =
      OptClassField[SourceBson, SourceBsonR.type]("source", SourceBsonR)
    val date = LocalDateTimeField("date")
  }

  val VenueClaimBsonR = new VenueClaimBsonRMeta

  /*
  VenueR.where(_.legacyid eqs 1).hint(VenueR.idIdx).toString()        must_== """db.venues.find({ "legId" : 1 }).hint({ "_id" : 1 })"""
      VenueR.where(_.legacyid eqs 1).hint(VenueR.legIdx).toString()       must_== """db.venues.find({ "legId" : 1 }).hint({ "legid" : -1 })"""
      VenueR.where(_.legacyid eqs 1).hint(VenueR.geoIdx).toString()       must_== """db.venues.find({ "legId" : 1 }).hint({ "latlng" : "2d" })"""
      VenueR.where(_.legacyid eqs 1).hint(VenueR.geoCustomIdx).toString() must_== """db.venues.find({ "legId" : 1 }).hint({ "latlng" : "custom", "tags" : 1 })"""

   */
  class VenueRMeta
      extends RCcMetaExt[Venue, VenueRMeta](PluralLowerCase)
      with IndexBuilder[VenueRMeta] {

    val id = ObjectIdTaggedField[Venue]("_id")
    val mayor = LongField
    val venuename = StringField
    val closed = BooleanField
    val status = EnumField(VenueStatus)
    val mayor_count = LongField
    val legacyid = LongField("legId")
    val userId = LongField
    val tags = ListField[String]

    val claims =
      ClassListField[VenueClaimBson, VenueClaimBsonRMeta](VenueClaimBsonR)
    val lastClaim =
      OptClassField[VenueClaimBson, VenueClaimBsonRMeta](VenueClaimBsonR)
    val firstClaim = ClassRequiredField(VenueClaimBsonR, VenueClaimBson.default)

    val last_updated = LocalDateTimeField
    val popularity = ListField[Long]
    val categories = ListField[ObjectId]

    val idIdx = index(id, Asc)
    val legIdx = index(legacyid, Desc)
    val legIdIdx = index(legacyid, Asc, id, Desc)
    val venuenameIdx = index(legacyid, Text)
  }

  val VenueR = new VenueRMeta

  class VenueClaimRMeta
      extends RCcMeta[VenueClaim]("venueclaims")
      with QueryFieldHelpers[VenueClaimRMeta]
      with RuntimeNameResolver[VenueClaimRMeta] {
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
    val counts = new MapField[String, Long, TipR.type]("counts", this)
  }

  object OAuthConsumerR extends RCcMeta[OAuthConsumer]("oauthconsumers") {
    val privileges =
      new ListField[ConsumerPrivilege.Value, OAuthConsumerR.type](
        "privileges",
        this
      )
  }

  object CommentR extends RCcMeta[Comment]("comments") {
    val comments =
      new ListField[OneComment.type, CommentR.type]("comments", this)
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

  case class Money(amount: BigDecimal, currency: Currency)

  case class Invoice(id: Long, name: String, total: Money)

  class MoneyMeta extends RCcMetaExt[Money, MoneyMeta] {
    val amount = BigDecimalField
    val currency = CurrencyField
  }

  class InvoiceMeta extends RCcMetaExt[Invoice, InvoiceMeta] {
    val id = LongField
    val name = StringField
    val total = ClassField[Money, MoneyMeta](new MoneyMeta)
  }

  val Invoices = new InvoiceMeta

  case class Counter(
      _id: ObjectId = ObjectId.get(),
      counts: Map[ObjectId, Long]
  )

  class CounterMeta extends RCcMetaExt[Counter, CounterMeta] {
    val id = ObjectIdField("_id")
    val counts = MapField[ObjectId, Long]
  }

  val Counters = new CounterMeta

  type CounterId = ObjectId @@ Counter

  case class TypedCounter(
      _id: ObjectId = ObjectId.get(),
      counts: Map[CounterId, Long]
  )

  class TypedCounterMeta extends RCcMetaExt[TypedCounter, TypedCounterMeta] {
    val id = ObjectIdField("_id")
    val counts = MapField[CounterId, Long]
  }

  val TypedCounters = new TypedCounterMeta

  case class BinaryData(data: Array[Byte])

  class BinaryDataMeta extends RCcMetaExt[BinaryData, BinaryDataMeta] {}

  val Binaries = new BinaryDataMeta

  case class LocaleData(locale: Locale)

  class LocaleDataMeta extends RCcMetaExt[LocaleData, LocaleDataMeta] {
    val locale = LocaleField
  }

  val Locales = new LocaleDataMeta
}
