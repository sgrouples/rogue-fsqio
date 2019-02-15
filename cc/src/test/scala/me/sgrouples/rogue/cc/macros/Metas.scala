package me.sgrouples.rogue.cc.macros

import java.time.Instant
import java.util.{ Currency, Locale, UUID }

import io.fsq.rogue.index.{ Asc, Desc, IndexBuilder }
import me.sgrouples.rogue.cc._
import me.sgrouples.rogue.naming.PluralLowerCase
import org.bson.types.ObjectId
import shapeless.tag.@@

case class UuidCc(_id: UUID, s: String, i: Instant = Instant.now())

object Metas {

  class SourceBsonMeta extends MCcMeta[SourceBson, SourceBsonMeta]("_") {
    @f val name = StringField
    @f val url = StringField
  }
  val SourceBsonR = new SourceBsonMeta

  class VenueClaimBsonRMeta extends MCcMeta[VenueClaimBson, VenueClaimBsonRMeta]("_") {
    @f val uid = LongField
    @f val status = EnumField(ClaimStatus)
    @f val source = OptClassField[SourceBson, SourceBsonMeta](SourceBsonR)
    val date = LocalDateTimeField("date")
  }

  val VenueClaimBsonR = new VenueClaimBsonRMeta

  class VenueRMeta extends MCcMeta[Venue, VenueRMeta](PluralLowerCase) with IndexBuilder[VenueRMeta] {
    val id = ObjectIdTaggedField[Venue]("_id")
    @f val mayor = LongField
    @f val venuename = StringField
    @f val closed = BooleanField
    @f val status = EnumField(VenueStatus)
    @f val mayor_count = LongField
    val legacyid = LongField("legId")
    @f val userId = LongField
    @f val tags = ListField[String]

    @f val claims = ClassListField[VenueClaimBson, VenueClaimBsonRMeta](VenueClaimBsonR)
    @f val lastClaim = OptClassField[VenueClaimBson, VenueClaimBsonRMeta](VenueClaimBsonR)
    @f val firstClaim = ClassRequiredField(VenueClaimBsonR, VenueClaimBson.default)

    @f val last_updated = LocalDateTimeField
    @f val popularity = ListField[Long]
    @f val categories = ListField[ObjectId]

    val idIdx = index(id, Asc)
    val legIdx = index(legacyid, Desc)
    val legIdIdx = index(legacyid, Asc, id, Desc)
  }

  val VenueR = new VenueRMeta
  class VenueClaimRMeta extends MCcMeta[VenueClaim, VenueClaimRMeta]("venueclaims") {
    @f val venueid = ObjectIdTaggedField[Venue]("vid")
    @f val status = EnumField[ClaimStatus.type]
    @f val reason = OptEnumField[RejectReason.type]
    @f val userId = LongField("uid")
  }

  val VenueClaimR = new VenueClaimRMeta

  class TipMeta extends MCcMeta[Tip, TipMeta]("tips") {
    @f val id = ObjectIdField("_id")
    @f val legacyid = LongField("legid")
    @f val userId = OptLongField("userId")
    @f val counts = MapField[String, Long]("counts")
  }

  val TipR = new TipMeta

  class OAuthConsumerMeta extends MCcMeta[OAuthConsumer, OAuthConsumerMeta]("oauthconsumers") {
    @f val privileges = ListField[ConsumerPrivilege.Value]
  }

  val OAuthConsumerR = new OAuthConsumerMeta

  class CommentMeta extends MCcMeta[Comment, CommentMeta]("comments") {
    @f val comments = ListField[OneComment]
  }

  val CommentR = new CommentMeta

  class OptValCCMeta extends MCcMeta[OptValCC, OptValCCMeta]("optvalcc") {
    @f val id = ObjectIdField("_id")
    @f val ms = OptStringField("maybes")
    @f val mi = OptObjectIdField("maybeid")
    @f val rs = StringField("realString")
  }
  val OptValCCR = new OptValCCMeta

  class UuidCcMeta extends MCcMeta[UuidCc, UuidCcMeta]("uuidcc") {
    @f val id = UUIdField("_id")
    @f val s = StringField
    @f val dt = InstantField("i")
  }
  val UuidCcR = new UuidCcMeta

  case class Money(amount: BigDecimal, currency: Currency)

  case class Invoice(id: Long, name: String, total: Money)

  class MoneyMeta extends MCcMeta[Money, MoneyMeta] {
    @f val amount = BigDecimalField
    @f val currency = CurrencyField
  }

  class InvoiceMeta extends MCcMeta[Invoice, InvoiceMeta] {
    @f val id = LongField
    @f val name = StringField
    @f val total = ClassField[Money, MoneyMeta](new MoneyMeta)
  }

  val Invoices = new InvoiceMeta

  case class MCounter(_id: ObjectId = ObjectId.get(), counts: Map[ObjectId, Long])

  class CounterMeta extends MCcMeta[MCounter, CounterMeta] {
    @f val id = ObjectIdField("_id")
    @f val counts = MapField[ObjectId, Long]
  }

  val Counters = new CounterMeta

  type CounterId = ObjectId @@ MCounter

  case class TypedCounter(_id: ObjectId = ObjectId.get(), counts: Map[CounterId, Long])

  class TypedCounterMeta extends MCcMeta[TypedCounter, TypedCounterMeta] {
    @f val id = ObjectIdField("_id")
    @f val counts = MapField[CounterId, Long]
  }

  val TypedCounters = new TypedCounterMeta

  case class BinaryData(data: Array[Byte])

  class BinaryDataMeta extends MCcMeta[BinaryData, BinaryDataMeta] {}

  val Binaries = new BinaryDataMeta

  case class LocaleData(locale: Locale)

  class LocaleDataMeta extends MCcMeta[LocaleData, LocaleDataMeta] {
    @f val locale = LocaleField
  }

  val Locales = new LocaleDataMeta
}

