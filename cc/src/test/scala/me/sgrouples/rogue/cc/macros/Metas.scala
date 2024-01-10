package me.sgrouples.rogue.cc.macros

import com.softwaremill.tagging._
import io.fsq.rogue.index.{Asc, Desc, IndexBuilder}
import me.sgrouples.rogue.cc.*
import me.sgrouples.rogue.cc.CcRogue.{given, *}
import me.sgrouples.rogue.naming.PluralLowerCase
import org.bson.types.ObjectId
import com.softwaremill.tagging.*

import java.time.Instant
import java.util.{Currency, Locale, UUID}

case class UuidCc(_id: UUID, s: String, i: Instant = Instant.now())

object Metas {

  class SourceBsonMeta extends MCcMeta[SourceBson, SourceBsonMeta]("_") {
    val name = StringField("name")
    val url = StringField("url")
  }
  val SourceBsonR = new SourceBsonMeta

  class VenueClaimBsonRMeta
      extends MCcMeta[VenueClaimBson, VenueClaimBsonRMeta]("_") {
    val uid = LongField("uid")
    val status = EnumField("status", ClaimStatus)
    val source = OptClassField("source", SourceBsonR)
    val date = LocalDateTimeField("date")
  }

  val VenueClaimBsonR = new VenueClaimBsonRMeta

  class VenueRMeta
      extends MCcMeta[Venue, VenueRMeta](PluralLowerCase)
      with IndexBuilder[VenueRMeta] {
    val id = ObjectIdTaggedField[Venue]("_id")
    val mayor = LongField("mayor")
    val venuename = StringField("venuename")
    val closed = BooleanField("closed")
    val status = EnumField("status", VenueStatus)
    val mayor_count = LongField("mayor_count")
    val legacyid = LongField("legId")
    val userId = LongField("userId")
    val tags = ListField[String]("tags")

    val claims =
      ClassListField[VenueClaimBson, VenueClaimBsonRMeta](
        "claims",
        VenueClaimBsonR
      )
    val lastClaim =
      OptClassField[VenueClaimBson, VenueClaimBsonRMeta](
        "lastClaim",
        VenueClaimBsonR
      )
    val firstClaim =
      ClassRequiredField("firstClaim", VenueClaimBsonR, VenueClaimBson.default)

    val last_updated = LocalDateTimeField("last_updated")
    val popularity = VectorField[Long]("popularity")
    val categories = ListField[ObjectId]("categories")

    val idIdx = index(id, Asc)
    val legIdx = index(legacyid, Desc)
    val legIdIdx = index(legacyid, Asc, id, Desc)
  }

  val VenueR = new VenueRMeta
  class VenueClaimRMeta
      extends MCcMeta[VenueClaim, VenueClaimRMeta]("venueclaims") {
    val venueid = ObjectIdSubtypeField[ObjectId @@ Venue]("vid")
    val status = EnumField("status", ClaimStatus)
    val reason = OptEnumField("reason", RejectReason)
    val userId = LongField("uid")
  }

  val VenueClaimR = new VenueClaimRMeta

  class TipMeta extends MCcMeta[Tip, TipMeta]("tips") {
    val id = ObjectIdField("_id")
    val legacyid = LongField("legid")
    val userId = OptLongField("userId")
    val counts = MapField[String, Long]("counts")
  }

  val TipR = new TipMeta

  class OAuthConsumerMeta
      extends MCcMeta[OAuthConsumer, OAuthConsumerMeta]("oauthconsumers") {
    val privileges = ListField[ConsumerPrivilege.Value]("privileges")
  }

  val OAuthConsumerR = new OAuthConsumerMeta

  class CommentMeta extends MCcMeta[Comment, CommentMeta]("comments") {
    val comments = ListField[OneComment]("comments")
  }

  val CommentR = new CommentMeta

  class OptValCCMeta extends MCcMeta[OptValCC, OptValCCMeta]("optvalcc") {
    val id = ObjectIdField("_id")
    val ms = OptStringField("maybes")
    val mi = OptObjectIdField("maybeid")
    val rs = StringField("realString")
  }
  val OptValCCR = new OptValCCMeta

  class UuidCcMeta extends MCcMeta[UuidCc, UuidCcMeta]("uuidcc") {
    val id = UUIdField("_id")
    val s = StringField("s")
    val dt = InstantField("i")
  }
  val UuidCcR = new UuidCcMeta

  case class Money(amount: BigDecimal, currency: Currency)

  case class Invoice(id: Long, name: String, total: Money)

  class MoneyMeta extends MCcMeta[Money, MoneyMeta] {
    val amount = BigDecimalField("amount")
    val currency = CurrencyField("currency")
  }

  class InvoiceMeta extends MCcMeta[Invoice, InvoiceMeta] {
    val id = LongField("id")
    val name = StringField("name")
    val total = ClassField[Money, MoneyMeta]("total", new MoneyMeta)
  }

  val Invoices = new InvoiceMeta

  case class MCounter(
      _id: ObjectId = ObjectId.get(),
      counts: Map[ObjectId, Long]
  )

  class CounterMeta extends MCcMeta[MCounter, CounterMeta] {
    val id = ObjectIdField("_id")
    val counts = MapField[ObjectId, Long]("counts")
  }

  val Counters = new CounterMeta

  type CounterId = ObjectId @@ MCounter

  case class TypedCounter(
      _id: ObjectId = ObjectId.get(),
      counts: Map[CounterId, Long]
  )

  class TypedCounterMeta extends MCcMeta[TypedCounter, TypedCounterMeta] {
    val id = ObjectIdField("_id")
    val counts = MapField[CounterId, Long]("counts")
  }

  val TypedCounters = new TypedCounterMeta

  case class BinaryData(data: Array[Byte])

  class BinaryDataMeta extends MCcMeta[BinaryData, BinaryDataMeta] {}

  val Binaries = new BinaryDataMeta

  case class LocaleData(locale: Locale)

  class LocaleDataMeta extends MCcMeta[LocaleData, LocaleDataMeta] {
    val locale = LocaleField("locale")
  }

  val Locales = new LocaleDataMeta
}
