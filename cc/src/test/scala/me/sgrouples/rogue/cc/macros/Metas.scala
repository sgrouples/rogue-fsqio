package me.sgrouples.rogue.cc.macros

import java.math.BigInteger
import java.time.Instant
import java.util.{ Currency, Locale, UUID }

import me.sgrouples.rogue.cc._
import me.sgrouples.rogue.naming.PluralLowerCase
import org.bson.types.ObjectId
import shapeless.tag.@@
import me.sgrouples.rogue.cc.macros.MacroCC._
import org.bson.{ BsonValue, BsonWriter }
import me.sgrouples.rogue.map.MapKeyFormats._
import scala.language.experimental.macros

case class UuidCc(_id: UUID, s: String, i: Instant = Instant.now())

object Metas {
  implicit object BigIntegerMF extends BaseBsonFormat[BigInteger] {

    override def append(writer: BsonWriter, k: String, v: BigInteger): Unit = ???

    override def append(writer: BsonWriter, v: BigInteger): Unit = ???

    override def read(b: BsonValue): BigInteger = ???

    override def write(t: BigInteger): BsonValue = ???

    override def defaultValue: BigInteger = ???
  }

  val bfx = implicitly[MacroBsonFormat[BigInteger]]
  println(s"BFX ${bfx}")

  class SourceBsonMeta extends MCcMeta[SourceBson, SourceBsonMeta]("_") {
    val name = StringField
    val url = StringField
  }
  val SourceBsonR = new SourceBsonMeta

  class VenueClaimBsonRMeta extends MCcMeta[VenueClaimBson, VenueClaimBsonRMeta]("_") {
    val uid = LongField
    val status = EnumField(ClaimStatus)
    val source = OptClassField[SourceBson, SourceBsonMeta](SourceBsonR)
    val date = LocalDateTimeField("date")
  }

  val VenueClaimBsonR = new VenueClaimBsonRMeta

  class VenueRMeta extends MCcMeta[Venue, VenueRMeta](PluralLowerCase) {
    val id = ObjectIdTaggedField[Venue]("_id")
    println("Mayor")
    @f val mayor = LongField
    println("venuename")
    @f val venuename = StringField
    println("closed")
    @f val closed = BooleanField
    println("status")
    val status = EnumField(VenueStatus)
    println("mayor_count")
    @f val mayor_count = LongField
    println("legacyid")
    val legacyid = LongField("legId")
    @f val userId = LongField
    @f val tags = ListField[String]

    val claims = ClassListField[VenueClaimBson, VenueClaimBsonRMeta](VenueClaimBsonR)
    val lastClaim = OptClassField[VenueClaimBson, VenueClaimBsonRMeta](VenueClaimBsonR)
    val firstClaim = ClassRequiredField(VenueClaimBsonR, VenueClaimBson.default)

    @f val last_updated = LocalDateTimeField
    @f val popularity = ListField[Long]
    @f val categories = ListField[ObjectId]
  }

  println("Create VenueRMeta")
  val VenueR = new VenueRMeta
  println("Done ")
  class VenueClaimRMeta extends MCcMeta[VenueClaim, VenueClaimRMeta]("venueclaims") {
    val venueid = ObjectIdTaggedField[Venue]("vid")
    val status = EnumField[ClaimStatus.type]
    val reason = OptEnumField[RejectReason.type]
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

  class OAuthConsumerMeta extends MCcMeta[OAuthConsumer, OAuthConsumerMeta]("oauthconsumers") {
    val privileges = ListField[ConsumerPrivilege.Value]
  }

  val OAuthConsumerR = new OAuthConsumerMeta

  class CommentMeta extends MCcMeta[Comment, CommentMeta]("comments") {
    val comments = ListField[OneComment]
  }

  val CommentR = new CommentMeta

  class OptValCCMeta extends MCcMeta[OptValCC, OptValCCMeta]("optvalcc") {
    val id = ObjectIdField("_id")
    val ms = OptStringField("maybes")
    val mi = OptObjectIdField("maybeid")
    val rs = StringField("realString")
  }
  val OptValCCR = new OptValCCMeta

  /*class UuidCcMeta extends MCcMeta[UuidCc, UuidCcMeta]("uuidcc") {
    val id = UUIdField("_id")
    val s = StringField
    val dt = InstantField("i")
  }
  val UuidCcR = new UuidCcMeta
*/
  case class Money(amount: BigDecimal, currency: Currency)

  case class Invoice(id: Long, name: String, total: Money)

  class MoneyMeta extends MCcMeta[Money, MoneyMeta] {
    val amount = BigDecimalField
    val currency = CurrencyField
  }

  class InvoiceMeta extends MCcMeta[Invoice, InvoiceMeta] {
    val id = LongField
    val name = StringField
    val total = ClassField[Money, MoneyMeta](new MoneyMeta)
  }

  val Invoices = new InvoiceMeta

  case class MCounter(_id: ObjectId = ObjectId.get(), counts: Map[ObjectId, Long])

  class CounterMeta extends MCcMeta[MCounter, CounterMeta] {
    val id = ObjectIdField("_id")
    val counts = MapField[ObjectId, Long]
  }

  val Counters = new CounterMeta

  type CounterId = ObjectId @@ MCounter

  case class TypedCounter(_id: ObjectId = ObjectId.get(), counts: Map[CounterId, Long])

  class TypedCounterMeta extends MCcMeta[TypedCounter, TypedCounterMeta] {
    val id = ObjectIdField("_id")
    val counts = MapField[CounterId, Long]
  }

  val TypedCounters = new TypedCounterMeta

  case class BinaryData(data: Array[Byte])

  class BinaryDataMeta extends MCcMeta[BinaryData, BinaryDataMeta] {}

  val Binaries = new BinaryDataMeta

  case class LocaleData(locale: Locale)

  class LocaleDataMeta extends MCcMeta[LocaleData, LocaleDataMeta] {
    val locale = LocaleField
  }

  val Locales = new LocaleDataMeta
}

