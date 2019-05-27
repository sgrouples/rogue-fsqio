package me.sgrouples.rogue.cc

import java.util.Currency

import me.sgrouples.rogue.BsonFormat
import me.sgrouples.rogue.cc.macros.MacroBsonFormat
import org.bson.{ BsonDecimal128, BsonDocument, BsonString, BsonValue, BsonWriter }
import org.bson.types.Decimal128

import scala.util.control.NonFatal
import me.sgrouples.rogue.cc.macros.MacroInstances._

final class MoneyFormat extends MacroBsonFormat[Money] {
  private[this] final val currencyKey = "currency"
  private[this] final val amountKey = "amount"

  override def append(writer: BsonWriter, k: String, v: Money): Unit = {
    writer.writeStartDocument(k)
    append(writer, v)
    writer.writeEndDocument()
  }

  override def append(writer: BsonWriter, v: Money): Unit = {
    writer.writeDecimal128(amountKey, new Decimal128(v.amount.bigDecimal))
    writer.writeString(currencyKey, v.currency.getCurrencyCode)
  }

  override def read(b: BsonValue): Money = {
    if (b.isDocument) {
      val d = b.asDocument()
      try {
        val currency = java.util.Currency.getInstance(d.getString(currencyKey).getValue)
        val amount = d.getNumber(amountKey).asDecimal128().getValue.bigDecimalValue()
        Money(amount, currency)
      } catch {
        case NonFatal(e) =>
          defaultValue
      }
    } else {
      defaultValue
    }
  }

  override def write(t: Money): BsonValue = {
    new BsonDocument()
      .append(amountKey, new BsonDecimal128(new Decimal128(t.amount.bigDecimal)))
      .append(currencyKey, new BsonString(t.currency.getCurrencyCode))
  }

  override def defaultValue: Money = Money(BigDecimal(0), Currency.getInstance("USD"))

  override def validNames(): Vector[String] = Vector(amountKey, currencyKey)

  override def flds: Map[String, BsonFormat[_]] = Map(
    amountKey -> bigDecimalFormat,
    currencyKey -> currencyFormat)
}
object MFInstance {
  implicit val moneyFormatInstance = new MoneyFormat
}
