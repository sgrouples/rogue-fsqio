package me.sgrouples.rogue.cc
import java.util.Currency

case class RoundingMode(value: BigDecimal.RoundingMode.RoundingMode) extends AnyVal

abstract case class Money private[Money] (amount: BigDecimal, currency: Currency)

object Money {

  implicit val defaultRoundingMode = RoundingMode(BigDecimal.RoundingMode.HALF_EVEN)

  def apply(amount: BigDecimal, currency: Currency): Money = apply(amount, currency, defaultRoundingMode)

  def apply(amount: BigDecimal, currency: Currency, roundingMode: RoundingMode): Money = {
    new Money(BigDecimal(amount.toString).setScale(
      currency.getDefaultFractionDigits, roundingMode.value), currency) {}
  }

}
