package com.wix.pay.leumicard.helpers

class CurrencyToCoinConverter {
  def currencyToCoin(currency: String) =
    currency match {
      case "ILS" => "1"
      case "USD" => "2"
      case "EUR" => "3"
      case "GBP" => "4"
      case _ => throw IllegalCurrencyException(s"Currency $currency is currently not support in Leumi Card")
    }
}

case class IllegalCurrencyException(message: String) extends IllegalArgumentException(message)
