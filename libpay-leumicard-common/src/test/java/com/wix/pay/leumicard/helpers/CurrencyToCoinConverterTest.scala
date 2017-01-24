package com.wix.pay.leumicard.helpers

import org.specs2.mutable.SpecWithJUnit

class CurrencyToCoinConverterTest extends SpecWithJUnit {
  val converter = new CurrencyToCoinConverter

  "CurrencyToCoinConverter" should {
    "return the matching coin value for a given currency" in {
      converter.currencyToCoin("ILS") must beEqualTo("1")
      converter.currencyToCoin("USD") must beEqualTo("2")
      converter.currencyToCoin("EUR") must beEqualTo("3")
      converter.currencyToCoin("GBP") must beEqualTo("4")
    }

    "fail for not supported currency" in {
      converter.currencyToCoin("AAA") must throwA[IllegalCurrencyException]
    }
  }
}
