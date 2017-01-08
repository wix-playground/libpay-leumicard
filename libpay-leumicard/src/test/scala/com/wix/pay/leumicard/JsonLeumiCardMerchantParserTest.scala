package com.wix.pay.leumicard

import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

class JsonLeumiCardMerchantParserTest extends SpecWithJUnit {

  "stringify and then parse" should {
    "yield a merchant similar to the original one" in new Context {
      val someMerchant = LeumiCardMerchant(masof = "012345678")

      val merchantKey = merchantParser.stringify(someMerchant)

      merchantParser.parse(merchantKey) must beEqualTo(someMerchant)
    }
  }

  trait Context extends Scope {
    val merchantParser: LeumiCardMerchantParser = new JsonLeumiCardMerchantParser
  }
}
