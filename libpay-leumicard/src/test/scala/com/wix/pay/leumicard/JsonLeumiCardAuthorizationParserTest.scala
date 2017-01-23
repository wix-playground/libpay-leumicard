package com.wix.pay.leumicard

import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

class JsonLeumiCardAuthorizationParserTest extends SpecWithJUnit {
  trait Ctx extends Scope {
    val authorizationParser: LeumiCardAuthorizationParser = new JsonLeumiCardAuthorizationParser
  }

  "stringify and then parse" should {
    "yield an authorization similar to the original one" in new Ctx {
      val someAuthorization = LeumiCardAuthorization(
        transactionId = "0123456"
      )

      val authorizationKey = authorizationParser.stringify(someAuthorization)
      authorizationParser.parse(authorizationKey) must beEqualTo(someAuthorization)
    }
  }
}
