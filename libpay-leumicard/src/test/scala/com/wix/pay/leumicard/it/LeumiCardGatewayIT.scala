package com.wix.pay.leumicard.it

import com.google.api.client.http.javanet.NetHttpTransport
import com.wix.pay.PaymentErrorException
import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.leumicard.{JsonLeumiCardMerchantParser, LeumiCardDriver, LeumiCardGateway, LeumiCardMerchant}
import com.wix.pay.model.CurrencyAmount
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

import scala.util.Failure

class LeumiCardGatewayIT extends SpecWithJUnit {

  val leumiCardPort = 10001
  val requestFactory = new NetHttpTransport().createRequestFactory()

  val driver = new LeumiCardDriver(port = leumiCardPort)

  step {
    driver.startProbe()
  }

  sequential

  "sale request via Leumi Card gateway" should {
    "successfully yield transaction id on valid request" in new Context {
      driver.aSaleFor(
        masof = merchant.masof,
        currencyAmount = currencyAmount,
        creditCard = buyerCreditCard) succeedsWith (transactionId = successfulTransactionId)

      val saleResult = leumicardGateway.sale(
        merchantKey = merchantKey,
        creditCard = buyerCreditCard,
        currencyAmount = currencyAmount)

      saleResult must beSuccessfulTry(check = beEqualTo(successfulTransactionId))
    }

    "fail with invalid merchant format" in new Context {
      val saleResult = leumicardGateway.sale(
        merchantKey = "bla bla",
        creditCard = buyerCreditCard,
        currencyAmount = currencyAmount)

      saleResult must beAnInstanceOf[Failure[PaymentErrorException]]
    }

    "fail when sale is not successful" in new Context {
      driver.aSaleFor(
        masof = merchant.masof,
        currencyAmount = currencyAmount,
        creditCard = buyerCreditCard) errors

      val saleResult = leumicardGateway.sale(
        merchantKey = merchantKey,
        creditCard = buyerCreditCard,
        currencyAmount = currencyAmount)

      saleResult must beAnInstanceOf[Failure[PaymentErrorException]]
    }
  }

  trait Context extends Scope {
    val leumicardGateway = new LeumiCardGateway(
      requestFactory = requestFactory,
      payUrl = s"http://localhost:$leumiCardPort/")

    val merchantParser = new JsonLeumiCardMerchantParser()

    val merchant = LeumiCardMerchant(masof = "012345678")
    val merchantKey = merchantParser.stringify(merchant)
    val currencyAmount = CurrencyAmount("USD", 33.3)
    val buyerCreditCard = CreditCard(
      "4580458045804580",
      YearMonth(2020, 12),
      Some(CreditCardOptionalFields.withFields(
        csc = Some("123"),
        holderName = Some("some holder name"),
        billingAddress = Some("some billing address"),
        billingPostalCode = Some("90210"))))

    val successfulTransactionId = "4638202"

    driver.resetProbe()
  }
}
