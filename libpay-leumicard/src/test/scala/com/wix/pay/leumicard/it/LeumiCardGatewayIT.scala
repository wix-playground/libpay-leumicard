package com.wix.pay.leumicard.it

import com.google.api.client.http.javanet.NetHttpTransport
import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.leumicard.{JsonLeumiCardMerchantParser, LeumiCardDriver, LeumiCardGateway, LeumiCardMerchant}
import com.wix.pay.model.{CurrencyAmount, Customer, Deal, Name}
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
      givenRequestToLeumiCard succeedsWith (transactionId = successfulTransactionId)

      val saleResult = executeValidSale

      saleResult must beSuccessfulTry(check = beEqualTo(successfulTransactionId))
    }

    "fail for invalid merchant format" in new Context {
      val saleResult = leumicardGateway.sale(
        merchantKey = "bla bla",
        creditCard = buyerCreditCard,
        currencyAmount = currencyAmount,
        customer = Some(customer),
        deal = Some(deal))

      saleResult must beAnInstanceOf[Failure[PaymentErrorException]]
    }

    "fail when sale is not successful" in new Context {
      givenRequestToLeumiCard.errors

      val saleResult = executeValidSale

      saleResult must beAnInstanceOf[Failure[PaymentErrorException]]
    }

    "fail if deal is not provided" in new Context {
      val saleResult = leumicardGateway.sale(
        merchantKey = merchantKey,
        creditCard = buyerCreditCard,
        currencyAmount = currencyAmount,
        customer = Some(customer))

      saleResult must beAnInstanceOf[Failure[IllegalArgumentException]]
    }

    "fail if customer is not provided" in new Context {
      val saleResult = leumicardGateway.sale(
        merchantKey = merchantKey,
        creditCard = buyerCreditCard,
        currencyAmount = currencyAmount,
        deal = Some(deal))

      saleResult must beAnInstanceOf[Failure[IllegalArgumentException]]
    }

    "fail with PaymentRejectedException for rejected transactions" in new Context {
      givenRequestToLeumiCard.isRejected

      val saleResult = executeValidSale

      saleResult must beAnInstanceOf[Failure[PaymentRejectedException]]
      saleResult.asInstanceOf[Failure[PaymentRejectedException]].exception must beAnInstanceOf[PaymentRejectedException]
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
        holderId = Some("0123456"))))

    val deal = Deal(
      id = System.currentTimeMillis().toString,
      title = Some("some deal title")
    )

    val customer = Customer(
      name = Some(Name(first = "John", last = "Doe"))
    )

    val successfulTransactionId = "4638202"

    driver.resetProbe()

    def givenRequestToLeumiCard: driver.SaleContext = {
      driver.aSaleFor(
        masof = merchant.masof,
        currencyAmount = currencyAmount,
        creditCard = buyerCreditCard,
        customer = customer,
        deal = deal)
    }

    def executeValidSale =
      leumicardGateway.sale(
        merchantKey = merchantKey,
        creditCard = buyerCreditCard,
        currencyAmount = currencyAmount,
        customer = Some(customer),
        deal = Some(deal))
  }
}
