package com.wix.pay.leumicard.it

import com.google.api.client.http.javanet.NetHttpTransport
import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.leumicard.{JsonLeumiCardMerchantParser, LeumiCardDriver, LeumiCardGateway, LeumiCardMerchant}
import com.wix.pay.model.{CurrencyAmount, Customer, Deal, Name}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

import scala.reflect.ClassTag
import scala.util.{Failure, Try}

class LeumiCardGatewayIT extends SpecWithJUnit {

  val leumiCardPort = 10019
  val requestFactory = new NetHttpTransport().createRequestFactory()

  val driver = new LeumiCardDriver(port = leumiCardPort, password = "some-password")

  step {
    driver.startProbe()
  }

  sequential

  "sale request via Leumi Card gateway" should {
    "successfully yield transaction id on valid request" in new Context {
      givenSaleRequestToLeumiCard succeedsWith successfulTransactionId

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
      givenSaleRequestToLeumiCard.errors

      val saleResult = executeValidSale

      assertFailure[PaymentErrorException](saleResult)
    }

    "fail if deal is not provided" in new Context {
      val saleResult = leumicardGateway.sale(
        merchantKey = merchantKey,
        creditCard = buyerCreditCard,
        currencyAmount = currencyAmount,
        customer = Some(customer))

      assertFailure[PaymentErrorException](saleResult)
    }

    "fail if customer is not provided" in new Context {
      val saleResult = leumicardGateway.sale(
        merchantKey = merchantKey,
        creditCard = buyerCreditCard,
        currencyAmount = currencyAmount,
        deal = Some(deal))

      assertFailure[PaymentErrorException](saleResult)
    }

    "fail with PaymentRejectedException for rejected transactions" in new Context {
      givenSaleRequestToLeumiCard.isRejected

      val saleResult = executeValidSale

      assertFailure[PaymentRejectedException](saleResult)
    }
  }

  "authorize request via Leumi Card gateway" should {
    "successfully yield transaction id on valid request" in new Context {
      givenAuthorizeRequestToLeumiCard succeedsWith successfulTransactionId

      val authorizeResult = executeValidAuthorize

      authorizeResult must beSuccessfulTry(check = beEqualTo(successfulTransactionId))
    }

    "fail for invalid merchant format" in new Context {
      val authorizeResult = leumicardGateway.authorize(
        merchantKey = "bla bla",
        creditCard = buyerCreditCard,
        currencyAmount = currencyAmount,
        customer = Some(customer),
        deal = Some(deal))

      assertFailure[PaymentErrorException](authorizeResult)
    }

    "fail when authorize is not successful" in new Context {
      givenAuthorizeRequestToLeumiCard.errors

      val authorizeResult = executeValidSale

      assertFailure[PaymentErrorException](authorizeResult)
    }

    "fail if deal is not provided" in new Context {
      val authorizeResult = leumicardGateway.authorize(
        merchantKey = merchantKey,
        creditCard = buyerCreditCard,
        currencyAmount = currencyAmount,
        customer = Some(customer))

      assertFailure[PaymentErrorException](authorizeResult)
    }
  }

  "capture request via Leumi Card gateway" should {
    "successfully yield transaction id on valid request" in new Context {
      givenCaptureRequestToLeumiCard succeedsWith successfulTransactionId

      val captureResult  = executeValidCapture

      captureResult must beSuccessfulTry(check = beEqualTo(successfulTransactionId))
    }

    "fail for invalid merchant format" in new Context {
      val captureResult = leumicardGateway.capture(
        merchantKey = "bla bla",
        amount = currencyAmount.amount,
        authorizationKey = successfulTransactionId)

      assertFailure[PaymentErrorException](captureResult)
    }

    "fail when capture is not successful" in new Context {
      givenCaptureRequestToLeumiCard.errors

      val captureResult = executeValidCapture

      assertFailure[PaymentErrorException](captureResult)
    }
  }

  trait Context extends Scope {
    val leumicardGateway = new LeumiCardGateway(
      requestFactory = requestFactory,
      paymentsEndpointUrl = s"http://localhost:$leumiCardPort/",
      password = "some-password")

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

    def givenSaleRequestToLeumiCard: driver.SaleContext = {
      driver.aSaleFor(
        masof = merchant.masof,
        currencyAmount = currencyAmount,
        creditCard = buyerCreditCard,
        customer = customer,
        deal = deal)
    }

    def givenAuthorizeRequestToLeumiCard: driver.AuthorizeContext = {
      driver.anAuthorizeFor(
        masof = merchant.masof,
        currencyAmount = currencyAmount,
        creditCard = buyerCreditCard,
        customer = customer,
        deal = deal)
    }

    def givenCaptureRequestToLeumiCard: driver.CaptureContext = {
      driver.aCaptureFor(
        masof = merchant.masof,
        currencyAmount = currencyAmount,
        authorizationKey = successfulTransactionId)
    }

    def executeValidSale =
      leumicardGateway.sale(
        merchantKey = merchantKey,
        creditCard = buyerCreditCard,
        currencyAmount = currencyAmount,
        customer = Some(customer),
        deal = Some(deal))

    def executeValidAuthorize =
      leumicardGateway.authorize(
        merchantKey = merchantKey,
        creditCard = buyerCreditCard,
        currencyAmount = currencyAmount,
        customer = Some(customer),
        deal = Some(deal))

    def executeValidCapture =
      leumicardGateway.capture(
        merchantKey = merchantKey,
        authorizationKey = successfulTransactionId,
        amount = currencyAmount.amount)
  }

  def assertFailure[T: ClassTag](result: Try[String]) = {
    result must beAnInstanceOf[Failure[PaymentRejectedException]]

    result.asInstanceOf[Failure[PaymentRejectedException]].exception must beAnInstanceOf[T]
  }
}
