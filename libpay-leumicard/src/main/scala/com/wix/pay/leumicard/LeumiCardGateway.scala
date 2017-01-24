package com.wix.pay.leumicard

import java.util.{List => JList}

import com.google.api.client.http._
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.leumicard.helpers.CurrencyToCoinConverter
import com.wix.pay.leumicard.model.{ErrorCodes, RequestFields, ResponseFields}
import com.wix.pay.model.{CurrencyAmount, Customer, Deal}
import com.wix.pay.{PaymentErrorException, PaymentGateway, PaymentRejectedException}

import scala.collection.JavaConversions._
import scala.collection.{JavaConversions, mutable}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}


class LeumiCardGateway(requestFactory: HttpRequestFactory,
                       connectTimeout: Option[Duration] = None,
                       readTimeout: Option[Duration] = None,
                       numberOfRetries: Int = 0,
                       paymentsEndpointUrl: String = Endpoints.leumiCardPayUrl,
                       merchantParser: LeumiCardMerchantParser = new JsonLeumiCardMerchantParser,
                       authorizationParser: LeumiCardAuthorizationParser = new JsonLeumiCardAuthorizationParser,
                       currencyConverter: CurrencyToCoinConverter = new CurrencyToCoinConverter,
                       password: String = ""
                      ) extends PaymentGateway {

  override def sale(merchantKey: String, creditCard: CreditCard, currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    Try {
      verifyRequiredParams(creditCard, customer, deal)

      val merchant = merchantParser.parse(merchantKey)
      val response = doRequest(saleParamsMap(creditCard, currencyAmount, customer, deal, merchant))
      val responseCode = response(ResponseFields.ccode)

      verifySuccess(responseCode)

      response(ResponseFields.id)
    } match {
      case Success(transactionId: String) => Success(transactionId)
      case Failure(e: PaymentRejectedException) => Failure(e)
      case Failure(e) => Failure(PaymentErrorException(e.getMessage, e))
    }
  }

  override def authorize(merchantKey: String, creditCard: CreditCard, currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    Try {
      verifyRequiredParams(creditCard, customer, deal)

      val merchant = merchantParser.parse(merchantKey)
      val response = doRequest(authorizeParamsMap(creditCard, currencyAmount, customer, deal, merchant))
      val responseCode = response(ResponseFields.ccode)

      verifyPostponed(responseCode)

      authorizationParser.stringify(LeumiCardAuthorization(
        transactionId = response(ResponseFields.id)
      ))

    } match {
      case Success(authorizationKey) => Success(authorizationKey)
      case Failure(e: PaymentRejectedException) => Failure(e)
      case Failure(e) => Failure(PaymentErrorException(e.getMessage, e))
    }
  }

  override def capture(merchantKey: String, authorizationKey: String, amount: Double): Try[String] = {
    Try {
      val merchant = merchantParser.parse(merchantKey)
      val response = doRequest(captureParamsMap(authorizationKey, merchant))
      val responseCode = response(ResponseFields.ccode)

      verifySuccess(responseCode)

      response(ResponseFields.id)
    } match {
      case Success(transactionId: String) => Success(transactionId)
      case Failure(e: PaymentRejectedException) => Failure(e)
      case Failure(e) => Failure(PaymentErrorException(e.getMessage, e))
    }
  }

  private def verifyRequiredParams(creditCard: CreditCard, customer: Option[Customer], deal: Option[Deal]): Unit = {
    require(deal.isDefined, "Deal is mandatory for Leumi Card")
    require(deal.get.title.isDefined, "Deal Title is mandatory for Leumi Card")
    require(customer.isDefined, "Customer is mandatory for Leumi Card")
    require(customer.get.firstName.isDefined, "Customer First Name is mandatory for Leumi Card")
    require(customer.get.lastName.isDefined, "Customer Last Name is mandatory for Leumi Card")
    require(creditCard.csc.isDefined, "Credit Card CVV is mandatory for Leumi Card")
    require(creditCard.holderId.isDefined, "Credit Card Holder ID is mandatory for Leumi Card")
  }



  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] = {
    Try{
      val authorization = authorizationParser.parse(authorizationKey)
      authorization.transactionId
    }
  }

  private def captureParamsMap(transactionId: String, merchant: LeumiCardMerchant): Map[String, String] = {
    Map(
      RequestFields.action -> "commitTrans",
      RequestFields.masof -> merchant.masof,
      RequestFields.transactionId -> transactionId,
      RequestFields.password -> password
    )
  }

  private def authorizeParamsMap(creditCard: CreditCard, currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal], merchant: LeumiCardMerchant): Map[String, String] = {
    saleParamsMap(creditCard, currencyAmount, customer, deal, merchant) + (RequestFields.postpone -> "True")
  }

  private def saleParamsMap(creditCard: CreditCard, currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal], merchant: LeumiCardMerchant): Map[String, String] = {
    Map(
      RequestFields.masof -> merchant.masof,
      RequestFields.action -> "soft",
      RequestFields.userId -> creditCard.holderId.get,
      RequestFields.clientName -> customer.get.firstName.get,
      RequestFields.clientLName -> customer.get.lastName.get,
      RequestFields.infoPurchaseDesc -> deal.get.title.get,
      RequestFields.amount -> currencyAmount.amount.toString,
      RequestFields.currency -> currencyConverter.currencyToCoin(currencyAmount.currency),
      RequestFields.creditCard -> creditCard.number,
      RequestFields.cvv -> creditCard.csc.get,
      RequestFields.expMonth -> creditCard.expiration.month.toString,
      RequestFields.expYear -> creditCard.expiration.year.toString,
      RequestFields.installments -> "1",
      RequestFields.password -> password
    )
  }

  private def doRequest(params: Map[String, String]): Map[String, String] = {
    val url = new GenericUrl(paymentsEndpointUrl)

    val content = new UrlEncodedContent(JavaConversions.mapAsJavaMap(params))
    val httpRequest = requestFactory.buildPostRequest(url, content)

    connectTimeout foreach (to => httpRequest.setConnectTimeout(to.toMillis.toInt))
    readTimeout foreach (to => httpRequest.setReadTimeout(to.toMillis.toInt))
    httpRequest.setNumberOfRetries(numberOfRetries)

    val response = extractAndCloseResponse(httpRequest.execute())
    response
  }

  private def extractAndCloseResponse(httpResponse: HttpResponse) = {
    try {
      val params = mutable.LinkedHashMap[String, JList[String]]()
      UrlEncodedParser.parse(httpResponse.parseAsString(), mutableMapAsJavaMap(params))
      params.mapValues( _(0) ).toMap
    } finally {
      httpResponse.ignore()
    }
  }

  private def verifySuccess(code: String): Unit = {
    (isSuccess orElse isReject orElse throwErrorException)(code)
  }

  private def verifyPostponed(code: String): Unit = {
    (isPostpone orElse isReject orElse throwErrorException)(code)
  }

  private def throwErrorException: PartialFunction[String, Unit] = {
    case code => throw PaymentErrorException(code)
  }

  private def isPostpone: PartialFunction[String, Unit] = {
    case ErrorCodes.Postponed =>
  }

  private def isSuccess: PartialFunction[String, Unit] = {
    case ErrorCodes.Success =>
  }

  private def isReject: PartialFunction[String, Unit] = {
    case ErrorCodes.Rejected => throw PaymentRejectedException()
  }
}

object Endpoints {
  val leumiCardPayUrl = "https://pay.leumicard.co.il/p/"
}
