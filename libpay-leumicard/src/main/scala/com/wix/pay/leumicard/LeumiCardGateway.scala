package com.wix.pay.leumicard

import java.util.{List => JList}

import com.google.api.client.http._
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.leumicard.model.{ErrorCodes, RequestFields, ResponseFields}
import com.wix.pay.model.{CurrencyAmount, Customer, Deal}
import com.wix.pay.{PaymentErrorException, PaymentGateway, PaymentRejectedException}

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}


class LeumiCardGateway(requestFactory: HttpRequestFactory,
                       connectTimeout: Option[Duration] = None,
                       readTimeout: Option[Duration] = None,
                       numberOfRetries: Int = 0,
                       paymentsEndpointUrl: String = Endpoints.leumiCardPayUrl,
                       merchantParser: LeumiCardMerchantParser = new JsonLeumiCardMerchantParser
                      ) extends PaymentGateway {

  override def sale(merchantKey: String, creditCard: CreditCard, currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    Try {
      verifyRequiredParams(creditCard, customer, deal)

      val merchant = merchantParser.parse(merchantKey)
      val response = doRequest(saleParamsMap(creditCard, currencyAmount, customer, deal, merchant))

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

      verifyPostponedResponse(response)

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


  override def capture(merchantKey: String, authorizationKey: String, amount: Double): Try[String] = ???

  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] = ???

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
      RequestFields.creditCard -> creditCard.number,
      RequestFields.cvv -> creditCard.csc.get,
      RequestFields.expMonth -> creditCard.expiration.month.toString,
      RequestFields.expYear -> creditCard.expiration.year.toString
    )
  }

  private def doRequest(params: Map[String, String]): Map[String, String] = {
    val url = new GenericUrl(paymentsEndpointUrl)
    params.foreach({ case (key, value) => url.set(key, value) })

    val httpRequest = requestFactory.buildGetRequest(url)

    connectTimeout foreach (to => httpRequest.setConnectTimeout(to.toMillis.toInt))
    readTimeout foreach (to => httpRequest.setReadTimeout(to.toMillis.toInt))
    httpRequest.setNumberOfRetries(numberOfRetries)

    val response = extractAndCloseResponse(httpRequest.execute())
    verifyLeumiCardResponse(response)
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

  private def verifyLeumiCardResponse(response: Map[String, String]): Unit = {
    val code = response(ResponseFields.ccode)

    code match {
      case ErrorCodes.Success => // Operation successful.
      case ErrorCodes.Postponed =>
      case ErrorCodes.Rejected => throw PaymentRejectedException(code)
      case _ => throw PaymentErrorException(code)
    }
  }

  private def verifyPostponedResponse(response: Map[String, String]): Unit = {
    val code = response(ResponseFields.ccode)

    code match {
      case ErrorCodes.Postponed =>
      case _ => throw PaymentErrorException(code)
    }
  }
}

object Endpoints {
  val leumiCardPayUrl = "https://pay.leumicard.co.il/p/"
}
