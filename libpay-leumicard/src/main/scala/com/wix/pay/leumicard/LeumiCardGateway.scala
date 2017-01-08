package com.wix.pay.leumicard

import java.util.List
import java.util.{List => JList}

import scala.collection.JavaConversions._
import com.google.api.client.http._
import com.wix.pay.{PaymentErrorException, PaymentGateway, PaymentRejectedException}
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.leumicard.model.{ErrorCodes, RequestFields, ResponseFields}
import com.wix.pay.model.{CurrencyAmount, Customer, Deal}

import scala.collection.JavaConversions._
import scala.collection.{JavaConversions, mutable}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}


class LeumiCardGateway(requestFactory: HttpRequestFactory,
                       connectTimeout: Option[Duration] = None,
                       readTimeout: Option[Duration] = None,
                       numberOfRetries: Int = 0,
                       payUrl: String = Endpoints.leumiCardPayUrl,
                       merchantParser: LeumiCardMerchantParser = new JsonLeumiCardMerchantParser
                      ) extends PaymentGateway {

  override def sale(merchantKey: String, creditCard: CreditCard, currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    Try {
      val merchant = merchantParser.parse(merchantKey)
      val response = doRequest(Map(RequestFields.masof -> merchant.masof))

      response(ResponseFields.id)
    } match {
      case Success(transactionId: String) => Success(transactionId)
      case Failure(e) => Failure(PaymentErrorException(e.getMessage, e))
    }
  }

  override def authorize(merchantKey: String, creditCard: CreditCard, currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal]): Try[String] = ???

  override def capture(merchantKey: String, authorizationKey: String, amount: Double): Try[String] = ???

  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] = ???

  private def doRequest(params: Map[String, String]): Map[String, String] = {
    val url = new GenericUrl(payUrl)
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
/*
      case ErrorCodes.INVALID_CARDHOLDER_NUMBER|
           ErrorCodes.INVALID_EXPIRATION|
           ErrorCodes.UNAUTHORIZED_CARD|
           ErrorCodes.UNAUTHORIZED_COUNTRY => throw PaymentRejectedException(message)
      case IsAuthorizationError(authorizationCode) => throw PaymentRejectedException(message)
*/
      case _ => throw PaymentErrorException(code)
    }
  }
}

object Endpoints {
  val leumiCardPayUrl = "https://pay.leumicard.co.il/p/"
}
