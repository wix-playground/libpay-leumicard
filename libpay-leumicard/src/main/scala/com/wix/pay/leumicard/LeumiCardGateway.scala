package com.wix.pay.leumicard

import java.util.{List => JList}

import com.google.api.client.http._
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.leumicard.helpers.CurrencyToCoinConverter
import com.wix.pay.leumicard.model.ResponseFields
import com.wix.pay.model.{Customer, Deal, Payment}
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

  private val saleExecutor = new LeumiCardSaleExecutor(currencyConverter, password)
  private val authorizeExecutor = new LeumiCardAuthorizeExecutor(currencyConverter, password, authorizationParser)
  private val captureExecutor = new LeumiCardCaptureExecutor(password)

  override def sale(merchantKey: String, creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    execute(saleExecutor, merchantKey, creditCard, payment, customer, deal)
  }

  override def authorize(merchantKey: String, creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    execute(authorizeExecutor, merchantKey, creditCard, payment, customer, deal)
  }

  override def capture(merchantKey: String, authorizationKey: String, amount: Double): Try[String] = {
    val authorization = authorizationParser.parse(authorizationKey)
    execute(captureExecutor, merchantKey, null, null, None, None, Some(authorization.transactionId))
  }

  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] = {
    Try{
      val authorization = authorizationParser.parse(authorizationKey)
      authorization.transactionId
    }
  }

  private def execute(executor: LeumiCardRequestExecutor,
                      merchantKey: String,
                      creditCard: CreditCard,
                      payment: Payment,
                      customer: Option[Customer],
                      deal: Option[Deal],
                      transactionId: Option[String] = None) = {
    Try {
      executor.verifyRequiredParams(creditCard, customer, deal)

      val merchant = merchantParser.parse(merchantKey)
      val response = doRequest(executor.paramsMap(creditCard, payment, customer, deal, merchant, transactionId))
      val responseCode = response(ResponseFields.ccode)

      executor.verifyResponse(responseCode)

      executor.buildResponse(response(ResponseFields.id))
    } match {
      case Success(transactionId: String) => Success(transactionId)
      case Failure(e: PaymentRejectedException) => Failure(e)
      case Failure(e: IllegalArgumentException) => Failure(PaymentErrorException(s"Probably erroneous Masof: ${e.getMessage}", e))
      case Failure(e) => Failure(PaymentErrorException(e.getMessage, e))
    }
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
}

object Endpoints {
  val leumiCardPayUrl = "https://pay.leumicard.co.il/p/"
}
