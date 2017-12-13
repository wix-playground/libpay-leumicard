package com.wix.pay.leumicard


import scala.collection.JavaConversions._
import scala.collection.mutable
import java.util.{List => JList}
import akka.http.scaladsl.model._
import com.google.api.client.http.UrlEncodedParser
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.leumicard.model.RequestFields
import com.wix.pay.model.{CurrencyAmount, Customer, Deal, Payment}
import com.wix.e2e.http.api.StubWebServer
import com.wix.e2e.http.client.extractors.HttpMessageExtractors._
import com.wix.e2e.http.server.WebServerFactory.aStubWebServer
import com.wix.pay.leumicard.helpers.CurrencyToCoinConverter


class LeumiCardDriver(port: Int,
                      password: String = "") {
  private val server: StubWebServer = aStubWebServer.onPort(port).build
  private val responseContentType = ContentType(MediaTypes.`text/html`, HttpCharsets.`UTF-8`)
  private val coinConverter = new CurrencyToCoinConverter()

  def start(): Unit = server.start()
  def stop(): Unit = server.stop()
  def reset(): Unit = server.replaceWith()


  def anAuthorizeFor(masof: String,
                     payment: Payment,
                     creditCard: CreditCard,
                     customer: Customer,
                     deal: Deal) =
    AuthorizeContext(masof, payment, creditCard, customer, deal)

  def aSaleFor(masof: String,
               payment: Payment,
               creditCard: CreditCard,
               customer: Customer,
               deal: Deal) =
    SaleContext(masof, payment, creditCard, customer, deal)

  def aCaptureFor(masof: String,
                  currencyAmount: CurrencyAmount,
                  authorizationKey: String) =
    CaptureContext(masof, currencyAmount, authorizationKey)


  trait RequestContext {
    def asRequestParams: Map[String, String]

    def succeedsWith(transactionId: String): Unit = {
      server.appendAll {
        case HttpRequest(HttpMethods.POST, _, _, entity, _) if isStubbedUri(entity) =>
            HttpResponse(
              status = StatusCodes.OK,
              entity = HttpEntity(responseContentType, successfulResponse(transactionId)))
      }
    }

    def errors(): Unit = {
      server.appendAll {
        case HttpRequest(HttpMethods.POST, _, _, entity, _) if isStubbedUri(entity) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(responseContentType, failResponse))
      }
    }

    def getsRejected(): Unit = {
      server.appendAll {
        case HttpRequest(HttpMethods.POST, _, _, entity, _) if isStubbedUri(entity) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(responseContentType, rejectResponse))
      }
    }

    def returnsIllegalMasof(): Unit = {
      server.appendAll {
        case HttpRequest(HttpMethods.POST, _, _, entity, _) if isStubbedUri(entity) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(responseContentType, illegalMasofResponse))
      }
    }

    private def illegalMasofResponse =
      "<p align=center dir=rtl style=\"font-size:200%;padding-left : 100px;\" class=\"tagline\">ùâéàä</p>\n"

    private def isStubbedUri(entity: HttpEntity) = {
      val requestParams: Map[String, String] = urlDecode(entity.extractAsString)

      asRequestParams.forall { case (key, value) => requestParams.contains((key, value)) }
    }

    private def urlDecode(str: String): Map[String, String] = {
      val params = mutable.LinkedHashMap[String, JList[String]]()
      UrlEncodedParser.parse(str, mutableMapAsJavaMap(params))
      params.mapValues( _(0) ).toMap
    }

    def successfulResponse(transactionId: String): String
    def failResponse: String = "Id=0&CCode=1&Amount=1000&ACode=&Fild1=&Fild2=&Fild3="
    def rejectResponse: String = "Id=0&CCode=6&Amount=1000&ACode=&Fild1=&Fild2=&Fild3="

  }

  case class AuthorizeContext(masof: String,
                              payment: Payment,
                              creditCard: CreditCard,
                              customer: Customer,
                              deal: Deal) extends RequestContext {
    def asRequestParams: Map[String, String] =
      SaleContext(masof, payment, creditCard, customer, deal).asRequestParams + (RequestFields.postpone -> "True")

    def successfulResponse(transactionId: String): String =
      s"Id=$transactionId&CCode=800&Amount=1000&ACode=&Fild1=&Fild2=&Fild3="
  }

  case class SaleContext(masof: String,
                         payment: Payment,
                         creditCard: CreditCard,
                         customer: Customer,
                         deal: Deal) extends RequestContext {

    def asRequestParams: Map[String, String] = {
      Map(
        RequestFields.masof -> masof,
        RequestFields.action -> "soft",
        RequestFields.userId -> creditCard.holderId.get,
        RequestFields.clientName -> customer.firstName.get,
        RequestFields.clientLName -> customer.lastName.get,
        RequestFields.infoPurchaseDesc -> deal.title.get,
        RequestFields.amount -> payment.currencyAmount.amount.toString,
        RequestFields.creditCard -> creditCard.number,
        RequestFields.cvv -> creditCard.csc.get,
        RequestFields.expMonth -> creditCard.expiration.month.toString,
        RequestFields.expYear -> creditCard.expiration.year.toString,
        RequestFields.installments -> payment.installments.toString,
        RequestFields.password -> password,
        RequestFields.currency -> coinConverter.currencyToCoin(payment.currencyAmount.currency))
    }

    def successfulResponse(transactionId: String) =
      s"Id=$transactionId&CCode=0&Amount=1000&ACode=&Fild1=&Fild2=&Fild3="
  }

  case class CaptureContext(masof: String,
                            currencyAmount: CurrencyAmount,
                            authorizationKey: String) extends RequestContext {

    def asRequestParams: Map[String, String] =
      Map(
        RequestFields.masof -> masof,
        RequestFields.action -> "commitTrans",
        RequestFields.transactionId -> authorizationKey,
        RequestFields.password -> password)

    def successfulResponse(transactionId: String): String =
      s"Id=$transactionId&CCode=0&Amount=1000&ACode=&Fild1=&Fild2=&Fild3="
  }
}
