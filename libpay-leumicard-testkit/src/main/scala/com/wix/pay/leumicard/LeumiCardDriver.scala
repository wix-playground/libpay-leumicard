package com.wix.pay.leumicard

import com.wix.hoopoe.http.testkit.EmbeddedHttpProbe
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.leumicard.model.RequestFields
import com.wix.pay.model.{CurrencyAmount, Customer, Deal}
import spray.http.{HttpEntity, _}

class LeumiCardDriver(port: Int) {
  private val probe = new EmbeddedHttpProbe(port, EmbeddedHttpProbe.NotFoundHandler)
  private val responseContentType = ContentType(MediaTypes.`text/html`)

  def startProbe() {
    probe.doStart()
  }

  def stopProbe() {
    probe.doStop()
  }

  def resetProbe() {
    probe.handlers.clear()
  }

  def anAuthorizeFor(masof: String,
                     currencyAmount: CurrencyAmount,
                     creditCard: CreditCard,
                     customer: Customer,
                     deal: Deal) =
    AuthorizeContext(masof, currencyAmount, creditCard, customer, deal)

  def aSaleFor(masof: String,
               currencyAmount: CurrencyAmount,
               creditCard: CreditCard,
               customer: Customer,
               deal: Deal) =
    SaleContext(masof, currencyAmount, creditCard, customer, deal)


  trait RequestContext {
    def masof: String
    def currencyAmount: CurrencyAmount
    def creditCard: CreditCard
    def customer: Customer
    def deal: Deal

    def succeedsWith(transactionId: String) = {
      probe.handlers += {
        case HttpRequest(HttpMethods.GET, uri, _, _, _) if isStubbedUri(uri) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(responseContentType, successfulResponse(transactionId)))
      }
    }

    def errors = {
      probe.handlers += {
        case HttpRequest(HttpMethods.GET, _, _, _, _) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(responseContentType, failResponse))
      }
    }

    def isRejected = {
      probe.handlers += {
        case HttpRequest(HttpMethods.GET, _, _, _, _) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(responseContentType, rejectResponse))
      }
    }


    private def isStubbedUri(uri: Uri) = {
      val uriParams = uri.query
      asRequestParams.forall({ case (key, value) => uriParams.contains((key, value)) })
    }

    protected def asRequestParams =
      Map(
        RequestFields.masof -> masof,
        RequestFields.action -> "soft",
        RequestFields.userId -> creditCard.holderId.get,
        RequestFields.clientName -> customer.firstName.get,
        RequestFields.clientLName -> customer.lastName.get,
        RequestFields.infoPurchaseDesc ->  deal.title.get,
        RequestFields.amount -> currencyAmount.amount.toString,
        RequestFields.creditCard -> creditCard.number,
        RequestFields.cvv -> creditCard.csc.get,
        RequestFields.expMonth -> creditCard.expiration.month.toString,
        RequestFields.expYear -> creditCard.expiration.year.toString,
        RequestFields.installments -> "1"
      )

    def successfulResponse(transactionId: String): String

    def failResponse =
      s"Id=0&CCode=6&Amount=1000&ACode=&Fild1=&Fild2=&Fild3="

    def rejectResponse =
      s"Id=0&CCode=33&Amount=1000&ACode=&Fild1=&Fild2=&Fild3="

  }

  case class AuthorizeContext(masof: String,
                              currencyAmount: CurrencyAmount,
                              creditCard: CreditCard,
                              customer: Customer,
                              deal: Deal) extends RequestContext {
    override protected def asRequestParams: Map[String, String] = super.asRequestParams + (RequestFields.postpone -> "True")

    def successfulResponse(transactionId: String) =
      s"Id=$transactionId&CCode=800&Amount=1000&ACode=&Fild1=&Fild2=&Fild3="

  }

  case class SaleContext(masof: String,
                         currencyAmount: CurrencyAmount,
                         creditCard: CreditCard,
                         customer: Customer,
                         deal: Deal) extends RequestContext {

    def successfulResponse(transactionId: String) =
      s"Id=$transactionId&CCode=0&Amount=1000&ACode=&Fild1=&Fild2=&Fild3="
  }

}