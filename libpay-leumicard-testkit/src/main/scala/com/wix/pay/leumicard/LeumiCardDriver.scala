package com.wix.pay.leumicard

import com.wix.hoopoe.http.testkit.EmbeddedHttpProbe
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.leumicard.model.RequestFields
import com.wix.pay.model.CurrencyAmount
import spray.http._

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

  def aSaleFor(masof: String,
               currencyAmount: CurrencyAmount,
               creditCard: CreditCard) =
    SaleContext(masof, currencyAmount, creditCard)


  case class SaleContext(masof: String,
                         currencyAmount: CurrencyAmount,
                         creditCard: CreditCard) {

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

    private def isStubbedUri(uri: Uri) = {
      val uriParams = uri.query
      asRequestParams.forall({ case (key, value) => uriParams.contains((key, value)) })
    }

    private def asRequestParams =
      Map(RequestFields.masof -> masof)

    private def successfulResponse(transactionId: String) =
      s"Id=$transactionId&CCode=0&Amount=1000&ACode=&Fild1=&Fild2=&Fild3="

    private def failResponse =
      s"Id=0&CCode=6&Amount=1000&ACode=&Fild1=&Fild2=&Fild3="
  }

}