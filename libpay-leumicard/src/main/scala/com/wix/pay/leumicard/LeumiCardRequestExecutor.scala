package com.wix.pay.leumicard

import com.wix.pay.creditcard.CreditCard
import com.wix.pay.leumicard.helpers.CurrencyToCoinConverter
import com.wix.pay.leumicard.model.{ErrorCodes, RequestFields}
import com.wix.pay.model.{Customer, Deal, Payment}
import com.wix.pay.{PaymentErrorException, PaymentRejectedException}

abstract class LeumiCardRequestExecutor {

  def paramsMap(creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal], merchantKey: LeumiCardMerchant, transactionId: Option[String] = None): Map[String, String]

  def verifyResponse(responseCode: String): Unit

  def buildResponse(transactionId: String): String

  def verifyRequiredParams(creditCard: CreditCard, customer: Option[Customer], deal: Option[Deal]): Unit

  def verifySuccess(code: String): Unit = {
    (isSuccess orElse isReject orElse throwErrorException)(code)
  }

  def verifyPostponed(code: String): Unit = {
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
    case ErrorCodes.Rejected | ErrorCodes.WrongCvvOrId => throw PaymentRejectedException()
  }
}

class LeumiCardSaleExecutor(currencyConverter: CurrencyToCoinConverter,
                            password: String) extends LeumiCardRequestExecutor {

  override def verifyRequiredParams(creditCard: CreditCard, customer: Option[Customer], deal: Option[Deal]): Unit = {
    require(deal.isDefined, "Deal is mandatory for Leumi Card")
    require(deal.get.title.isDefined, "Deal Title is mandatory for Leumi Card")
    require(customer.isDefined, "Customer is mandatory for Leumi Card")
    require(customer.get.firstName.isDefined, "Customer First Name is mandatory for Leumi Card")
    require(customer.get.lastName.isDefined, "Customer Last Name is mandatory for Leumi Card")
    require(creditCard.csc.isDefined, "Credit Card CVV is mandatory for Leumi Card")
    require(creditCard.holderId.isDefined, "Credit Card Holder ID is mandatory for Leumi Card")
  }

  override def paramsMap(creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal], merchant: LeumiCardMerchant, transactionId: Option[String] = None): Map[String, String] = {
    Map(
      RequestFields.masof -> merchant.masof,
      RequestFields.action -> "soft",
      RequestFields.userId -> creditCard.holderId.get,
      RequestFields.clientName -> customer.get.firstName.get,
      RequestFields.clientLName -> customer.get.lastName.get,
      RequestFields.infoPurchaseDesc -> deal.get.title.get,
      RequestFields.amount -> payment.currencyAmount.amount.toString,
      RequestFields.currency -> currencyConverter.currencyToCoin(payment.currencyAmount.currency),
      RequestFields.creditCard -> creditCard.number,
      RequestFields.cvv -> creditCard.csc.get,
      RequestFields.expMonth -> creditCard.expiration.month.toString,
      RequestFields.expYear -> creditCard.expiration.year.toString,
      RequestFields.installments -> payment.installments.toString,
      RequestFields.password -> password
    )
  }

  override def verifyResponse(responseCode: String) = verifySuccess(responseCode)

  override def buildResponse(transactionId: String): String = transactionId
}

class LeumiCardAuthorizeExecutor(currencyConverter: CurrencyToCoinConverter,
                                 password: String,
                                 authorizationParser: LeumiCardAuthorizationParser) extends LeumiCardSaleExecutor(currencyConverter, password) {
  override def paramsMap(creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal], merchant: LeumiCardMerchant, transactionId: Option[String] = None): Map[String, String] =
    super.paramsMap(creditCard, payment, customer, deal, merchant) + (RequestFields.postpone -> "True")

  override def verifyResponse(responseCode: String) = verifyPostponed(responseCode)

  override def buildResponse(transactionId: String): String = authorizationParser.stringify(LeumiCardAuthorization(transactionId = transactionId))
}

class LeumiCardCaptureExecutor(password: String) extends LeumiCardRequestExecutor {
  override def paramsMap(creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal], merchant: LeumiCardMerchant, transactionId: Option[String] = None): Map[String, String] =
    Map(
      RequestFields.action -> "commitTrans",
      RequestFields.masof -> merchant.masof,
      RequestFields.transactionId -> transactionId.get,
      RequestFields.password -> password
    )

  override def verifyResponse(responseCode: String) = verifySuccess(responseCode)

  override def buildResponse(transactionId: String): String = transactionId

  override def verifyRequiredParams(creditCard: CreditCard, customer: Option[Customer], deal: Option[Deal]): Unit = {}
}
