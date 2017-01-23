package com.wix.pay.leumicard

/**
  * Created by tzufitb on 23/01/2017.
  */
trait LeumiCardAuthorizationParser {
  def parse(authorizationKey: String): LeumiCardAuthorization
  def stringify(authorization: LeumiCardAuthorization): String
}
