package com.wix.pay.leumicard
import org.json4s.DefaultFormats
import org.json4s.native.Serialization

class JsonLeumiCardAuthorizationParser extends LeumiCardAuthorizationParser {
  private implicit val formats = DefaultFormats

  override def parse(authorizationKey: String): LeumiCardAuthorization =
    Serialization.read[LeumiCardAuthorization](authorizationKey)


  override def stringify(authorization: LeumiCardAuthorization): String =
    Serialization.write(authorization)

}
