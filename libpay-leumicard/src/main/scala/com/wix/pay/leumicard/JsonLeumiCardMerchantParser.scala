package com.wix.pay.leumicard

import org.json4s.DefaultFormats
import org.json4s.native.Serialization

class JsonLeumiCardMerchantParser extends LeumiCardMerchantParser {
  implicit val formats = DefaultFormats

  override def parse(merchantKey: String): LeumiCardMerchant =
    Serialization.read[LeumiCardMerchant](merchantKey)

  override def stringify(merchant: LeumiCardMerchant): String =
    Serialization.write(merchant)

}
