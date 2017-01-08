package com.wix.pay.leumicard

trait LeumiCardMerchantParser {
  def parse(merchantKey: String): LeumiCardMerchant
  def stringify(merchant: LeumiCardMerchant): String
}
