package org.bitcoins.core.protocol.ln

import org.bitcoins.core.crypto.Sha256Digest
import org.scalatest.FlatSpec

class LnInvoiceTagsTest extends FlatSpec {

  behavior of "LnInvoiceTags"

  //https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#examples

  it must "serialize and deserialize BOLT11's example" in {
    //BOLT11 Example #1

    val paymentHash = Sha256Digest.fromHex("0001020304050607080900010203040506070809000102030405060708090102")
    val paymentHashTag = LnInvoiceTag.PaymentHashTag(paymentHash)

    val descriptionE = Left(LnInvoiceTag.DescriptionTag("Please consider supporting this project"))
    val lnTags = LnInvoiceTags(
      paymentHash = paymentHashTag,
      descriptionOrHash = descriptionE)
  }
}
