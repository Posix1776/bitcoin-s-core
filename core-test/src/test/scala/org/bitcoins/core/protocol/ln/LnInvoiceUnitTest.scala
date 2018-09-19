package org.bitcoins.core.protocol.ln

import org.bitcoins.core.crypto.{ ECDigitalSignature, Sha256Digest }
import org.bitcoins.core.number.{ UInt64, UInt8 }
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.core.protocol.ln.LnParams.{ LnBitcoinMainNet, LnBitcoinTestNet }
import org.scalatest.{ FlatSpec, MustMatchers }

class LnInvoiceUnitTest extends FlatSpec with MustMatchers {
  behavior of "LnInvoice"

  val hrpEmpty = LnHumanReadablePart(LnBitcoinMainNet)
  val hrpMicro = LnHumanReadablePart(LnBitcoinMainNet, Some(MicroBitcoins(2500)))
  val hrpMilli = LnHumanReadablePart(LnBitcoinMainNet, Some(MilliBitcoins(20)))
  val hrpTestNetMilli = LnHumanReadablePart(LnBitcoinTestNet, Some(MilliBitcoins(20)))
  val time = UInt64(1496314658)

  val paymentHash = Sha256Digest.fromHex("0001020304050607080900010203040506070809000102030405060708090102")
  val paymentTag = LnInvoiceTag.PaymentHashTag(paymentHash)

  it must "parse BOLT11 example 1" in {
    //BOLT11 Example #1

    val descriptionTagE = Left(LnInvoiceTag.DescriptionTag("Please consider supporting this project"))
    val lnTags = LnInvoiceTags(
      paymentHash = paymentTag,
      descriptionOrHash = descriptionTagE,
      None, None, None,
      None, None)

    Invoice(hrpEmpty, time, lnTags,
      (ECDigitalSignature.fromHex("38ec6891345e204145be8a3a99de38e98a39d6a569434e1845c8af7205afcfcc7f425fcd1463e93c32881ead0d6e356d467ec8c02553f9aab15e5738b11f127f"), 0)).toString must be("lnbc1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpl2pkx2ctnv5sxxmmwwd5kgetjypeh2ursdae8g6twvus8g6rfwvs8qun0dfjkxaq8rkx3yf5tcsyz3d73gafnh3cax9rn449d9p5uxz9ezhhypd0elx87sjle52x86fux2ypatgddc6k63n7erqz25le42c4u4ecky03ylcqca784w")
  }

  it must "parse BOLT11 example 2" in {
    //BOLT11 Example #2

    val descriptionTagE = Left(LnInvoiceTag.DescriptionTag("1 cup coffee"))
    val expiryTimeTag = LnInvoiceTag.ExpiryTimeTag(UInt64(60))
    val lnTags = LnInvoiceTags(
      paymentTag, descriptionTagE, None,
      Some(expiryTimeTag), None,
      None, None)

    Invoice(hrpMicro, time, lnTags,
      (ECDigitalSignature.fromHex("e89639ba6814e36689d4b91bf125f10351b55da057b00647a8dabaeb8a90c95f160f9d5a6e0f79d1fc2b964238b944e2fa4aa677c6f020d466472ab842bd750e"), 1)).toString must be("lnbc2500u1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5xysxxatsyp3k7enxv4jsxqzpuaztrnwngzn3kdzw5hydlzf03qdgm2hdq27cqv3agm2awhz5se903vruatfhq77w3ls4evs3ch9zw97j25emudupq63nyw24cg27h2rspfj9srp")
  }

  it must "parse BOLT11 example 3" in {
    //BOLT11 Example #3 - Description field does not encode correctly due to Japanese letters

    val descriptionTagE = Left(LnInvoiceTag.DescriptionTag("ナンセンス 1杯"))
    val expiryTag = LnInvoiceTag.ExpiryTimeTag(UInt64(60))
    val lnTags = LnInvoiceTags(
      paymentTag, descriptionTagE, None,
      Some(expiryTag), None, None,
      None)

    Invoice(hrpMicro, time, lnTags,
      (ECDigitalSignature.fromHex("259f04511e7ef2aa77f6ff04d51b4ae9209504843e5ab9672ce32a153681f687515b73ce57ee309db588a10eb8e41b5a2d2bc17144ddf398033faa49ffe95ae6"), 0)).toString must be("lnbc2500u1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpquwpc4curk03c9wlrswe78q4eyqc7d8d0xqzpuyk0sg5g70me25alkluzd2x62aysf2pyy8edtjeevuv4p2d5p76r4zkmneet7uvyakky2zr4cusd45tftc9c5fh0nnqpnl2jfll544esqchsrny")
  }

  it must "parse BOLT11 example 4" in {
    //BOLT11 Example #4

    val descriptionHash = Sha256Digest.fromHex("3925b6f67e2c340036ed12093dd44e0368df1b6ea26c53dbe4811f58fd5db8c1")
    val descriptionHashTagE = Right(LnInvoiceTag.DescriptionHashTag(descriptionHash))
    val lnTags = LnInvoiceTags(
      paymentHash = paymentTag,
      descriptionOrHash = descriptionHashTagE,
      None, None, None,
      None, None)

    Invoice(hrpMilli, time, lnTags,
      (ECDigitalSignature.fromHex("c63486e81f8c878a105bc9d959af1973854c4dc552c4f0e0e0c7389603d6bdc67707bf6be992a8ce7bf50016bb41d8a9b5358652c4960445a170d049ced4558c"), 0)).toString must be("lnbc20m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqscc6gd6ql3jrc5yzme8v4ntcewwz5cnw92tz0pc8qcuufvq7khhr8wpald05e92xw006sq94mg8v2ndf4sefvf9sygkshp5zfem29trqq2yxxz7")
  }

  it must "parse BOLT11 example 5" in {
    //BOLT11 Example #5

    val descriptionHash = Sha256Digest.fromHex("3925b6f67e2c340036ed12093dd44e0368df1b6ea26c53dbe4811f58fd5db8c1")
    val descriptionHashTagE = Right(LnInvoiceTag.DescriptionHashTag(descriptionHash))
    val fallbackAddr = LnInvoiceTag.FallbackAddressTag(UInt8(17), P2PKHAddress.fromString("mk2QpYatsKicvFVuTAQLBryyccRXMUaGHP").get)

    val lnTags = LnInvoiceTags(
      paymentTag, descriptionHashTagE,
      None, None,
      None, Some(fallbackAddr), None)

    Invoice(hrpTestNetMilli, time, lnTags,
      (ECDigitalSignature.fromHex("b6c42b8a61e0dc5823ea63e76ff148ab5f6c86f45f9722af0069c7934daff70d5e315893300774c897995e3a7476c8193693d144a36e2645a0851e6ebafc9d0a"), 1)).toString must be("lntb20m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqsfpp3x9et2e20v6pu37c5d9vax37wxq72un98kmzzhznpurw9sgl2v0nklu2g4d0keph5t7tj9tcqd8rexnd07ux4uv2cjvcqwaxgj7v4uwn5wmypjd5n69z2xm3xgksg28nwht7f6zsp")
    //In example #5, the order in which tags are encoded in the invoice has been changed to demonstrate the ability to move tags as needed.
    //For that reason, the example #5 output we are matching against has been modified to fit the order in which we encode our invoices.
    //TODO: Add checksum data to check
  }
}