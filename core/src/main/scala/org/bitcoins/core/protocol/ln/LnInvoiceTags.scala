package org.bitcoins.core.protocol.ln

import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.util.BitcoinSUtil
import org.bitcoins.core.number.{ UInt64, UInt8 }

sealed abstract class LnInvoiceTags {
  def paymentHash: String

  def description: Option[String]

  def signaturePubKey: Option[String]

  def descriptionHash: Option[String]

  def expiryTime: Option[UInt64]

  def cltvExpiry: Option[UInt64]

  type fallbackAddress = (Int, String)
  def fallbackAddress: Option[fallbackAddress] //TODO: Not implemented

  def routingInfo: Option[Seq[LnRoutingInfo]] //TODO: Not implemented

  def toUnsignedByte(byte: Byte): Int = byte & 0xFF

  override def toString: String = {
    fromHexStrToBech32(LnTagPrefix.PaymentHash, paymentHash) +
    fromStringToBech32(LnTagPrefix.Description, description.getOrElse("")) +
    fromHexStrToBech32(LnTagPrefix.SignaturePubKey, signaturePubKey.getOrElse("")) +
    fromHexStrToBech32(LnTagPrefix.DescriptionHash, descriptionHash.getOrElse("")) +
    fromUInt64toBech32(LnTagPrefix.ExpiryTime, expiryTime) +
    fromUInt64toBech32(LnTagPrefix.CltvExpiry, cltvExpiry)
    //TODO: Add fallBackAddress
    //TODO: Add routingInfo
  }

  def fromStringToBech32(prefix: String, tag: String): String = {
    if (!tag.isEmpty) {
      val uInt8Array = tag.map(a => a.toByte).map(i => UInt8(toUnsignedByte(i).toShort))
      val base32Array = Bech32Address.encode(uInt8Array)
      val bech32String = Bech32Address.encodeToString(base32Array.get)
      val bech32DataLength = bech32EncodeDataLength(bech32String)
      prefix + bech32DataLength + bech32String
    } else { "" }
  }

  def fromHexStrToBech32(prefix: String, tag: String): String = {
    if (!tag.isEmpty) {
      val uInt8Array = fromHexToBase32(tag)
      val bech32String = Bech32Address.encodeToString(uInt8Array)
      val bech32DataLength = bech32EncodeDataLength(bech32String)
      prefix + bech32DataLength + bech32String
    } else { "" }
  }

  def fromUInt64toBech32(prefix: String, tag: Option[UInt64]): String = {
    if (tag.isDefined) {
      val uInt8Array = uInt64ToBase32(tag.get)
      val bech32String = Bech32Address.encodeToString(uInt8Array)
      val bech32DataLength = bech32EncodeDataLength(bech32String)
      prefix + bech32DataLength + bech32String
    } else { "" }
  }

  def fromHexToBase32(hex: String): Seq[UInt8] = {
    val byteArr = BitcoinSUtil.decodeHex(hex).toSeq.map(b => UInt8(toUnsignedByte(b)))
    Bech32Address.encode(byteArr).get
  }

  //TODO: Refactor Into Bech32Address?
  def uInt64ToBase32(input: UInt64): Seq[UInt8] = {
    //To fit an UInt64 value, we need at most ceil(64 / 5) = 13 groups of 5 bits.
    val arr: Array[Int] = new Array[Int](13)
    for (x <- 0 to 12) {
      arr(x) = (input >> x * 5 & UInt64(0x1F)).toInt.toByte
    }
    arr.reverse.dropWhile(_ == 0).map(b => UInt8(b))
  }

  /**
   * The formula for this calculation is as follows:
   * Take the length of the Bech32 encoded input and divide it by 32.
   * Take the quotient, and encode this value as Bech32. Take the remainder and encode this value as Bech32.
   * Append these values to produce a valid Lighting Network data_length field.
   * Please see Bolt-11 for examples:
   * https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#examples
   */
  def bech32EncodeDataLength(Bech32EncodedString: String): String = {
    val quotient = Bech32EncodedString.length / 32
    val remainder = Bech32EncodedString.length % 32
    Bech32Address.encodeToString(Seq(UInt8(quotient.toShort), UInt8(remainder.toShort)))
  }

  /**
   * This defines the necessary Lightning Network Tag Prefix's, as specified in BOLT-11
   * Please see: https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#tagged-fields
   */
  object LnTagPrefix extends Enumeration {
    val PaymentHash = "p"
    val Description = "d"
    val SignaturePubKey = "n"
    val DescriptionHash = "h"
    val ExpiryTime = "x"
    val CltvExpiry = "c"
    val FallbackAddress = "f"
    val RoutingInfo = "r"
    val None = ""
  }
}

case class InvoiceTags(paymentHash: String, description: Option[String], signaturePubKey: Option[String],
  descriptionHash: Option[String], expiryTime: Option[UInt64], cltvExpiry: Option[UInt64],
  fallbackAddress: Option[(Int, String)], routingInfo: Option[Seq[LnRoutingInfo]]) extends LnInvoiceTags {
  require(
    (description.nonEmpty && description.get.length < 640) || descriptionHash.nonEmpty,
    "You must supply either a description hash, or a literal description that is 640 characters or less to create an invoice.")
}

case class LnRoutingInfo(pubkey: String, shortChannelID: String, feeBaseMsat: Int, feePropMilli: Int, cltvExpiryDelta: Int) {
  //TODO: Placeholder
}