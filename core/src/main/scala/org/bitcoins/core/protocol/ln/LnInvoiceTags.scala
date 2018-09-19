package org.bitcoins.core.protocol.ln

import org.bitcoins.core.crypto.Sha256Digest
import org.bitcoins.core.protocol.{ Bech32Address, BitcoinAddress }
import org.bitcoins.core.util.{ Bech32, BitcoinSUtil }
import org.bitcoins.core.number.{ UInt64, UInt8 }
import org.bitcoins.core.protocol.script.ScriptPubKey
import scodec.bits.ByteVector

sealed abstract class LnInvoiceTags {
  def paymentHash: Sha256Digest

  def description: Option[String]

  def spk: Option[ScriptPubKey]

  def descriptionHash: Option[Sha256Digest]

  def expiryTime: Option[UInt64]

  def cltvExpiry: Option[UInt64]

  type fallbackAddress = (Int, BitcoinAddress)
  def fallbackAddress: Option[fallbackAddress]

  def routingInfo: Option[Vector[LnRoutingInfo]] //TODO: Not implemented

  def toUnsignedByte(byte: Byte): Int = byte & 0xFF

  override def toString: String = {
    fromHexStrToBech32(LnTagPrefix.PaymentHash, paymentHash.hex) +
      fromStringToBech32(LnTagPrefix.Description, description.getOrElse("")) +
      fromHexStrToBech32(LnTagPrefix.SignaturePubKey, spk.map(_.hex).getOrElse("")) +
      fromHexStrToBech32(LnTagPrefix.DescriptionHash, descriptionHash.map(_.hex).getOrElse("")) +
      fromUInt64toBech32(LnTagPrefix.ExpiryTime, expiryTime) +
      fromUInt64toBech32(LnTagPrefix.CltvExpiry, cltvExpiry) +
      fallbackAddressBech32(fallbackAddress)
    //TODO: Add routingInfo
  }

  def fallbackAddressBech32(input: Option[fallbackAddress]): String = {
    if (input.isDefined) {
      val uInt8Array = uInt64ToBase32(UInt64(input.get._1)) ++ fromHexToBase32(input.get._2.hash.hex)
      val bech32String = Bech32.encodeToString(uInt8Array)
      val bech32DataLength = bech32EncodeDataLength(bech32String)
      LnTagPrefix.FallbackAddress + bech32DataLength + bech32String
    } else {
      ""
    }
  }

  def fromStringToBech32(prefix: LnTagPrefix, tag: String): String = {
    if (!tag.isEmpty) {
      val byteVec = ByteVector(tag.map(a => a.toByte))
      val base32Array = Bech32.from8bitTo5bit(byteVec)
      val bech32String = Bech32.encodeToString(base32Array.get)
      val bech32DataLength = bech32EncodeDataLength(bech32String)
      prefix.value + bech32DataLength + bech32String
    } else { "" }
  }

  def fromHexStrToBech32(prefix: LnTagPrefix, tag: String): String = {
    if (!tag.isEmpty) {
      val uInt8Array = fromHexToBase32(tag)
      val bech32String = Bech32.encodeToString(uInt8Array)
      val bech32DataLength = bech32EncodeDataLength(bech32String)
      prefix.toString + bech32DataLength + bech32String
    } else {
      ""
    }
  }

  def fromUInt64toBech32(prefix: LnTagPrefix, tag: Option[UInt64]): String = {
    if (tag.isDefined) {
      val uInt8Array = uInt64ToBase32(tag.get)
      val bech32String = Bech32.encodeToString(uInt8Array)
      val bech32DataLength = bech32EncodeDataLength(bech32String)
      prefix + bech32DataLength + bech32String
    } else {
      ""
    }
  }

  def fromHexToBase32(hex: String): Vector[UInt8] = {
    val byteArr = BitcoinSUtil.decodeHex(hex)
    UInt8.toUInt8s(byteArr)
  }

  //TODO: Refactor Into Bech32Address?
  def uInt64ToBase32(input: UInt64): Vector[UInt8] = {
    //To fit an UInt64 value, we need at most ceil(64 / 5) = 13 groups of 5 bits.
    val arr: Array[Int] = new Array[Int](13)
    for (x <- 0 to 12) {
      arr(x) = (input >> x * 5 & UInt64(0x1F)).toInt.toByte
    }
    arr.reverse.dropWhile(_ == 0).map(b => UInt8(b)).toVector
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
    val v = Vector(UInt8(quotient.toShort), UInt8(remainder.toShort))
    Bech32.encodeToString(v)
  }

}

case class InvoiceTags(paymentHash: Sha256Digest, description: Option[String], spk: Option[ScriptPubKey],
  descriptionHash: Option[Sha256Digest], expiryTime: Option[UInt64], cltvExpiry: Option[UInt64],
  fallbackAddress: Option[(Int, BitcoinAddress)], routingInfo: Option[Vector[LnRoutingInfo]]) extends LnInvoiceTags {
  require(
    (description.nonEmpty && description.get.length < 640) || descriptionHash.nonEmpty,
    "You must supply either a description hash, or a literal description that is 640 characters or less to create an invoice.")
}

case class LnRoutingInfo(pubkey: String, shortChannelID: String, feeBaseMsat: Int, feePropMilli: Int, cltvExpiryDelta: Int) {
  //TODO: Placeholder
}