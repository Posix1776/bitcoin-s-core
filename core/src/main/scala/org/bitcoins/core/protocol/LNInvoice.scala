package org.bitcoins.core.protocol

import LNTagPrefix.LNTagPrefix
import org.bitcoins.core.config._
import org.bitcoins.core.config.{MainNet, RegTest, TestNet3}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.number.UInt8
import org.bitcoins.core.util._


sealed abstract class LightningInvoice {

  private def logger = BitcoinSLogger.logger

  val Bech32Separator: Int = 1

  def network: NetworkParameters

  type Amount = (Int, CurrencyUnit) //e.g, 2500u
  def amount: Option[Amount]

  def timestamp: Long = 1496314658 //This is the timestamp used in the examples, in production we would get a current timestamp --> System.currentTimeMillis / 1000

  def lnTags: LNInvoiceTags

  type Signature = (String, Int)
  def signature:Signature

  def bech32Checsum:String = "" //TODO: Unimplemented. See Bech32Address.createChecksum

  def networkPrefix: String = network match {
    case MainNet  => "lnbc"
    case TestNet3 => "lntb"
    case RegTest  => "lnbcrt"
  }

  def multiplier:String =  amount.get._2 match {
    //TODO: THIS IS ONLY FOR TESTING. Additional CurrencyUnits need to be added to support LN.
    case CurrencyUnits.oneMBTC  => "u"
    case CurrencyUnits.oneBTC   => "m"
  }

  def invoiceAmount: String = { if(amount.nonEmpty){ amount.get._1 + multiplier } else { "" } }

  def Bech32TimeStamp:String = {
    val paddedBinary = timestamp.toBinaryString.reverse.padTo(35, "0").reverse.grouped(5).toList
    val timeBinaryToInt = paddedBinary.map(s => Integer.parseInt(s.mkString, 2))
    val timeUInt8 = timeBinaryToInt.map(i => UInt8(i.toShort))
    Bech32Address.encodeToString(timeUInt8)
  }

  def Bech32EncodedSignature:String = {
    val sigRecoveryHex = if(signature._2 > 0) { "0" + signature._2 } else { "" } //Hex value of recovery bit, padded to ##. I dont like this.
    val sigFullHex = signature._1 + sigRecoveryHex                               //There should be some type of String.Format(##) to expand the hex value to ##.
    val sigHexPaddedBinary = BigInt(sigFullHex, 16).toString(2).reverse.padTo(512, "0").reverse.padTo(520, "0").grouped(5).toList
    val sigHexBinaryToInt = sigHexPaddedBinary.map(s => Integer.parseInt(s.mkString, 2))
    val sigHexUInt8 = sigHexBinaryToInt.map(i => UInt8(i.toShort))
    Bech32Address.encodeToString(sigHexUInt8)
  }

  def Invoice: String = { networkPrefix + invoiceAmount + Bech32Separator + Bech32TimeStamp + lnTags.toBech32String + Bech32EncodedSignature + bech32Checsum }

}

case class LNInvoice(network: NetworkParameters, amount:Option[(Int, CurrencyUnit)], lnTags: LNInvoiceTags, signature: (String, Int)) extends LightningInvoice {

}

//We cant use enums without creating an object?
//https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#tagged-fields
object LNTagPrefix extends Enumeration {
  type LNTagPrefix = Value
  val paymentHash     = Value("p")
  val description     = Value("d")
  val signaturePubKey = Value("n")
  val descriptionHash = Value("h")
  val expiryTime      = Value("x")
  val ctvlExpiry      = Value("c")
  val fallbackAddress = Value("f")
  val routingInfo     = Value("r")
}

sealed abstract class LNInvoiceTags {
  def paymentHash: String

  def description: Option[String]

  def signaturePubKey: Option[String] //TODO: Unimplemented

  def descriptionHash: Option[String]

  def expiryTime: Option[Int]

  def cltvExpiry: Option[Int] //TODO: Unimplemented

  def fallbackAddress: Option[String] //TODO: Work-In-Progress. Add address Type

  def routingInfo: Option[String] //TODO: Unimplemented

  def toUnsigned(byte:Byte):Int = {
    byte & 0xFF
  }

  def toBech32String:String = {
    Bech32EncodeHex(LNTagPrefix.paymentHash, paymentHash) +
      Bech32EncodeString(LNTagPrefix.description, description) +
      Bech32EncodeString(LNTagPrefix.signaturePubKey, signaturePubKey) +
      Bech32EncodeHex(LNTagPrefix.descriptionHash, descriptionHash.getOrElse("")) +
      Bech32EncodeInt(LNTagPrefix.expiryTime, expiryTime) +
      Bech32EncodeInt(LNTagPrefix.ctvlExpiry, cltvExpiry) +
      Bech32EncodeHex(LNTagPrefix.fallbackAddress, fallbackAddress.getOrElse("")) +
      Bech32EncodeString(LNTagPrefix.routingInfo, routingInfo)
  }

  def Bech32EncodeString(prefix:LNTagPrefix, tag:Option[String]):String = {
    if (tag.isDefined) {
      val stringUInt8 = tag.get.map(a => a.toByte).map(i => UInt8(toUnsigned(i).toShort))
      val stringBase5 = Bech32Address.encode(stringUInt8)
      val bech32String = Bech32Address.encodeToString(stringBase5.get)
      val bech32DataLength = Bech32CalcDataLength(bech32String)
      prefix.toString + bech32DataLength + bech32String
    }
    else { "" }
  }

  def Bech32EncodeHex(prefix:LNTagPrefix, tag:String):String = {
    if (!tag.isEmpty) {
      val tagUInt8 = BitcoinSUtil.decodeHex(tag).map(i => UInt8(toUnsigned(i).toShort))
      val tagBase5 = Bech32Address.encode(tagUInt8).get
      val bech32String = Bech32Address.encodeToString(tagBase5)
      val bech32DataLength = Bech32CalcDataLength(bech32String)
      prefix.toString + bech32DataLength + bech32String
    } else { "" }
  }

  def Bech32EncodeInt(prefix:LNTagPrefix, tag:Option[Int]):String = {
    if (tag.isDefined) {
      val inputBinary = tag.get.toBinaryString
      val inputPadLength = inputBinary.length + (5 - (inputBinary.length % 5)) //Get the nearest number that is divisible by 5.
      val inputPaddedBinary = inputBinary.reverse.padTo(inputPadLength, "0").reverse.grouped(5).toList //Pad to the nearest number that is divisible by 5.
      val inputBinaryToInt = inputPaddedBinary.map(s => Integer.parseInt(s.mkString, 2))
      val inputUInt8 = inputBinaryToInt.map(i => UInt8(i.toShort))
      val bech32String = Bech32Address.encodeToString(inputUInt8)
      val bech32DataLength = Bech32CalcDataLength(bech32String)
      prefix.toString + bech32DataLength + bech32String
    }
    else { "" }
  }

  def Bech32CalcDataLength(Bech32EncodedString:String):String = {
    val dataLengthMultiplier = Bech32EncodedString.length / 32
    val dataLength = Bech32EncodedString.length % 32
    Bech32Address.encodeToString(Seq(UInt8(dataLengthMultiplier.toShort), UInt8(dataLength.toShort)))
  }
}

case class LNTags(paymentHash: String, description: Option[String], signaturePubKey: Option[String],
                  descriptionHash: Option[String],
                  expiryTime: Option[Int], cltvExpiry: Option[Int],
                  fallbackAddress: Option[String], routingInfo: Option[String]) extends LNInvoiceTags {
  //TODO: Add additional checking to verify that description is <640 bytes. If over, you must use descriptionHash
  require(description.nonEmpty || descriptionHash.nonEmpty, "You must supply an invoice description, or description hash in order to create an invoice.")

}
