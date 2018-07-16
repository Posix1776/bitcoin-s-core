package org.bitcoins.core.protocol

import org.bitcoins.core.config._
import org.bitcoins.core.crypto.ECDigitalSignature
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.number.UInt8
import org.bitcoins.core.util._


sealed abstract class LightningInvoice {

  private def logger = BitcoinSLogger.logger

  val bech32Separator: Int = 1

  def network: NetworkParameters

  type Amount = (Int, CurrencyUnit) //e.g, 2500u
  def amount: Option[Amount]

  def timestamp: Long = 1496314658 //This is the timestamp used in the examples, in production we would get a current timestamp --> System.currentTimeMillis / 1000

  def lnTags: LNInvoiceTags

  type Signature = (ECDigitalSignature, Int)
  def signature: Signature

  def bech32Checksum: String = "" //TODO: Unimplemented. See Bech32Address.createChecksum

  def networkPrefix: String = LightningNetworkPrefix.fromNetworkParameters(network).get.value

  def multiplier: String =  amount.get._2 match {
    //TODO: THIS IS ONLY FOR TESTING. Additional CurrencyUnits need to be added to support LN.
    case CurrencyUnits.oneMBTC  => "u"
    case CurrencyUnits.oneBTC   => "m"
  }

  def invoiceAmount: String = { if(amount.nonEmpty){ amount.get._1 + multiplier } else { "" } }

  def bech32TimeStamp: String = {
    val paddedBinary = timestamp.toBinaryString.reverse.padTo(35, "0").reverse.grouped(5).toList
    val timeBinaryToInt = paddedBinary.map(s => Integer.parseInt(s.mkString, 2))
    val timeUInt8 = timeBinaryToInt.map(i => UInt8(i.toShort))
    Bech32Address.encodeToString(timeUInt8)
  }

  def bech32EncodedSignature: String = {
    val sigRecoveryHex = if(signature._2 > 0) { "%02d".format(signature._2)} else { "" } //Hex value of recovery bit, padded to ##.
    val sigFullHex = signature._1.hex + sigRecoveryHex
    val sigHexPaddedBinary = BigInt(sigFullHex, 16).toString(2).reverse.padTo(512, "0").reverse.padTo(520, "0").grouped(5).toList //TODO: Fix this
    val sigHexBinaryToInt = sigHexPaddedBinary.map(s => Integer.parseInt(s.mkString, 2))
    val sigHexUInt8 = sigHexBinaryToInt.map(i => UInt8(i.toShort))
    Bech32Address.encodeToString(sigHexUInt8)
  }

  def invoice: String = { networkPrefix + invoiceAmount + bech32Separator + bech32TimeStamp + lnTags.toBech32String + bech32EncodedSignature +  bech32Checksum }

}

sealed abstract class LNInvoiceTags {
  def paymentHash: String

  def description: Option[String]

  def signaturePubKey: Option[String]

  def descriptionHash: Option[String]

  def expiryTime: Option[Int]

  def cltvExpiry: Option[Int]

  type fallbackAddress = (Int,String)
  def fallbackAddress: Option[fallbackAddress]

  def routingInfo: Option[Seq[LNRoutingInfo]] //TODO: Implementation started. Not working.

  def toUnsigned(byte:Byte): Int = {
    byte & 0xFF
  }

  def toBech32String: String = {
    bech32EncodeHex(LNTagPrefix.PaymentHash, paymentHash) +
    bech32EncodeString(LNTagPrefix.Description, description) +
    bech32EncodeHex(LNTagPrefix.SignaturePubKey, signaturePubKey.getOrElse("")) +
    bech32EncodeHex(LNTagPrefix.DescriptionHash, descriptionHash.getOrElse("")) +
    bech32EncodeInt(LNTagPrefix.ExpiryTime, expiryTime) +
    bech32EncodeCltv(LNTagPrefix.CtvlExpiry, cltvExpiry) +
    toBech32FallBackAddress(LNTagPrefix.FallbackAddress, fallbackAddress) +
    toBech32RoutingInfo(LNTagPrefix.RoutingInfo, routingInfo)
  }

  def toBech32RoutingInfo(prefix:LNTagPrefix, routingInfo:Option[Seq[LNRoutingInfo]]): String = {
    if(!routingInfo.isEmpty) {
      var routing = ""
      for(route <- routingInfo.get) {
        /** Enter at your own risk. Magic numbers and incorrect encoding ahead. **/
        val nodeId = route.pubkey
        val shortId = route.shortChannelID
        val feeBasedMsat = route.feeBaseMsat
        val feePropMilli = route.feePropMilli
        val cltvExpiry = route.cltvExpiryDelta

        val nodeIdUInt8 = BitcoinSUtil.decodeHex(nodeId).map(i => UInt8(toUnsigned(i).toShort))
        val shortIdPaddedBinary = BigInt(shortId, 16).toString(2).reverse.padTo(63, "0").reverse.grouped(5).toList //Pad to 63 as defined in bolt11
        val shortIdBinaryToInt = shortIdPaddedBinary.map(s => Integer.parseInt(s.mkString, 2))
        val shortIdUInt8 = shortIdBinaryToInt.map(i => UInt8(i.toShort))

        val feeBasedMsatUInt8 = intToUInt8(feeBasedMsat, 30, 0)
        val feePropMilliUInt8 = intToUInt8(feePropMilli, 32, 35)
        val cltvExpiryUInt8 = intToUInt8(cltvExpiry, 13, 15)

        val nodeIdBase5 = Bech32Address.encode(nodeIdUInt8).get
        val nodeIdBech32 = Bech32Address.encodeToString(nodeIdBase5)
        val shortIdBech32 = Bech32Address.encodeToString(shortIdUInt8)

        routing += "-routing:" + nodeIdBech32 + "_" + shortIdBech32 + "_" + Bech32Address.encodeToString(feeBasedMsatUInt8) + "_" + Bech32Address.encodeToString(feePropMilliUInt8) + "_" + Bech32Address.encodeToString(cltvExpiryUInt8)
      }
      routing
    }
    else { "" }
  }

  def intToUInt8(input:Int, paddingLeft:Int, paddingRight:Int): Seq[UInt8] = {
    val paddedBinary = input.toBinaryString.reverse.padTo(paddingLeft, "0").reverse.padTo(paddingRight, "0").grouped(5).toList
    val binaryToInt = paddedBinary.map(s => Integer.parseInt(s.mkString, 2))
    binaryToInt.map(i => UInt8(i.toShort))
  }

  def getPaddedInt(number:Int): List[Int] = {
    //Pad Binary to nearest length that is divisible by 5.
    val numberInBinary = number.toBinaryString
    var paddingLength = numberInBinary.length
    if(numberInBinary.length % 5 > 0) { paddingLength += (5 - (numberInBinary.length % 5)) }
    val paddedBinary = numberInBinary.reverse.padTo(paddingLength, "0").reverse.grouped(5).toList
    paddedBinary.map(s => Integer.parseInt(s.mkString, 2))
  }

  def toBech32FallBackAddress(prefix:LNTagPrefix, tag:Option[fallbackAddress]): String = {
    if (!tag.isEmpty) {
      val addrType = tag.get._1
      val address  = tag.get._2

      val addrTypeUInt8        = getPaddedInt(addrType).map(i => UInt8(i.toShort))
      val addrTypeBech32String = Bech32Address.encodeToString(addrTypeUInt8)

      val addressUInt8 = BitcoinSUtil.decodeHex(address).map(i => UInt8(toUnsigned(i).toShort))
      val addressBase5 = Bech32Address.encode(addressUInt8).get
      val addressBech32String = Bech32Address.encodeToString(addressBase5)

      val bech32DataLength = bech32CalcDataLength(addrTypeBech32String + addressBech32String)
      prefix.value + bech32DataLength + addrTypeBech32String + addressBech32String
    } else { "" }
  }

  def bech32EncodeString(prefix:LNTagPrefix, tag:Option[String]): String = {
    if (tag.isDefined) {
      val stringUInt8 = tag.get.map(a => a.toByte).map(i => UInt8(toUnsigned(i).toShort))
      val stringBase5 = Bech32Address.encode(stringUInt8)
      val bech32String = Bech32Address.encodeToString(stringBase5.get)
      val bech32DataLength = bech32CalcDataLength(bech32String)
      prefix.value + bech32DataLength + bech32String
    }
    else { "" }
  }

  def bech32EncodeHex(prefix:LNTagPrefix, tag:String): String = {
    if (!tag.isEmpty) {
      val tagUInt8 = BitcoinSUtil.decodeHex(tag).map(i => UInt8(toUnsigned(i).toShort))
      val tagBase5 = Bech32Address.encode(tagUInt8).get
      val bech32String = Bech32Address.encodeToString(tagBase5)
      val bech32DataLength = bech32CalcDataLength(bech32String)
      prefix.value + bech32DataLength + bech32String
    } else { "" }
  }

  def bech32EncodeInt(prefix:LNTagPrefix, tag:Option[Int]): String = {
    if (tag.isDefined) {
      val inputUInt8 = getPaddedInt(tag.get).map(i => UInt8(i.toShort))
      val bech32String = Bech32Address.encodeToString(inputUInt8)
      val bech32DataLength = bech32CalcDataLength(bech32String)
      prefix.value + bech32DataLength + bech32String
    }
    else { "" }
  }

  def bech32CalcDataLength(Bech32EncodedString:String): String = {
    val dataLengthMultiplier = Bech32EncodedString.length / 32
    val dataLength = Bech32EncodedString.length % 32
    Bech32Address.encodeToString(Seq(UInt8(dataLengthMultiplier.toShort), UInt8(dataLength.toShort)))
  }

  def bech32EncodeCltv(prefix:LNTagPrefix, tag:Option[Int]): String = {
    if(!tag.isEmpty) {
      val paddedBinary = tag.get.toBinaryString.reverse.padTo(10, "0").reverse.grouped(5).toList
      val ctlvBinaryToInt = paddedBinary.map(s => Integer.parseInt(s.mkString, 2))
      val ctlvUInt8 = ctlvBinaryToInt.map(i => UInt8(i.toShort))
      val bech32String = Bech32Address.encodeToString(ctlvUInt8)
      val bech32DataLength = bech32CalcDataLength(bech32String)
      prefix.value + bech32DataLength + bech32String
    }
    else { "" }
  }

}

case class LNInvoice(network: NetworkParameters, amount:Option[(Int, CurrencyUnit)], lnTags: LNInvoiceTags, signature: (ECDigitalSignature, Int)) extends LightningInvoice {

}

case class LNTags(paymentHash: String, description: Option[String], signaturePubKey: Option[String], descriptionHash: Option[String],
                  expiryTime: Option[Int], cltvExpiry: Option[Int], fallbackAddress: Option[(Int,String)],
                  routingInfo: Option[Seq[LNRoutingInfo]]) extends LNInvoiceTags {
    require((description.nonEmpty && description.get.length < 640) || descriptionHash.nonEmpty, "You must supply either a description hash, or a literal description that is 640 characters or less to create an invoice.")
}

case class LNRoutingInfo(pubkey:String, shortChannelID:String,feeBaseMsat:Int,feePropMilli:Int,cltvExpiryDelta:Int) {

}
