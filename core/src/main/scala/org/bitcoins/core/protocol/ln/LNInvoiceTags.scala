package org.bitcoins.core.protocol.ln

import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.util.BitcoinSUtil
import org.bitcoins.core.number.{UInt64, UInt8}

sealed abstract class LNInvoiceTags { //TODO: Review Variable names, standardize bech and format
  def paymentHash: String

  def description: Option[String]

  def signaturePubKey: Option[String]

  def descriptionHash: Option[String]

  def expiryTime: Option[UInt64]

  def cltvExpiry: Option[UInt64]

  type fallbackAddress = (Int, String) //TODO: Change to UInt64?
  def fallbackAddress: Option[fallbackAddress]

  def routingInfo: Option[Seq[LNRoutingInfo]] //TODO: Implementation started. Not working.

  def toUnsigned(byte: Byte): Int = { //TODO: Refactor Into Bech32Address
    byte & 0xFF
  }

  def toBech32String: String = {
    bech32EncodeHex(LNTagPrefix.PaymentHash, paymentHash) +
    bech32EncodeString(LNTagPrefix.Description, description) +
    bech32EncodeHex(LNTagPrefix.SignaturePubKey, signaturePubKey.getOrElse("")) +
    bech32EncodeHex(LNTagPrefix.DescriptionHash, descriptionHash.getOrElse("")) +
    bech32EncodeUInt64(LNTagPrefix.ExpiryTime, expiryTime) +
    bech32EncodeUInt64(LNTagPrefix.CtvlExpiry, cltvExpiry) +
    toBech32FallBackAddress(LNTagPrefix.FallbackAddress, fallbackAddress) +
    toBech32RoutingInfo(LNTagPrefix.RoutingInfo, routingInfo)
  }

  val shortIdBase32Len = 15     //TODO: Add spec
  val feeBasedMsatBase32Len = 7
  val feePropMilliBase32Len = 7
  val ctlvBase32Len = 4
  def toBech32RoutingInfo(prefix: LNTagPrefix, routingInfo: Option[Seq[LNRoutingInfo]]): String = {
    if (!routingInfo.isEmpty) {
      var routing = ""
      for (route <- routingInfo.get) {
        /** Enter at your own risk. **/
        val nodeId = route.pubkey
        val shortId = route.shortChannelID
        val feeBasedMsat = route.feeBaseMsat //TODO: Convert these to UInt64?
        val feePropMilli = route.feePropMilli
        val cltvExpiry = route.cltvExpiryDelta

        val nodeIdUInt8 = base32FromHex(nodeId)
        val shortIdUInt8 = padBase32(base32FromHex(shortId), shortIdBase32Len)
        val feeBasedMsatUInt8 = padBase32(base32FromUInt64(UInt64(feeBasedMsat)),feeBasedMsatBase32Len)
        val feePropMilliUInt8 = padBase32(base32FromUInt64(UInt64(feePropMilli)),feePropMilliBase32Len)
        val cltvExpiryUInt8 = padBase32(base32FromUInt64(UInt64(cltvExpiry)), ctlvBase32Len)
        val nodeIdBech32 = Bech32Address.encodeToString(nodeIdUInt8)
        val shortIdBech32 = Bech32Address.encodeToString(shortIdUInt8)
        println(Bech32Address.encodeToString(nodeIdUInt8 ++ shortIdUInt8 ++ feeBasedMsatUInt8 ++ feePropMilliUInt8 ++ cltvExpiryUInt8))
        routing += "-routing:" + nodeIdBech32 + "_" + shortIdBech32 + "_" + Bech32Address.encodeToString(feeBasedMsatUInt8) + "_" + Bech32Address.encodeToString(feePropMilliUInt8) + "_" + Bech32Address.encodeToString(cltvExpiryUInt8) + "!!!!"

        //TESTING ZONE:
        //val nodeIdUInt8 = BitcoinSUtil.decodeHex(nodeId).map(i => UInt8(toUnsigned(i).toShort))
        //val shortIdPaddedBinary = BigInt(shortId, 16).toString(2).reverse.padTo(63, "0").reverse.grouped(5).toList //Pad to 63 as defined in bolt11
        //val shortIdBinaryToInt = shortIdPaddedBinary.map(s => Integer.parseInt(s.mkString, 2))
        //val shortIdUInt8 = shortIdBinaryToInt.map(i => UInt8(i.toShort))
        //val feeBasedMsatUInt8 = intToUInt8(feeBasedMsat, 30, 0)
        //val feePropMilliUInt8 = intToUInt8(feePropMilli, 32, 35)
        //val cltvExpiryUInt8 = intToUInt8(cltvExpiry, 13, 15)
        //val nodeIdBase5 = Bech32Address.encode(nodeIdUInt8).get
        //val nodeIdBech32 = Bech32Address.encodeToString(nodeIdBase5)
        //val shortIdBech32 = Bech32Address.encodeToString(shortIdUInt8)
      }
      routing
    } else { "" }
  }

  def intToUInt8(input: Int, paddingLeft: Int, paddingRight: Int): Seq[UInt8] = {
    val paddedBinary = input.toBinaryString.reverse.padTo(paddingLeft, "0").reverse.padTo(paddingRight, "0").grouped(5).toList
    val binaryToInt = paddedBinary.map(s => Integer.parseInt(s.mkString, 2))
    binaryToInt.map(i => UInt8(i.toShort))
  }

  def toBech32FallBackAddress(prefix: LNTagPrefix, tag: Option[fallbackAddress]): String = {
    println("ok")
    if (!tag.isEmpty) {
      val addrType = tag.get._1
      val address = tag.get._2

      val bech32AddType = base32FromUInt64(UInt64(addrType))
      val addrTypeBech32String = Bech32Address.encodeToString(bech32AddType)

      val base32Tag = base32FromHex(address)
      val bech32String = Bech32Address.encodeToString(base32Tag)

      val bech32DataLength = bech32CalcDataLength(addrTypeBech32String + bech32String)
      "---" + prefix.value + bech32DataLength + addrTypeBech32String + bech32String + "---"

      //TESTING ZONE:
      //val addrTypeUInt8 = getPaddedInt(addrType).map(i => UInt8(i.toShort))
      //val addrTypeBech32String = Bech32Address.encodeToString(addrTypeUInt8)
      //val addressUInt8 = BitcoinSUtil.decodeHex(address).map(i => UInt8(toUnsigned(i).toShort))
      //val addressBase5 = Bech32Address.encode(addressUInt8).get
      //val addressBech32String = Bech32Address.encodeToString(addressBase5)
    } else { "" }
  }

  def bech32EncodeString(prefix: LNTagPrefix, tag: Option[String]): String = {
    if (tag.isDefined) {
      val stringUInt8 = tag.get.map(a => a.toByte).map(i => UInt8(toUnsigned(i).toShort))
      val stringBase5 = Bech32Address.encode(stringUInt8)
      val bech32String = Bech32Address.encodeToString(stringBase5.get)
      val bech32DataLength = bech32CalcDataLength(bech32String)
      prefix.value + bech32DataLength + bech32String
    } else { "" }
  }

  def bech32EncodeHex(prefix: LNTagPrefix, tag: String): String = {
    if (!tag.isEmpty) {
      val base32Tag = base32FromHex(tag)
      val bech32String = Bech32Address.encodeToString(base32Tag)
      val bech32DataLength = bech32CalcDataLength(bech32String)
      prefix.value + bech32DataLength + bech32String
      //TESTING ZONE:
      //val tagUInt8 = BitcoinSUtil.decodeHex(tag).map(i => UInt8(toUnsigned(i).toShort))
      //val tagBase5 = Bech32Address.encode(tagUInt8).get
    } else { "" }
  }

  def bech32EncodeUInt64(prefix: LNTagPrefix, tag: Option[UInt64]): String = {
    if (tag.isDefined) {
      val base32UInt64 = base32FromUInt64(tag.get)
      val bech32String = Bech32Address.encodeToString(base32UInt64)
      val bech32DataLength = bech32CalcDataLength(bech32String)
      prefix.value + bech32DataLength + bech32String
    } else { "" }
  }

  def bech32CalcDataLength(Bech32EncodedString: String): String = {
    val dataLengthMultiplier = Bech32EncodedString.length / 32
    val dataLength = Bech32EncodedString.length % 32
    Bech32Address.encodeToString(Seq(UInt8(dataLengthMultiplier.toShort), UInt8(dataLength.toShort)))
  }

  def base32FromUInt64(inputNum:UInt64): Seq[UInt8] = {
    if(inputNum > UInt64.zero) {
      // To fit an UInt64, we need at most ceil(64 / 5) = 13 groups of 5 bits.
      val base32Arr: Array[Byte] = new Array[Byte](13)
      var group = 13
      var number = inputNum

      while (number > UInt64.zero) {
        group -= 1
        base32Arr(group) = (number & UInt64(31)).toInt.toByte
        number = number >> 5
      }
      base32Arr.dropWhile(_ == 0).map(b => UInt8(b)) //Drop leading 0's and return Seq[UInt8]
    }
    else { Seq() }
  }

  def base32FromHex(hex:String): Seq[UInt8] = {
    val byteArr = BitcoinSUtil.decodeHex(hex).map(b => UInt8(toUnsigned(b)))
    Bech32Address.encode(byteArr).get
  }

  def padBase32(base32: Seq[UInt8], padTo:Int) = {
    var arr = base32
    while (arr.length < padTo) {
      arr = arr :+ UInt8(0)
    }
    arr
  }

  def getUINT8s: Seq[UInt8] = toUINT8s(LNTagPrefix.PaymentHash, paymentHash, LNTagPrefix.Description, description) //Test Method

  def toUINT8s(prefixHash: LNTagPrefix, tagHash: String, prefixDesc: LNTagPrefix, tagDesc: Option[String]): Seq[UInt8] = {
    var UINTSHASH: Seq[UInt8] = Nil
    var UINTSDESC: Seq[UInt8] = Nil
    if (!tagHash.isEmpty) {
      val tagUInt8 = BitcoinSUtil.decodeHex(tagHash).map(i => UInt8(toUnsigned(i).toShort))
      val tagBase5 = Bech32Address.encode(tagUInt8).get
      UINTSHASH = tagBase5
    }

    if (tagDesc.isDefined) {
      val stringUInt8 = tagDesc.get.map(a => a.toByte).map(i => UInt8(toUnsigned(i).toShort))
      val stringBase5 = Bech32Address.encode(stringUInt8).get
      UINTSDESC = stringBase5
    }
    UINTSHASH ++ UINTSDESC
  }
}

case class LNTags(paymentHash: String, description: Option[String], signaturePubKey: Option[String], descriptionHash: Option[String],
  expiryTime: Option[UInt64], cltvExpiry: Option[UInt64], fallbackAddress: Option[(Int, String)],
  routingInfo: Option[Seq[LNRoutingInfo]]) extends LNInvoiceTags {
  require((description.nonEmpty && description.get.length < 640) || descriptionHash.nonEmpty, "You must supply either a description hash, or a literal description that is 640 characters or less to create an invoice.")
}

//Remove
/*def getPaddedInt(number: Int): List[Int] = {
  //Pad Binary to nearest length that is divisible by 5.
  val numberInBinary = number.toBinaryString
  var paddingLength = numberInBinary.length
  if (numberInBinary.length % 5 > 0) { paddingLength += (5 - (numberInBinary.length % 5)) }
  val paddedBinary = numberInBinary.reverse.padTo(paddingLength, "0").reverse.grouped(5).toList
  paddedBinary.map(s => Integer.parseInt(s.mkString, 2))
}*/

/*def bech32EncodeCltv(prefix: LNTagPrefix, tag: Option[Int]): String = {
  if (!tag.isEmpty) {
    //val paddedBinary = tag.get.toBinaryString.reverse.padTo(10, "0").reverse.grouped(5).toList
    //val ctlvBinaryToInt = paddedBinary.map(s => Integer.parseInt(s.mkString, 2))
    //val ctlvUInt8 = ctlvBinaryToInt.map(i => UInt8(i.toShort))
    val base32 = base32FromUInt64(UInt64(tag.get))
    val bech32String = Bech32Address.encodeToString(base32)
    val bech32DataLength = bech32CalcDataLength(bech32String)
    prefix.value + "-" + bech32DataLength + "-" + bech32String
  } else { "" }
}*/