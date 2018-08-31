package org.bitcoins.core.protocol.ln

import org.bitcoins.core.config._
import org.bitcoins.core.crypto.ECDigitalSignature
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{UInt64, UInt8}
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.util._

sealed abstract class LightningInvoice {

  //TESTING ZONE:
  var timeUINT8: Seq[UInt8] = Nil
  var tagsUINT8: Seq[UInt8] = lnTags.getUINT8s
  var sigsUINT8: Seq[UInt8] = Nil
  var UINTS = timeUINT8 ++ tagsUINT8 ++ sigsUINT8

  private def logger = BitcoinSLogger.logger

  val bech32Separator: Char = Bech32Address.separator

  //BOLT-11 data length requirements
  val signatureBase32Len = 104 //520 bit field. 520 / 5 = 104
  val timestampBase32Len = 7   //35  bit field.  35 / 5 = 7

  def hrp: LnInvoiceHRP

  def network: LightningNetworkParams = hrp.network

  //let's create currency units for lightning
  //def amountOpt: Option[CurrencyUnit] = hrp.amountOpt
  //def multiplierOpt: Option[LnInvoiceMultiplier] = hrp.multiplierOpt

  def timestamp: UInt64

  def lnTags: LNInvoiceTags

  type Signature = (ECDigitalSignature, Int)
  def signature: Signature

  def bech32Checksum: String = "" //TODO: Unimplemented. See Bech32Address.createChecksum

  //TODO: Refactor Into Bech32Address
  def uInt64ToBase32(inputNum:UInt64): Seq[UInt8] = {
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

  def hexToBase32(hex:String): Seq[UInt8] = {
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

  def toUnsigned(byte: Byte): Int = {
    byte & 0xFF
  }

  def bech32TimeStamp: String = {
    val base32Timestamp = uInt64ToBase32(timestamp)
    val paddedTime = padBase32(base32Timestamp, timestampBase32Len)
    timeUINT8 = paddedTime //TESTING ZONE
    Bech32Address.encodeToString(paddedTime)
  }

  def bech32Signature: String = {
    val sigRecoveryHex = if (signature._2 > 0) { "%02d".format(signature._2) } else { "" } //Hex value of recovery bit, padded to ##.
    val sigFullHex = signature._1.hex + sigRecoveryHex
    val sigBase32 = hexToBase32(sigFullHex)
    val padBase = padBase32(sigBase32, signatureBase32Len)
    sigsUINT8 = padBase //TESTING ZONE
    Bech32Address.encodeToString(padBase)
  }

  def invoice: String = { hrp.toString + bech32Separator + bech32TimeStamp + lnTags.toBech32String + bech32Signature + bech32Checksum }

}

case class LNInvoice(
  hrp: LnInvoiceHRP,
  lnTags: LNInvoiceTags,
  timestamp: UInt64,
  signature: (ECDigitalSignature, Int)) extends LightningInvoice

case class LNRoutingInfo(pubkey: String, shortChannelID: String, feeBaseMsat: Int, feePropMilli: Int, cltvExpiryDelta: Int) {

}

