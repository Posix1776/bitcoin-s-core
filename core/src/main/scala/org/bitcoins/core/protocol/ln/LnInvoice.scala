package org.bitcoins.core.protocol.ln

import org.bitcoins.core.crypto.ECDigitalSignature
import org.bitcoins.core.number.{ UInt64, UInt8 }
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.util._

sealed abstract class LnInvoice {
  val bech32Separator: Char = Bech32.separator

  def hrp: LnHumanReadablePart

  def network: LnParams = hrp.network

  def amount: Option[LnCurrencyUnit] = hrp.amount

  def timestamp: UInt64

  def lnTags: LnInvoiceTags

  type Signature = (ECDigitalSignature, Int)
  def signature: Signature

  def bech32Checksum: String = "" //TODO: Unimplemented. See Bech32Address.createChecksum

  //TODO: Refactor Into Bech32Address?
  def uInt64ToBase32(input: UInt64): Vector[UInt8] = {
    //To fit a UInt64 value, we need at most ceil(64 / 5) = 13 groups of 5 bits.
    val arr: Array[Int] = new Array[Int](13)
    for (x <- 0 to 12) {
      arr(x) = (input >> x * 5 & UInt64(0x1F)).toInt.toByte
    }
    arr.reverse.dropWhile(_ == 0).map(b => UInt8(b)).toVector
  }

  private def hexToBase32(hex: String): Vector[UInt8] = {
    val byteArray = BitcoinSUtil.decodeHex(hex)
    Bech32.from8bitTo5bit(byteArray)
  }

  private def bech32TimeStamp: String = {
    Bech32.encode5bitToString(uInt64ToBase32(timestamp))
  }

  private def bech32Signature: String = {
    val signatureHex = signature._1.hex + "%02d".format(signature._2) //Append version information
    val signatureBase32 = hexToBase32(signatureHex)
    Bech32.encode5bitToString(signatureBase32)
  }

  override def toString: String = {
    val b = new StringBuilder
    b.append(hrp.toString)
    b.append(bech32Separator)
    b.append(bech32TimeStamp)
    b.append(lnTags.toString)
    b.append(bech32Signature)
    b.append(bech32Checksum)

    b.toString()
  }
}

case class Invoice(hrp: LnHumanReadablePart, timestamp: UInt64, lnTags: LnInvoiceTags,
  signature: (ECDigitalSignature, Int)) extends LnInvoice