package org.bitcoins.core.protocol.ln

import org.bitcoins.core.crypto.ECDigitalSignature
import org.bitcoins.core.number.{ UInt5, UInt64, UInt8 }
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.util._
import org.slf4j.LoggerFactory
import scodec.bits.ByteVector

sealed abstract class LnInvoice {
  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)
  val bech32Separator: Char = Bech32.separator

  def hrp: LnHumanReadablePart

  def network: LnParams = hrp.network

  def amount: Option[LnCurrencyUnit] = hrp.amount

  def timestamp: UInt64

  def lnTags: LnInvoiceTaggedFields

  def signature: LnInvoiceSignature

  def bech32Checksum: String = {
    //note this is a 5bit bytevector
    val bytes = LnInvoice.createChecksum(hrp, data)
    val bech32 = Bech32.encode5bitToString(bytes)
    bech32
  }

  //TODO: Refactor Into Bech32Address?
  def uInt64ToBase32(input: UInt64): Vector[UInt8] = {
    //To fit a UInt64 value, we need at most ceil(64 / 5) = 13 groups of 5 bits.
    val arr: Array[Int] = new Array[Int](13)
    for (x <- 0 to 12) {
      arr(x) = (input >> x * 5 & UInt64(0x1F)).toInt.toByte
    }
    arr.reverse.dropWhile(_ == 0).map(b => UInt8(b)).toVector
  }

  private def bech32Signature: String = {
    val signatureBase32 = UInt5.toUInt5s(signature.bytes)
    Bech32.encode5bitToString(signatureBase32)
  }

  private def bech32TimeStamp: String = {
    val tsB32 = UInt5.toUInt5s(timestamp.bytes)
    Bech32.encode5bitToString(tsB32)
  }

  private def data: Vector[UInt5] = {
    val bytes: ByteVector = timestamp.bytes ++ lnTags.bytes ++ signature.bytes
    val u5s = UInt5.toUInt5s(bytes)
    u5s
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

object LnInvoice {

  def hrpExpand(lnHumanReadablePart: LnHumanReadablePart): Vector[UInt5] = {
    val u5s = lnHumanReadablePart.bytes
    u5s
  }

  def createChecksum(hrp: LnHumanReadablePart, data: Vector[UInt5]): Vector[UInt5] = {
    val hrpBytes = hrpExpand(hrp)
    val u5s = Bech32.createChecksum(hrpBytes ++ data)
    u5s
  }
}

case class Invoice(hrp: LnHumanReadablePart, timestamp: UInt64, lnTags: LnInvoiceTaggedFields,
  signature: LnInvoiceSignature) extends LnInvoice