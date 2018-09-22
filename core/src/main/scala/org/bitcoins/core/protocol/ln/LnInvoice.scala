package org.bitcoins.core.protocol.ln

import org.bitcoins.core.crypto.ECDigitalSignature
import org.bitcoins.core.number.{ UInt32, UInt5, UInt64, UInt8 }
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.util._
import org.slf4j.LoggerFactory
import scodec.bits.ByteVector

sealed abstract class LnInvoice {
  require(
    timestamp < UInt64(NumberUtil.pow2(35)),
    s"timestamp ${timestamp.toBigInt} < ${NumberUtil.pow2(35)}")

  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)
  val bech32Separator: Char = Bech32.separator

  def hrp: LnHumanReadablePart

  private def data: Vector[UInt5] = {
    val u5s: Vector[UInt5] = bech32TimeStamp ++ lnTags.data ++ signature.data
    u5s
  }

  def network: LnParams = hrp.network

  def amount: Option[LnCurrencyUnit] = hrp.amount

  def timestamp: UInt64

  def lnTags: LnInvoiceTaggedFields

  def signature: LnInvoiceSignature

  def bech32Checksum: String = {
    val bytes: Vector[UInt5] = LnInvoice.createChecksum(hrp, data)
    val bech32 = Bech32.encode5bitToString(bytes)
    bech32
  }

  //TODO: Refactor Into Bech32Address?
  def uInt64ToBase32(input: UInt64): Vector[UInt5] = {
    //To fit a UInt64 value, we need at most ceil(64 / 5) = 13 groups of 5 bits.
    val arr: Array[Int] = new Array[Int](13)
    for (x <- 0 to 12) {
      arr(x) = (input >> x * 5 & UInt64(0x1F)).toInt.toByte
    }
    arr.reverse.dropWhile(_ == 0).map(b => UInt5(b)).toVector
  }

  private def bech32Signature: String = {
    val signatureBase32 = signature.data
    Bech32.encode5bitToString(signatureBase32)
  }

  private def bech32TimeStamp: Vector[UInt5] = {
    val tsB32 = uInt64ToBase32(timestamp)
    tsB32
  }

  override def toString: String = {
    val b = new StringBuilder
    b.append(hrp.toString)
    b.append(bech32Separator)

    val dataToString = Bech32.encode5bitToString(data)
    b.append(dataToString)
    b.append(bech32Checksum)

    b.toString()
  }
}

object LnInvoice {

  def hrpExpand(lnHumanReadablePart: LnHumanReadablePart): Vector[UInt5] = {
    val bytes = lnHumanReadablePart.bytes
    val u5s = Bech32.hrpExpand(bytes)
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