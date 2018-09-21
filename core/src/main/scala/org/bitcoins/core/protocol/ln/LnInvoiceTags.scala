package org.bitcoins.core.protocol.ln

import org.bitcoins.core.crypto.{ ECPublicKey, Sha256Digest }
import org.bitcoins.core.number.{ UInt32, UInt5, UInt8 }
import org.bitcoins.core.protocol._
import org.bitcoins.core.protocol.ln.LnInvoiceTag.PaymentHashTag
import org.bitcoins.core.protocol.ln.routing.LnRoute
import org.bitcoins.core.util.Bech32
import org.slf4j.LoggerFactory
import scodec.bits.ByteVector

import scala.collection.mutable

/**
 * One of the tagged fields on a Lightning Network invoice
 * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#tagged-fields]]
 */
sealed abstract class LnInvoiceTag extends NetworkElement {

  def prefix: LnTagPrefix

  /** Converts the data portion of the LnInvoiceTag to a bech32 string */
  def dataToBech32: String = {
    Bech32.encode8bitToString(bytes)
  }

  override def toString: String = {
    val b = new mutable.StringBuilder

    val data = dataToBech32
    val dataLen = LnInvoiceTag.dataLength(
      bech32String = data)

    b.append(prefix.toString)
    b.append(dataLen)
    b.append(dataToBech32)

    b.toString()
  }
}

/**
 * All of the different invoice tags that are currently defined
 * Refer to BOLT11 for a full list
 * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#tagged-fields]]
 */
object LnInvoiceTag {
  private val logger = LoggerFactory.getLogger(this.getClass.getName)
  /**
   * The formula for this calculation is as follows:
   * Take the length of the Bech32 encoded input and divide it by 32.
   * Take the quotient, and encode this value as Bech32. Take the remainder and encode this value as Bech32.
   * Append these values to produce a valid Lighting Network data_length field.
   * Please see Bolt-11 for examples:
   * https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#examples
   */
  def dataLength(bech32String: String): String = {
    val e = encodeNumber(bech32String.length)
    Bech32.encode5bitToString(e)
  }

  /** Returns a 5bit bytevector with the encoded number for a ln invoice */
  def encodeNumber(len: Long): Vector[UInt5] = {
    val quotient = len / 32
    val remainder = len % 32
    val v = Vector(UInt5(quotient.toByte), UInt5(remainder.toByte))
    v
  }

  case class PaymentHashTag(hash: Sha256Digest) extends LnInvoiceTag {

    override val prefix: LnTagPrefix = LnTagPrefix.PaymentHash

    override val bytes = hash.bytes
  }

  case class DescriptionTag(string: String) extends LnInvoiceTag {
    override val prefix: LnTagPrefix = LnTagPrefix.Description

    override val bytes: ByteVector = ByteVector(string.getBytes("UTF-8"))

  }

  case class NodeIdTag(pubKey: ECPublicKey) extends LnInvoiceTag {

    override val prefix: LnTagPrefix = LnTagPrefix.NodeId

    //don't think this serialization is currently correct, per BOLT11
    //Fallback on-chain address: for bitcoin, this starts with a 5-bit version
    //and contains a witness program or P2PKH or P2SH address.
    override val bytes: ByteVector = pubKey.bytes
  }

  case class DescriptionHashTag(hash: Sha256Digest) extends LnInvoiceTag {
    override val prefix: LnTagPrefix = LnTagPrefix.DescriptionHash

    override val bytes: ByteVector = hash.bytes
  }

  /** The amount in seconds until this payment request expires */
  case class ExpiryTimeTag(u32: UInt32) extends LnInvoiceTag {
    override val prefix: LnTagPrefix = LnTagPrefix.ExpiryTime

    override val bytes: ByteVector = {
      //encodeNumber(u32.toLong)
      u32.bytes
    }

    override def dataToBech32: String = {
      //bytes is already in base5, so need to to decode again
      val u5s = encodeNumber(u32.toLong)
      Bech32.encode5bitToString(u5s)
    }
  }

  /**
   * min_final_ctlv_expiry is the minimum difference between HTLC CLTV timeout and
   * the current block height, for the terminal case (C)
   * This is denominated in blocks
   * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/02-peer-protocol.md#cltv_expiry_delta-selection]]
   */
  case class MinFinalCltvExpiry(u32: UInt32) extends LnInvoiceTag {
    override val prefix: LnTagPrefix = LnTagPrefix.CltvExpiry

    override val bytes: ByteVector = {
      u32.bytes
    }

    override def dataToBech32: String = {
      //bytes is already in base5, so need to to decode again
      val u5 = encodeNumber(u32.toLong)
      Bech32.encode5bitToString(u5)
    }
  }

  case class FallbackAddressTag(address: Address) extends LnInvoiceTag {

    /** The version of the fallback address is indicated here in BOLT11 */
    def version: UInt8 = {
      address match {
        case _: P2PKHAddress => UInt8(17)
        case _: P2SHAddress => UInt8(18)
        case bech32: Bech32Address =>
          UInt8(bech32.scriptPubKey.witnessVersion.version.toInt)
      }
    }

    override val prefix: LnTagPrefix = LnTagPrefix.FallbackAddress

    override val bytes: ByteVector = {
      val b = UInt8.toByte(version) +: address.hash.bytes
      b
    }

    override def dataToBech32: String = {
      val b = address.hash.bytes
      val u5s = version.toUInt5 +: Bech32.from8bitTo5bit(b)
      Bech32.encode5bitToString(u5s)
    }
  }

  case class RoutingInfo(routes: Vector[LnRoute]) extends LnInvoiceTag {

    override val prefix: LnTagPrefix = LnTagPrefix.RoutingInfo

    override val bytes: ByteVector = {
      val serializedRoutes: ByteVector = {
        routes.foldLeft(ByteVector.empty)(_ ++ _.bytes)
      }
      serializedRoutes
    }
  }
}