package org.bitcoins.core.protocol.ln

import org.bitcoins.core.crypto.{ ECPublicKey, Sha256Digest }
import org.bitcoins.core.number.{ UInt32, UInt5, UInt8 }
import org.bitcoins.core.protocol._
import org.bitcoins.core.protocol.ln.LnInvoiceTag.{ PaymentHashTag, encodeNumber }
import org.bitcoins.core.protocol.ln.routing.LnRoute
import org.bitcoins.core.util.Bech32
import org.slf4j.LoggerFactory
import scodec.bits.ByteVector

import scala.collection.mutable

/**
 * One of the tagged fields on a Lightning Network invoice
 * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#tagged-fields]]
 */
sealed abstract class LnInvoiceTag {

  def prefix: LnTagPrefix

  def prefixUInt5: UInt5 = {
    val char = Bech32.charset.indexOf(prefix.value)
    UInt5(char.toByte)
  }

  /** The payload for the tag without any meta infromation encoded with it */
  def encoded: Vector[UInt5]

  def data: Vector[UInt5] = {
    val len = encodeNumber(encoded.length)
    prefixUInt5 +: (len ++ encoded)
  }

  override def toString: String = {
    val b = new mutable.StringBuilder

    val dataBech32 = Bech32.encode5bitToString(data)

    b.append(prefix.toString)
    b.append(dataBech32)

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
  def dataLength(bech32String: String): Vector[UInt5] = {
    val e = encodeNumber(bech32String.length)
    e
  }

  private def u32ToU5(u32: UInt32): Vector[UInt5] = {
    val encoded = encodeNumber(u32.toLong)
    require(encoded.size == 2, s"u32ToU5 ${encoded} ${encoded.size}")
    encoded
  }

  /** Returns a 5bit bytevector with the encoded number for a ln invoice */
  def encodeNumber(len: Long): Vector[UInt5] = {
    val quotient = UInt5(len / 32)
    val remainder = UInt5(len % 32)
    val v = Vector(quotient, remainder)
    v
  }

  case class PaymentHashTag(hash: Sha256Digest) extends LnInvoiceTag {

    override val prefix: LnTagPrefix = LnTagPrefix.PaymentHash

    override val encoded: Vector[UInt5] = {
      Bech32.from8bitTo5bit(hash.bytes)
    }
  }

  case class DescriptionTag(string: String) extends LnInvoiceTag {
    override val prefix: LnTagPrefix = LnTagPrefix.Description

    override val encoded: Vector[UInt5] = {
      val bytes = ByteVector(string.getBytes("UTF-8"))
      Bech32.from8bitTo5bit(bytes)
    }

  }

  case class NodeIdTag(pubKey: ECPublicKey) extends LnInvoiceTag {

    override val prefix: LnTagPrefix = LnTagPrefix.NodeId

    override val encoded: Vector[UInt5] = {
      Bech32.from8bitTo5bit(pubKey.bytes)
    }
  }

  case class DescriptionHashTag(hash: Sha256Digest) extends LnInvoiceTag {
    override val prefix: LnTagPrefix = LnTagPrefix.DescriptionHash

    override val encoded: Vector[UInt5] = {
      Bech32.from8bitTo5bit(hash.bytes)
    }
  }

  /** The amount in seconds until this payment request expires */
  case class ExpiryTimeTag(u32: UInt32) extends LnInvoiceTag {
    override val prefix: LnTagPrefix = LnTagPrefix.ExpiryTime

    override val encoded: Vector[UInt5] = {
      u32ToU5(u32)
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

    override val encoded: Vector[UInt5] = {
      u32ToU5(u32)
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

    override val encoded: Vector[UInt5] = {
      val b = address.hash.bytes
      val u5s = version.toUInt5 +: Bech32.from8bitTo5bit(b)
      u5s
    }
  }

  case class RoutingInfo(routes: Vector[LnRoute]) extends LnInvoiceTag {

    override val prefix: LnTagPrefix = LnTagPrefix.RoutingInfo

    override val encoded: Vector[UInt5] = {
      val serializedRoutes: ByteVector = {
        routes.foldLeft(ByteVector.empty)(_ ++ _.bytes)
      }

      val u5s = Bech32.from8bitTo5bit(serializedRoutes)

      //encodeNumber(u5s.length) ++ u5s
      u5s
    }
  }
}