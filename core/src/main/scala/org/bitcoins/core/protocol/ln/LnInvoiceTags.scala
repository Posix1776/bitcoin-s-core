package org.bitcoins.core.protocol.ln

import org.bitcoins.core.crypto.{ ECPublicKey, Sha256Digest }
import org.bitcoins.core.number.{ UInt64, UInt8 }
import org.bitcoins.core.protocol._
import org.bitcoins.core.protocol.ln.LnInvoiceTag.PaymentHashTag
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.util.Bech32
import org.slf4j.LoggerFactory
import scodec.bits.ByteVector

sealed abstract class LnInvoiceTag extends NetworkElement {

  def prefix: LnTagPrefix

  /** Converts the data portion of the LnInvoiceTag to a bech32 string */
  def dataToBech32: String = {
    Bech32.encode8bitToString(bytes)
  }

  override def toString: String = {
    val b = new StringBuilder
    b.append(prefix)
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

  case class ExpiryTimeTag(u64: UInt64) extends LnInvoiceTag {
    override val prefix: LnTagPrefix = LnTagPrefix.ExpiryTime

    override val bytes: ByteVector = u64.bytes
  }

  case class CltvExpiryTag(u64: UInt64) extends LnInvoiceTag {
    override val prefix: LnTagPrefix = LnTagPrefix.CltvExpiry

    override val bytes: ByteVector = u64.bytes
  }

  case class FallbackAddressTag(version: UInt8, address: Address) extends LnInvoiceTag {
    require(version < UInt8(32), s"Version on the fallback address must be less than 2^5 (32), got $version")

    override val prefix: LnTagPrefix = LnTagPrefix.FallbackAddress

    override val bytes: ByteVector = {
      //not sure if this serialization is correct or not, come back and look later
      version.bytes ++ address.scriptPubKey.asmBytes
    }
  }

}

sealed abstract class LnInvoiceTags {
  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  def paymentHash: LnInvoiceTag.PaymentHashTag

  def description: Option[LnInvoiceTag.DescriptionTag]

  def spk: Option[LnInvoiceTag.NodeIdTag]

  def descriptionHash: Option[LnInvoiceTag.DescriptionHashTag]

  def expiryTime: Option[LnInvoiceTag.ExpiryTimeTag]

  def cltvExpiry: Option[LnInvoiceTag.CltvExpiryTag]

  def fallbackAddress: Option[LnInvoiceTag.FallbackAddressTag]

  def routingInfo: Option[Vector[LnRoutingInfo]] //TODO: Not implemented

  override def toString: String = {
    val empty = ""

    val b = new StringBuilder()

    b.append(paymentHash.toString)
    b.append(description.getOrElse(empty))
    b.append(spk.getOrElse(empty))
    b.append(descriptionHash.getOrElse(empty))
    b.append(expiryTime.getOrElse(empty))
    b.append(expiryTime.getOrElse(empty))
    b.append(cltvExpiry.getOrElse(empty))
    b.append(fallbackAddress.getOrElse(empty))

    //TODO: add routing info

    b.toString()
  }

  /**
   * The formula for this calculation is as follows:
   * Take the length of the Bech32 encoded input and divide it by 32.
   * Take the quotient, and encode this value as Bech32. Take the remainder and encode this value as Bech32.
   * Append these values to produce a valid Lighting Network data_length field.
   * Please see Bolt-11 for examples:
   * https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#examples
   */
  private def bech32EncodeDataLength(Bech32EncodedString: String): String = {
    val quotient = Bech32EncodedString.length / 32
    val remainder = Bech32EncodedString.length % 32
    val v = Vector(UInt8(quotient.toShort), UInt8(remainder.toShort))
    Bech32.encode8bitToString(v)
  }

}

object LnInvoiceTags extends {
  private case class InvoiceTagImpl(
    paymentHash: LnInvoiceTag.PaymentHashTag,
    description: Option[LnInvoiceTag.DescriptionTag],
    spk: Option[LnInvoiceTag.NodeIdTag],
    descriptionHash: Option[LnInvoiceTag.DescriptionHashTag],
    expiryTime: Option[LnInvoiceTag.ExpiryTimeTag],
    cltvExpiry: Option[LnInvoiceTag.CltvExpiryTag],
    fallbackAddress: Option[LnInvoiceTag.FallbackAddressTag],
    routingInfo: Option[Vector[LnRoutingInfo]]) extends LnInvoiceTags {
    require(
      (description.nonEmpty && description.get.string.length < 640) ||
        descriptionHash.nonEmpty,
      "You must supply either a description hash, or a literal description that is 640 characters or less to create an invoice.")
  }

  /**
   * According to BOLT11 these are the required fields in a LnInvoice
   * You need to provide a payment hash and either a description,
   * or the hash of the description
   */
  def apply(
    paymentHashTag: PaymentHashTag,
    descriptionOrHash: Either[LnInvoiceTag.DescriptionTag, LnInvoiceTag.DescriptionHashTag]): LnInvoiceTags = {

    LnInvoiceTags.apply(paymentHashTag, descriptionOrHash)
  }

  def apply(
    paymentHash: LnInvoiceTag.PaymentHashTag,
    descriptionOrHash: Either[LnInvoiceTag.DescriptionTag, LnInvoiceTag.DescriptionHashTag],
    spk: Option[LnInvoiceTag.NodeIdTag] = None,
    expiryTime: Option[LnInvoiceTag.ExpiryTimeTag] = None,
    cltvExpiry: Option[LnInvoiceTag.CltvExpiryTag] = None,
    fallbackAddress: Option[LnInvoiceTag.FallbackAddressTag] = None,
    routingInfo: Option[Vector[LnRoutingInfo]] = None): LnInvoiceTags = {

    if (descriptionOrHash.isLeft) {
      InvoiceTagImpl(
        paymentHash = paymentHash,
        description = descriptionOrHash.left.toOption,
        spk = spk,
        descriptionHash = None,
        expiryTime = expiryTime,
        cltvExpiry = cltvExpiry,
        fallbackAddress = fallbackAddress,
        routingInfo = routingInfo)
    } else {

      InvoiceTagImpl(
        paymentHash = paymentHash,
        description = None,
        spk = spk,
        descriptionHash = descriptionOrHash.right.toOption,
        expiryTime = expiryTime,
        cltvExpiry = cltvExpiry,
        fallbackAddress = fallbackAddress,
        routingInfo = routingInfo)
    }

  }

}

case class LnRoutingInfo(pubkey: ECPublicKey, shortChannelID: String, feeBaseMsat: LnCurrencyUnit, feePropMilli: Int, cltvExpiryDelta: Int) {
  require(pubkey.isCompressed, s"Can only use a compressed public key in routing")
  //TODO: Placeholder
}