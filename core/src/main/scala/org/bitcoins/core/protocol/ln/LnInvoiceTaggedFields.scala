package org.bitcoins.core.protocol.ln

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.ln.LnInvoiceTag.PaymentHashTag
import org.slf4j.LoggerFactory
import scodec.bits.ByteVector

import scala.collection.mutable

/**
 * An aggregation of all the individual tagged fields in a [[org.bitcoins.core.protocol.ln.LnInvoice]]
 */
sealed abstract class LnInvoiceTaggedFields extends NetworkElement {

  def paymentHash: LnInvoiceTag.PaymentHashTag

  def description: Option[LnInvoiceTag.DescriptionTag]

  def nodeId: Option[LnInvoiceTag.NodeIdTag]

  def descriptionHash: Option[LnInvoiceTag.DescriptionHashTag]

  def expiryTime: Option[LnInvoiceTag.ExpiryTimeTag]

  def cltvExpiry: Option[LnInvoiceTag.MinFinalCltvExpiry]

  def fallbackAddress: Option[LnInvoiceTag.FallbackAddressTag]

  def routingInfo: Option[LnInvoiceTag.RoutingInfo]

  override def bytes: ByteVector = {
    paymentHash.bytes ++
      description.map(_.bytes).getOrElse(ByteVector.empty) ++
      nodeId.map(_.bytes).getOrElse(ByteVector.empty) ++
      descriptionHash.map(_.bytes).getOrElse(ByteVector.empty) ++
      expiryTime.map(_.bytes).getOrElse(ByteVector.empty) ++
      cltvExpiry.map(_.bytes).getOrElse(ByteVector.empty) ++
      fallbackAddress.map(_.bytes).getOrElse(ByteVector.empty) ++
      routingInfo.map(_.bytes).getOrElse(ByteVector.empty)
  }

  override def toString: String = {
    val empty = ""

    val b = new mutable.StringBuilder()

    b.append(paymentHash.toString)
    b.append(description.map(_.toString).getOrElse(empty))
    b.append(nodeId.map(_.toString).getOrElse(empty))
    b.append(descriptionHash.map(_.toString).getOrElse(empty))
    b.append(expiryTime.map(_.toString).getOrElse(empty))
    b.append(cltvExpiry.map(_.toString).getOrElse(empty))
    b.append(fallbackAddress.map(_.toString).getOrElse(empty))

    val routes = routingInfo.map(_.toString).getOrElse(empty)
    b.append(routes)

    b.toString()
  }
}

object LnInvoiceTaggedFields extends {
  private case class InvoiceTagImpl(
    paymentHash: LnInvoiceTag.PaymentHashTag,
    description: Option[LnInvoiceTag.DescriptionTag],
    nodeId: Option[LnInvoiceTag.NodeIdTag],
    descriptionHash: Option[LnInvoiceTag.DescriptionHashTag],
    expiryTime: Option[LnInvoiceTag.ExpiryTimeTag],
    cltvExpiry: Option[LnInvoiceTag.MinFinalCltvExpiry],
    fallbackAddress: Option[LnInvoiceTag.FallbackAddressTag],
    routingInfo: Option[LnInvoiceTag.RoutingInfo]) extends LnInvoiceTaggedFields {
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
    descriptionOrHash: Either[LnInvoiceTag.DescriptionTag, LnInvoiceTag.DescriptionHashTag]): LnInvoiceTaggedFields = {

    LnInvoiceTaggedFields.apply(paymentHashTag, descriptionOrHash)
  }

  def apply(
    paymentHash: LnInvoiceTag.PaymentHashTag,
    descriptionOrHash: Either[LnInvoiceTag.DescriptionTag, LnInvoiceTag.DescriptionHashTag],
    nodeId: Option[LnInvoiceTag.NodeIdTag] = None,
    expiryTime: Option[LnInvoiceTag.ExpiryTimeTag] = None,
    cltvExpiry: Option[LnInvoiceTag.MinFinalCltvExpiry] = None,
    fallbackAddress: Option[LnInvoiceTag.FallbackAddressTag] = None,
    routingInfo: Option[LnInvoiceTag.RoutingInfo] = None): LnInvoiceTaggedFields = {

    if (descriptionOrHash.isLeft) {
      InvoiceTagImpl(
        paymentHash = paymentHash,
        description = descriptionOrHash.left.toOption,
        nodeId = nodeId,
        descriptionHash = None,
        expiryTime = expiryTime,
        cltvExpiry = cltvExpiry,
        fallbackAddress = fallbackAddress,
        routingInfo = routingInfo)
    } else {

      InvoiceTagImpl(
        paymentHash = paymentHash,
        description = None,
        nodeId = nodeId,
        descriptionHash = descriptionOrHash.right.toOption,
        expiryTime = expiryTime,
        cltvExpiry = cltvExpiry,
        fallbackAddress = fallbackAddress,
        routingInfo = routingInfo)
    }

  }
}