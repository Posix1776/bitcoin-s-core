package org.bitcoins.core.protocol.ln

sealed abstract class LnTagPrefix {
  def value: String

  override def toString: String = value
}

/**
 * This defines the necessary Lightning Network Tag Prefix's, as specified in BOLT-11
 * Please see: https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#tagged-fields
 */
object LnTagPrefix {

  case object PaymentHash extends LnTagPrefix {
    override def value: String = "p"
  }
  case object Description extends LnTagPrefix {
    override def value: String = "d"
  }

  /** The nodeId of the node paying the invoice */
  case object NodeId extends LnTagPrefix {
    override def value: String = "n"
  }

  case object DescriptionHash extends LnTagPrefix {
    override def value: String = "h"
  }

  case object ExpiryTime extends LnTagPrefix {
    override def value: String = "x"
  }

  case object CltvExpiry extends LnTagPrefix {
    override def value: String = "c"
  }

  case object FallbackAddress extends LnTagPrefix {
    override def value: String = "f"
  }

  case object RoutingInfo extends LnTagPrefix {
    override def value: String = "r"
  }

  case object None extends LnTagPrefix {
    override def value: String = ""
  }

  private val all = List(
    PaymentHash, Description, NodeId,
    DescriptionHash, ExpiryTime, CltvExpiry,
    FallbackAddress, RoutingInfo, None)

  def fromString(str: String): Option[LnTagPrefix] = {
    all.find(_.value == str)
  }
}