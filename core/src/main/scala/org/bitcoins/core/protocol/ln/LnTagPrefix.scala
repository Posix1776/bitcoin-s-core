package org.bitcoins.core.protocol.ln

sealed abstract class LnTagPrefix {
  def value: Char

  override def toString: String = value.toString
}

/**
 * This defines the necessary Lightning Network Tag Prefix's, as specified in BOLT-11
 * Please see: https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#tagged-fields
 */
object LnTagPrefix {

  case object PaymentHash extends LnTagPrefix {
    override def value: Char = 'p'
  }
  case object Description extends LnTagPrefix {
    override def value: Char = 'd'
  }

  /** The nodeId of the node paying the invoice */
  case object NodeId extends LnTagPrefix {
    override def value: Char = 'n'
  }

  case object DescriptionHash extends LnTagPrefix {
    override def value: Char = 'h'
  }

  case object ExpiryTime extends LnTagPrefix {
    override def value: Char = 'x'
  }

  case object CltvExpiry extends LnTagPrefix {
    override def value: Char = 'c'
  }

  case object FallbackAddress extends LnTagPrefix {
    override def value: Char = 'f'
  }

  case object RoutingInfo extends LnTagPrefix {
    override def value: Char = 'r'
  }

  case object None extends LnTagPrefix {
    override def value: Char = "".toCharArray.head
  }

  private val all = List(
    PaymentHash, Description, NodeId,
    DescriptionHash, ExpiryTime, CltvExpiry,
    FallbackAddress, RoutingInfo, None)

  def fromString(str: String): Option[LnTagPrefix] = {
    all.find(_.value == str)
  }
}