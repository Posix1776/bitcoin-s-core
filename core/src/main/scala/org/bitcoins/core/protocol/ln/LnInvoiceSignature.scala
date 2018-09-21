package org.bitcoins.core.protocol.ln

import org.bitcoins.core.crypto.ECDigitalSignature
import org.bitcoins.core.number.UInt8
import org.bitcoins.core.protocol.NetworkElement
import scodec.bits.ByteVector

sealed abstract class LnInvoiceSignature extends NetworkElement {
  def signature: ECDigitalSignature

  def version: UInt8

  override def bytes: ByteVector = {
    signature.toRawRS ++ version.bytes
  }
}

object LnInvoiceSignature {
  private case class LnInvoiceSignatureImpl(
    version: UInt8,
    signature: ECDigitalSignature) extends LnInvoiceSignature

  def apply(version: UInt8, signature: ECDigitalSignature): LnInvoiceSignature = {
    LnInvoiceSignatureImpl(version, signature)
  }
}
