package org.bitcoins.core.protocol.ln

import org.bitcoins.core.crypto.ECDigitalSignature
import org.bitcoins.core.number.{ UInt5, UInt8 }
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.Bech32
import scodec.bits.ByteVector

sealed abstract class LnInvoiceSignature {
  def signature: ECDigitalSignature

  def version: UInt8

  def data: Vector[UInt5] = {
    val bytes = signature.toRawRS ++ version.bytes
    Bech32.from8bitTo5bit(bytes)
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
