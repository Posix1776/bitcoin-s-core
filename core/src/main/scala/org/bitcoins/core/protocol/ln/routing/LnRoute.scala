package org.bitcoins.core.protocol.ln.routing

import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.ln.ShortChannelId
import org.bitcoins.core.protocol.ln.fee.{ FeeBaseMSat, FeeProportionalMillionths }
import org.bitcoins.core.util.BitcoinSUtil
import scodec.bits.ByteVector

/**
 * Indicates a node to route through with specific options on the Lightning Network
 * For more details on these settings please see
 * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/02-peer-protocol.md#cltv_expiry_delta-selection]]
 */
case class LnRoute(
  pubkey: ECPublicKey,
  shortChannelID: ShortChannelId,
  feeBaseMsat: FeeBaseMSat,
  feePropMilli: FeeProportionalMillionths,
  cltvExpiryDelta: Short) extends NetworkElement {

  require(pubkey.isCompressed, s"Can only use a compressed public key in routing")

  override def bytes: ByteVector = {

    val cltvExpiryDeltaHex = BitcoinSUtil.encodeHex(cltvExpiryDelta)

    pubkey.bytes ++
      shortChannelID.bytes ++
      feeBaseMsat.bytes ++
      feePropMilli.bytes ++
      ByteVector.fromValidHex(cltvExpiryDeltaHex)
  }
}