package org.bitcoins.core.protocol

import org.bitcoins.core.config.{ MainNet, NetworkParameters, RegTest, TestNet3 }

sealed abstract class LightningNetworkPrefix {
  def value: String
  def network: NetworkParameters
}

object LightningNetworkPrefix {

  case object BitcoinMainNet extends LightningNetworkPrefix {
    override def value = "lnbc"
    override def network = MainNet
  }

  case object BitcoinTestNet extends LightningNetworkPrefix {
    override def value = "lntb"
    override def network = TestNet3
  }

  case object BitcoinRegTest extends LightningNetworkPrefix {
    override def value = "lnbcrt"
    override def network = RegTest
  }

  val allNetworks = Vector(BitcoinMainNet, BitcoinTestNet, BitcoinRegTest)

  def fromString(prefix: String): Option[LightningNetworkPrefix] = allNetworks.find(_.value == prefix)

  def fromNetworkParameters(np: NetworkParameters) = allNetworks.find(_.network == np)
}
