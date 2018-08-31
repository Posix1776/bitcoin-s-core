package org.bitcoins.core.protocol.ln

import org.bitcoins.core.config.{ MainNet, NetworkParameters, RegTest, TestNet3 }
import org.bitcoins.core.currency.{ CurrencyUnit, CurrencyUnits }
//import org.bitcoins.core.protocol.ln.LightningNetworkPrefix.{BitcoinMainNet, BitcoinRegTest, BitcoinTestNet}
import org.bitcoins.core.protocol.ln.LightningNetworkPrefix._

sealed abstract class LnInvoiceHRP {
  def network: LightningNetworkParams

  def amount: Option[LnCurrencyUnit]

  override def toString: String = network.value + invoiceAmount

  def invoiceAmount: String = {
    if (amount.isDefined) {
      amount.get.toBigInt.toString() + amount.get.character
    } else ""
  }

  def bytes: Seq[Byte] = network.value.map(c => c.toByte) ++ invoiceAmount.map(c => c.toByte)
}

case class HRP(inNetwork: LightningNetworkParams, amnt: LnCurrencyUnit) extends LnInvoiceHRP {
  def network: LightningNetworkParams = inNetwork
  def amount: Option[LnCurrencyUnit] = Some(amnt)
}

case object lnbc extends LnInvoiceHRP {
  def network: LightningNetworkParams = BitcoinMainNet
  def amount: Option[LnCurrencyUnit] = None
}

case object lntb extends LnInvoiceHRP {
  def network: LightningNetworkParams = BitcoinTestNet
  def amount: Option[LnCurrencyUnit] = None
}

case object lnbcrt extends LnInvoiceHRP {
  def network: LightningNetworkParams = BitcoinRegTest
  def amount: Option[LnCurrencyUnit] = None
}

object LnInvoiceHRP {

  def apply(str: String) = str match {
    case "lnbc" => lnbc
    case "lntb" => lntb
    case "lnbcrt" => lnbcrt
  }

  def apply(network: NetworkParameters): LnInvoiceHRP = network match { //TODO: FIX
    case _: MainNet => lnbc
    case _: TestNet3 => lntb
    case _: RegTest => lnbcrt
  }

  def apply(network: LightningNetworkParams, amnt: LnCurrencyUnit): LnInvoiceHRP = {
    HRP(network, amnt)
  }
}