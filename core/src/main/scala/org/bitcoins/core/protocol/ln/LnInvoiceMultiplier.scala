package org.bitcoins.core.protocol.ln

sealed abstract class LnInvoiceMultiplier {
  def char: Char
  def multi: Double
}

object LnInvoiceMultiplier {
  case object Milli extends LnInvoiceMultiplier {
    override def char = 'm'

    override def multi: Double = 0.001
  }

  case object Micro extends LnInvoiceMultiplier {
    override def char = 'u'

    override def multi: Double = 0.000001
  }

  case object Nano extends LnInvoiceMultiplier {
    override def char = 'n'

    override def multi: Double = 0.000000001
  }

  case object Pico extends LnInvoiceMultiplier {
    override def char = 'p'

    override def multi: Double = 0.000000000001
  }
}
