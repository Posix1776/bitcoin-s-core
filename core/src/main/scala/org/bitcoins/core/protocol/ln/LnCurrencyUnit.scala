package org.bitcoins.core.protocol.ln

import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.{BaseNumbers, Int64}
import org.bitcoins.core.protocol.NetworkElement

sealed abstract class LnCurrencyUnit extends NetworkElement {
  type A

  def character: Char

  def multiplier: BigDecimal

  def >=(c: LnCurrencyUnit): Boolean = {
    toSatoshis >= c.toSatoshis
  }

  def >(c: LnCurrencyUnit): Boolean = {
    toSatoshis > c.toSatoshis
  }

  def <(c: LnCurrencyUnit): Boolean = {
    toSatoshis < c.toSatoshis
  }

  def <=(c: LnCurrencyUnit): Boolean = {
    toSatoshis <= c.toSatoshis
  }

  def !=(c: LnCurrencyUnit): Boolean = !(this == c)

  def ==(c: LnCurrencyUnit): Boolean = toSatoshis == c.toSatoshis

  def +(c: LnCurrencyUnit): LnCurrencyUnit = {
    if(this.getClass.getName == c.getClass().getName) {
      currencyUnitFromInt(c, toBigInt + c.toBigInt)
    } else { throw new IllegalArgumentException(s"Cannot add different currency types. Precision will be lost.") }
  }

  def -(c: LnCurrencyUnit): LnCurrencyUnit = {
    if(this.getClass.getName == c.getClass().getName) {
      currencyUnitFromInt(c, toBigInt - c.toBigInt)
    } else { throw new IllegalArgumentException(s"Cannot subtract different currency types. Precision will be lost.") }
  }

  def *(c: LnCurrencyUnit): LnCurrencyUnit = {
    if(this.getClass.getName == c.getClass().getName) {
      currencyUnitFromInt(c, toBigInt * c.toBigInt)
    } else { throw new IllegalArgumentException(s"Cannot multiply different currency types. Precision will be lost.") }
  }

  def unary_- : Unit = { //Satoshis(-satoshis.underlying) //if(this == c) {
    -toBigInt// + c.toSatoshis }
  }

  override def bytes: Seq[Byte] = toSatoshis.bytes

  def toBigInt: BigInt

  protected def underlying: A

  def toSatoshis: Satoshis

  def currencyUnitFromInt(lnType: LnCurrencyUnit, unit: BigInt): LnCurrencyUnit = lnType match {
    case b: MilliBitcoins => MilliBitcoins(unit)
    case c: MicroBitcoins => MicroBitcoins(unit)
    case d: NanoBitcoins => NanoBitcoins(unit)
    case e: PicoBitcoins => PicoBitcoins(unit)
  }
}

sealed abstract class MilliBitcoins extends LnCurrencyUnit {
  override type A = BigInt

  override def character: Char = 'm'

  override def multiplier: BigDecimal = 0.001

  override def toBigInt: A = underlying

  override def toSatoshis: Satoshis = {
    val sat = BigDecimal(underlying) * (Bitcoins.one.satoshis.toBigDecimal * multiplier)
    Satoshis(Int64(sat.toLongExact))
  }
}

object MilliBitcoins extends BaseNumbers[MilliBitcoins] {
  val min = MilliBitcoins(-(Consensus.maxMoney.toBigDecimal / (Bitcoins.one.satoshis.toBigDecimal * LnCurrencyUnits.MilliMultiplier)).toBigInt())
  val max = MilliBitcoins((Consensus.maxMoney.toBigDecimal / (Bitcoins.one.satoshis.toBigDecimal * LnCurrencyUnits.MilliMultiplier)).toBigInt())
  val zero = MilliBitcoins(0)
  val one = MilliBitcoins(1)

  def apply(milliBitcoins: Int64): MilliBitcoins = MilliBitcoins(milliBitcoins)

  def apply(underlying: BigInt): MilliBitcoins = BitcoinsImpl(underlying)

  private case class BitcoinsImpl(underlying: BigInt) extends MilliBitcoins
}

sealed abstract class MicroBitcoins extends LnCurrencyUnit {
  override type A = BigInt

  override def character: Char = 'u'

  override def multiplier: BigDecimal = 0.000001

  override def toBigInt: A = underlying

  override def toSatoshis: Satoshis = {
    val sat = BigDecimal(underlying) * Bitcoins.one.satoshis.toBigDecimal * multiplier
    Satoshis(Int64(sat.toLongExact))
  }
}

object MicroBitcoins extends BaseNumbers[MicroBitcoins] {
  val min = MicroBitcoins(-(Consensus.maxMoney.toBigDecimal / (Bitcoins.one.satoshis.toBigDecimal * 0.000001)).toBigInt()) //TODO: Replace with LnCurrencyUnits.MicroMultiplier
  val max = MicroBitcoins((Consensus.maxMoney.toBigDecimal / (Bitcoins.one.satoshis.toBigDecimal * 0.000001)).toBigInt())  //TODO: Replace with LnCurrencyUnits.MicroMultiplier
  val zero = MicroBitcoins(0)
  val one = MicroBitcoins(1)

  def apply(microBitcoins: Int64): MicroBitcoins = MicroBitcoins(microBitcoins)

  def apply(underlying: BigInt): MicroBitcoins = BitcoinsImpl(underlying)

  private case class BitcoinsImpl(underlying: BigInt) extends MicroBitcoins
}

sealed abstract class NanoBitcoins extends LnCurrencyUnit {
  override type A = BigInt

  override def character: Char = 'n'

  override def multiplier: BigDecimal = 0.000000001

  override def toBigInt: A = underlying

  override def toSatoshis: Satoshis = {
    val sat = BigDecimal(underlying) * Bitcoins.one.satoshis.toBigDecimal * multiplier
    val rounded = math.floor(sat.toDouble).toInt //Value must be larger than 1 Satoshi (10 nano) rounded down, otherwise we return 0 Satoshis
    if(rounded >= 1) {
      Satoshis(Int64(rounded))
    } else Satoshis.zero
  }
}

object NanoBitcoins extends BaseNumbers[NanoBitcoins] {
  val min = NanoBitcoins(-(Consensus.maxMoney.toBigDecimal / (Bitcoins.one.satoshis.toBigDecimal * 0.000000001)).toBigInt()) //TODO: Replace with LnCurrencyUnits.MicroMultiplier
  val max = NanoBitcoins((Consensus.maxMoney.toBigDecimal / (Bitcoins.one.satoshis.toBigDecimal * 0.000000001)).toBigInt())  //TODO: Replace with LnCurrencyUnits.MicroMultiplier
  val zero = NanoBitcoins(0)
  val one = NanoBitcoins(1)

  def apply(nanoBitcoins: Int64): NanoBitcoins = NanoBitcoins(nanoBitcoins)

  def apply(underlying: BigInt): NanoBitcoins = BitcoinsImpl(underlying)

  private case class BitcoinsImpl(underlying: BigInt) extends NanoBitcoins
}

sealed abstract class PicoBitcoins extends LnCurrencyUnit {
  override type A = BigInt

  override def character: Char = 'p'

  override def multiplier: BigDecimal = 0.000000000001

  override def toBigInt: A = underlying

  override def toSatoshis: Satoshis = {
    val sat = BigDecimal(underlying) * Bitcoins.one.satoshis.toBigDecimal * multiplier
    val rounded = math.floor(sat.toDouble).toInt //Value must be larger than 1 Satoshi (10000 pico) rounded down, otherwise we return 0 Satoshis
    if(rounded >= 1) {
      Satoshis(Int64(rounded))
    } else Satoshis.zero
  }
}

object PicoBitcoins extends BaseNumbers[PicoBitcoins] {
  val min = PicoBitcoins(-(Consensus.maxMoney.toBigDecimal / (Bitcoins.one.satoshis.toBigDecimal * 0.000000000001)).toBigInt()) //TODO: Replace with LnCurrencyUnits.MicroMultiplier
  val max = PicoBitcoins((Consensus.maxMoney.toBigDecimal / (Bitcoins.one.satoshis.toBigDecimal * 0.000000000001)).toBigInt())  //TODO: Replace with LnCurrencyUnits.MicroMultiplier
  val zero = PicoBitcoins(0)
  val one = PicoBitcoins(1)

  def apply(picoBitcoins: Int64): PicoBitcoins = PicoBitcoins(picoBitcoins)

  def apply(underlying: BigInt): PicoBitcoins = BitcoinsImpl(underlying)

  private case class BitcoinsImpl(underlying: BigInt) extends PicoBitcoins
}

object LnCurrencyUnits {
  val oneMilliBTC: LnCurrencyUnit = MilliBitcoins.one
  val oneMicroBTC: LnCurrencyUnit = MicroBitcoins.one
  val oneNanoBTC: LnCurrencyUnit = NanoBitcoins.one
  val onePicoBTC: LnCurrencyUnit = PicoBitcoins.one
  val MilliMultiplier: BigDecimal = BigDecimal(0.001)
  val MicroMultiplier: BigDecimal = BigDecimal(0.000001)
  val NanoMultiplier: BigDecimal = BigDecimal(0.000000001)
  val PicoMultiplier: BigDecimal = BigDecimal(0.000000000001)
}


