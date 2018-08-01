package org.bitcoins.core.protocol.ln

import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.{BaseNumbers, Int64}
import org.bitcoins.core.protocol.NetworkElement

sealed abstract class LnCurrencyUnit extends NetworkElement {
  type A

  def character: Char

  def multiplier: BigDecimal

  def >=(ln: LnCurrencyUnit): Boolean = {
    toPicoBitcoins >= ln.toPicoBitcoins
  }

  def >(ln: LnCurrencyUnit): Boolean = {
    toPicoBitcoins > ln.toPicoBitcoins
  }

  def <(ln: LnCurrencyUnit): Boolean = {
    toPicoBitcoins < ln.toPicoBitcoins
  }

  def <=(ln: LnCurrencyUnit): Boolean = {
    toPicoBitcoins <= ln.toPicoBitcoins
  }

  def !=(ln: LnCurrencyUnit): Boolean = !(this == ln)

  def ==(ln: LnCurrencyUnit): Boolean = toPicoBitcoins == ln.toPicoBitcoins

  def +(ln: LnCurrencyUnit): LnCurrencyUnit = {
    val returnType = if(ln.multiplier < multiplier) ln else this //return the type with the most precision.
    currencyUnitFromPicoValue(returnType, toPicoBitcoins + ln.toPicoBitcoins)
  }

  def -(ln: LnCurrencyUnit): LnCurrencyUnit = {
    val returnType = if(ln.multiplier < multiplier) ln else this //return the type with the most precision.
    currencyUnitFromPicoValue(returnType, toPicoBitcoins - ln.toPicoBitcoins)
  }

  def *(ln: LnCurrencyUnit): LnCurrencyUnit = {
    PicoBitcoins(toPicoBitcoins * ln.toPicoBitcoins)
  }

  def unary_- : LnCurrencyUnit = {
    currencyUnitFromPicoValue(this, -toPicoBitcoins)
  }

  override def bytes: Seq[Byte] = toSatoshis.bytes

  def toBigInt: BigInt

  protected def underlying: A

  def toSatoshis: Satoshis

  def toPicoBitcoins: BigInt

  def currencyUnitFromPicoValue(lnType: LnCurrencyUnit, unit: BigInt): LnCurrencyUnit = {
    val amount = unit / (lnType.multiplier / LnCurrencyUnits.PicoMultiplier).toBigInt()
    lnType match {
      case b: MilliBitcoins => MilliBitcoins(amount)
      case c: MicroBitcoins => MicroBitcoins(amount)
      case d: NanoBitcoins => NanoBitcoins(amount)
      case e: PicoBitcoins => PicoBitcoins(amount)
    }
  }
}

sealed abstract class MilliBitcoins extends LnCurrencyUnit {
  override type A = BigInt

  override def character: Char = 'm'

  override def multiplier: BigDecimal = 0.001

  override def toBigInt: A = underlying

  override def toSatoshis: Satoshis = LnCurrencyUnits.toSatoshi(this)

  override def toPicoBitcoins: BigInt = LnCurrencyUnits.toPicoBitcoins(this)
}

object MilliBitcoins extends BaseNumbers[MilliBitcoins] {
  val min = MilliBitcoins(-(Consensus.maxMoney.toBigDecimal / (Bitcoins.one.satoshis.toBigDecimal * LnCurrencyUnits.MilliMultiplier)).toBigInt())
  val max = MilliBitcoins((Consensus.maxMoney.toBigDecimal / (Bitcoins.one.satoshis.toBigDecimal * LnCurrencyUnits.MilliMultiplier)).toBigInt())
  val zero = MilliBitcoins(0)
  val one = MilliBitcoins(1)

  def apply(milliBitcoins: Int64): MilliBitcoins = MilliBitcoins(milliBitcoins)

  def apply(underlying: BigInt): MilliBitcoins = MilliBitcoinsImpl(underlying)

  private case class MilliBitcoinsImpl(underlying: BigInt) extends MilliBitcoins
}

sealed abstract class MicroBitcoins extends LnCurrencyUnit {
  override type A = BigInt

  override def character: Char = 'u'

  override def multiplier: BigDecimal = 0.000001

  override def toBigInt: A = underlying

  override def toSatoshis: Satoshis = LnCurrencyUnits.toSatoshi(this)

  override def toPicoBitcoins: BigInt = LnCurrencyUnits.toPicoBitcoins(this)
}

object MicroBitcoins extends BaseNumbers[MicroBitcoins] {
  val min = MicroBitcoins(-(Consensus.maxMoney.toBigDecimal / (Bitcoins.one.satoshis.toBigDecimal * 0.000001)).toBigInt()) //TODO: Replace with LnCurrencyUnits.MicroMultiplier
  val max = MicroBitcoins((Consensus.maxMoney.toBigDecimal / (Bitcoins.one.satoshis.toBigDecimal * 0.000001)).toBigInt())  //TODO: Replace with LnCurrencyUnits.MicroMultiplier
  val zero = MicroBitcoins(0)
  val one = MicroBitcoins(1)

  def apply(microBitcoins: Int64): MicroBitcoins = MicroBitcoins(microBitcoins)

  def apply(underlying: BigInt): MicroBitcoins = MicroBitcoinsImpl(underlying)

  private case class MicroBitcoinsImpl(underlying: BigInt) extends MicroBitcoins
}

sealed abstract class NanoBitcoins extends LnCurrencyUnit {
  override type A = BigInt

  override def character: Char = 'n'

  override def multiplier: BigDecimal = 0.000000001

  override def toBigInt: A = underlying

  override def toSatoshis: Satoshis = LnCurrencyUnits.toSatoshi(this)

  override def toPicoBitcoins: BigInt = LnCurrencyUnits.toPicoBitcoins(this)
}

object NanoBitcoins extends BaseNumbers[NanoBitcoins] {
  val min = NanoBitcoins(-(Consensus.maxMoney.toBigDecimal / (Bitcoins.one.satoshis.toBigDecimal * 0.000000001)).toBigInt()) //TODO: Replace with LnCurrencyUnits.NanoMultiplier
  val max = NanoBitcoins((Consensus.maxMoney.toBigDecimal / (Bitcoins.one.satoshis.toBigDecimal * 0.000000001)).toBigInt())  //TODO: Replace with LnCurrencyUnits.NanoMultiplier
  val zero = NanoBitcoins(0)
  val one = NanoBitcoins(1)

  def apply(nanoBitcoins: Int64): NanoBitcoins = NanoBitcoins(nanoBitcoins)

  def apply(underlying: BigInt): NanoBitcoins = NanoBitcoinsImpl(underlying)

  private case class NanoBitcoinsImpl(underlying: BigInt) extends NanoBitcoins
}

sealed abstract class PicoBitcoins extends LnCurrencyUnit {
  override type A = BigInt

  override def character: Char = 'p'

  override def multiplier: BigDecimal = 0.000000000001

  override def toBigInt: A = underlying

  override def toSatoshis: Satoshis = LnCurrencyUnits.toSatoshi(this)

  override def toPicoBitcoins: BigInt = this.toBigInt
}

object PicoBitcoins extends BaseNumbers[PicoBitcoins] {
  val min = PicoBitcoins(-(Consensus.maxMoney.toBigDecimal / (Bitcoins.one.satoshis.toBigDecimal * 0.000000000001)).toBigInt()) //TODO: Replace with LnCurrencyUnits.PicoMultiplier
  val max = PicoBitcoins((Consensus.maxMoney.toBigDecimal / (Bitcoins.one.satoshis.toBigDecimal * 0.000000000001)).toBigInt())  //TODO: Replace with LnCurrencyUnits.PicoMultiplier
  val zero = PicoBitcoins(0)
  val one = PicoBitcoins(1)

  def apply(picoBitcoins: Int64): PicoBitcoins = PicoBitcoins(picoBitcoins)

  def apply(underlying: BigInt): PicoBitcoins = PicoBitcoinsImpl(underlying)

  private case class PicoBitcoinsImpl(underlying: BigInt) extends PicoBitcoins
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

  def toPicoBitcoins(lnCurrencyUnits: LnCurrencyUnit): BigInt = lnCurrencyUnits.toBigInt * (lnCurrencyUnits.multiplier / LnCurrencyUnits.PicoMultiplier).toBigInt()

  def toSatoshi(lnCurrencyUnits: LnCurrencyUnit): Satoshis = {
    val sat = BigDecimal(lnCurrencyUnits.toBigInt) * Bitcoins.one.satoshis.toBigDecimal * lnCurrencyUnits.multiplier
    val rounded = math.floor(sat.toDouble).toInt
    if (rounded >= 1) {
      Satoshis(Int64(rounded))
    } else Satoshis.zero
  }
}