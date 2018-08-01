package org.bitcoins.core.protocol.ln

import org.bitcoins.core.gen.LnCurrencyUnitGenerator
import org.scalacheck.{ Prop, Properties }

import scala.util.Try

class LnCurrencyUnitSpec extends Properties("LnCurrencyUnitSpec") {

  property("Additive identity for LnCurrencyUnits") =
    Prop.forAll(LnCurrencyUnitGenerator.lnCurrencyUnit) { lnUnit =>
      lnUnit + LnCurrencyUnits.getZeroUnit(lnUnit) == lnUnit
    }

  property("Add two LnCurrencyUnits") =
    Prop.forAll(LnCurrencyUnitGenerator.lnCurrencyUnit, LnCurrencyUnitGenerator.lnCurrencyUnit) { (num1: LnCurrencyUnit, num2: LnCurrencyUnit) =>
      val result: Try[LnCurrencyUnit] = Try(num1 + num2)
      if (result.isSuccess && result.get >= PicoBitcoins.min &&
        result.get <= PicoBitcoins.max) num1 + num2 == result.get
      else Try(num1 + num2).isFailure
    }

  property("Subtractive identity for LnCurrencyUnits") =
    Prop.forAll(LnCurrencyUnitGenerator.lnCurrencyUnit) { lnUnit =>
      lnUnit - LnCurrencyUnits.getZeroUnit(lnUnit) == lnUnit
    }

  property("Subtract two LnCurrencyUnit values") =
    Prop.forAll(LnCurrencyUnitGenerator.lnCurrencyUnit, LnCurrencyUnitGenerator.lnCurrencyUnit) { (num1: LnCurrencyUnit, num2: LnCurrencyUnit) =>
      val result: Try[LnCurrencyUnit] = Try(num1 - num2)
      if (result.isSuccess && result.get >= PicoBitcoins.min &&
        result.get <= PicoBitcoins.max) num1 - num2 == result.get
      else Try(num1 - num2).isFailure
    }

  property("Multiply LnCurrencyUnit by zero") =
    Prop.forAll(LnCurrencyUnitGenerator.lnCurrencyUnit) { lnUnit =>
      lnUnit * LnCurrencyUnits.getZeroUnit(lnUnit) == PicoBitcoins.zero
    }

  property("Multiplicative identity for LnCurrencyUnits") =
    Prop.forAll(LnCurrencyUnitGenerator.lnCurrencyUnit) { lnUnit =>
      lnUnit * LnCurrencyUnits.onePicoBTC == lnUnit
    }

  property("Multiply two LnCurrencyUnit values") =
    Prop.forAll(LnCurrencyUnitGenerator.lnCurrencyUnit, LnCurrencyUnitGenerator.lnCurrencyUnit) { (num1: LnCurrencyUnit, num2: LnCurrencyUnit) =>
      val result: Try[LnCurrencyUnit] = Try(num1 * num2)
      if (result.isSuccess && result.get >= PicoBitcoins.min &&
        result.get <= PicoBitcoins.max) num1 * num2 == result.get
      else Try(num1 * num2).isFailure
    }

  property("< & >=") =
    Prop.forAll(LnCurrencyUnitGenerator.lnCurrencyUnit, LnCurrencyUnitGenerator.lnCurrencyUnit) { (num1: LnCurrencyUnit, num2: LnCurrencyUnit) =>
      (num1 < num2) || (num1 >= num2)
    }

  property("<= & >") =
    Prop.forAll(LnCurrencyUnitGenerator.lnCurrencyUnit, LnCurrencyUnitGenerator.lnCurrencyUnit) { (num1: LnCurrencyUnit, num2: LnCurrencyUnit) =>
      (num1 <= num2) || (num1 > num2)
    }

  property("== & !=") =
    Prop.forAll(LnCurrencyUnitGenerator.lnCurrencyUnit, LnCurrencyUnitGenerator.lnCurrencyUnit) { (num1: LnCurrencyUnit, num2: LnCurrencyUnit) =>
      (num1 == num2) || (num1 != num2)
    }
}

