package spinal.lib.float

import spinal.core._

object RecToRaw {

  def apply[B](rec : RecFloat[B]) : RawFloat = {
   

    val raw = new RawFloat(rec.mantissaWidth,
                           rec.exponentWidth)

    raw.sign := rec.sign
    raw.infinite := rec.isInfinite
    raw.zero := rec.isZero
    raw.normalized := rec.isNormalized
    raw.nan := rec.isNan
    raw.snan := rec.isSNan
    raw.exponent := (B(0, 1 bits) ## rec.exponent)
    raw.mantissa := (B(0, 1 bits) ## 
                    !rec.isZero ## 
                    rec.mantissa(rec.mantissaWidth-2 downto 0))
    raw
  }
}
