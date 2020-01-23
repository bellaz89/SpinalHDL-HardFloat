package spinal.lib.float

import spinal.core._

object IEEEtoRaw {

  def apply(ieee : IEEEFloat) : RawFloat = {
    
    val normalizedDistance = countLeadingZeros(ieee.mantissa) 
    val normalizedMantissa = ((ieee.mantissa<<normalizedDistance)
                              (ieee.mantissaLength-2 to 0) << 1)

    val raw = new RawFloat(ieee.mantissaWidth,
                           ieee.exponentWidth)

    raw.sign := ieee.sign
    raw.infinite := ieee.isInfinite
    raw.zero := ieee.isZero
    raw.normalized := ieee.isNormalized
    raw.nan := ieee.isNan
    raw.snan := ieee.isSNan
    raw.mantissa(0) := False
    raw.mantissa(raw.mantissaLength-1 downto 1) := Mux(ieee.isExponentZero, 
                                                        normalizedMantissa,
                                                        ieee.mantissa)
    raw
  }
}
