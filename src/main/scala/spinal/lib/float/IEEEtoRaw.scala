package spinal.lib.float

import spinal.core._
import spire.math.pow

object IEEEToRaw {

  def apply[B](ieee : IEEEFloat[B]) : RawFloat = {
    
    val normalizedDistance = countLeadingZeros(ieee.mantissa) 
    val normalizedMantissa = ((ieee.mantissa<<normalizedDistance)
                              (ieee.mantissaLength-2 to 0) << 1)

    val adjustedExponent = (Mux(ieee.isZero,
                                 normalizedDistance ^ U(pow(2, BigInt(ieee.exponentWidth+1))-1),
                                 U(ieee.exponent)) +
                             (U(pow(2,ieee.exponentWidth-1)) | Mux(ieee.isExponentZero,
                                                                   U(2),
                                                                   U(1))))

    val raw = new RawFloat(ieee.mantissaWidth,
                           ieee.exponentWidth)

    raw.sign := ieee.sign
    raw.infinite := ieee.isInfinite
    raw.zero := ieee.isZero
    raw.normalized := ieee.isNormalized
    raw.nan := ieee.isNan
    raw.snan := ieee.isSNan
    raw.exponent := (B(0, 1 bits) ## adjustedExponent)
    raw.mantissa := (B(0, 1 bits) ## !ieee.isZero ## Mux(ieee.isExponentZero, 
                                                          normalizedMantissa,
                                                          ieee.mantissa))
    raw
  }
}
