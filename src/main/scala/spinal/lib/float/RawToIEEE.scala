package spinal.lib.float
import spinal.core._
import spire.math._

object RawToIEEE {
  def apply(raw : RawFloat) : IEEEFloat[Null] = {
    
    val denormalizedShiftDistance = (U(1) - 
                                    U(raw.exponent(log2Up(raw.mantissaWidth - 1) - 1)))
    val denormalizedMantissa = (((raw.mantissa>>1)>>denormalizedShiftDistance)
                                 (raw.mantissaWidth - 2 downto 0))

    val exponent = Mux(raw.isDenormalized, 
                      B(0, raw.exponentWidth bits),
                      (raw.exponent(raw.exponentWidth-1 downto 0).asUInt - 
                       (B(pow(2, BigInt(raw.exponentWidth - 1)) + 1, raw.exponentWidth bits) | 
                        Bits(raw.exponentWidth bits).setAllTo(raw.isNan | raw.isInfinite)).asUInt).asBits)

    val mantissa = Mux(raw.isDenormalized,
                       denormalizedMantissa,
                       Mux(raw.isInfinite, B(0), raw.mantissa(raw.mantissaWidth - 2 downto 0)))

    val ieee = IEEEFloat(raw.mantissaWidth, raw.exponentWidth)
    ieee.sign := raw.sign
    ieee.exponent := exponent
    ieee.mantissa := mantissa
    ieee
  }
}

