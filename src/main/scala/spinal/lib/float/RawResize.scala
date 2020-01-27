package spinal.lib.float

import spinal.core._
import spire.math.pow

object RawResize {

  def apply(in : RawFloat, 
            newMantissaWidth : Int, 
            newExponentWidth : Int) : RawFloat = {
  
    val out = new RawFloat(newMantissaWidth, newExponentWidth)
    val adjustedExponent = in.exponent.asSInt +^ (S(pow(2, out.exponentWidth) - 
                                                    pow(2, in.exponentWidth)))

    val outExponent = if (in.exponentWidth <= out.exponentWidth) adjustedExponent 
                      else ((adjustedExponent < 0) ## 
                            Mux(adjustedExponent(in.exponentWidth+1 downto out.exponentWidth+1).orR,
                                Bits(out.exponentWidth-1 bits).setAllTo(True) ## B(0, 2 bits),
                                adjustedExponent(out.exponentWidth downto 0)))
 
    val outMantissa = if (in.mantissaWidth <= out.mantissaWidth) {
                      in.mantissa << (out.mantissaWidth - in.mantissaWidth)
                    } else {
                      (in.mantissa((in.mantissaWidth + 2) downto (in.mantissaWidth - out.mantissaWidth + 1)) ##
                      in.mantissa(in.mantissaWidth - out.mantissaWidth downto 0).orR)
                    }

    out.exponent := outExponent.asBits
    out.mantissa := outMantissa
    out.sign := in.sign
    out.isNan := in.isNan
    out.isInfinite := in.isInfinite
    out.isZero := in.isZero
    out.isSNan := in.isSNan
    out
  }
}
