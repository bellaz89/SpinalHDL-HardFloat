package spinal.lib.float
import spinal.core._

object RawToRec {
  
  def apply(raw : RawFloat) : RecFloat[Null] = {
  
    val rec = RecFloat(raw.mantissaWidth,
                       raw.exponentWidth)

    val upperExponent = (Mux(raw.isZero, 
                             B(0, 3 bits),
                             raw.exponent(raw.exponentWidth downto 
                                          raw.exponentWidth-2)) |
                         Mux(raw.isNan, B(0), B(1)))
    rec.sign := raw.sign
    rec.exponent := (upperExponent ## 
                     raw.exponent(raw.exponentWidth-3 downto 0))

    rec.mantissa := raw.mantissa(raw.mantissaWidth-2 downto 0)
    rec
  }
} 
