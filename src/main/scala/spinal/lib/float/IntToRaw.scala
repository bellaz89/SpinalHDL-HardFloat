package spinal.lib.float

import spinal.core._
import spire.math.{pow, round}

object IntToRaw {

  def apply(integer : SInt) : RawFloat = apply(integer.asBits, True)
  def apply(integer : UInt) : RawFloat = apply(integer.asBits, False)

  def apply(rawInteger : Bits, signed : Bool) : RawFloat = {

    val mantissaWidth = rawInteger.getWidth+1
    val exponentWidth = log2Up(rawInteger.getWidth) + 1
    val extIntWidth = pow(2, exponentWidth - 1).toInt

    val raw = new RawFloat(mantissaWidth, exponentWidth)

    raw.sign := signed & rawInteger(rawInteger.getWidth-1)
    val absoluteInt = Mux(!raw.sign, rawInteger.asUInt, (~rawInteger).asUInt+1)
    val extAbsoluteInt = (B(0, extIntWidth bits) ## 
                          absoluteInt)(extIntWidth-1 downto 0)
    val adjustedNormDistance = countLeadingZeros(extAbsoluteInt)

    raw.infinite := False
    raw.zero := (rawInteger =/= B(0))
    raw.normalized := !raw.zero
    raw.nan := False
    raw.snan := False
    raw.exponent := (B(2, 3 bits) ## 
                     ~adjustedNormDistance(exponentWidth - 2 downto 0))

    raw.mantissa := (extAbsoluteInt << adjustedNormDistance)(extIntWidth-1 downto 
                                                             extIntWidth-rawInteger.getWidth)
    raw
  }
}
