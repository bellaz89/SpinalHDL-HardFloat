package spinal.lib.float

import spinal.core._

object RawCompare {

  def apply(a : RawFloat, 
            b : RawFloat, 
            signalling : Bool) : (Bool, Bool, Bool, OpResultException.C) = {

    a.assertExponentMantissaLengthEqual(b)
    val ordered = !a.isNan & !b.isNan
    val bothInfinite =  a.isInfinite & b.isInfinite
    val bothZero = a.isZero & b.isZero
    val equalExponents = a.exponent === b.exponent
    val commonLTMagnitude = ((a.exponent.asUInt < b.exponent.asUInt) | 
                             (equalExponents & (a.mantissa.asUInt < b.mantissa.asUInt)))
    val commonEQMagnitude = equalExponents & a.mantissa === b.mantissa
    val orderedLT = !bothZero &
                    ((a.sign & !b.sign) |
                      (!bothInfinite &
                        ((a.sign & !commonLTMagnitude & !commonEQMagnitude) |
                          (!b.sign & commonLTMagnitude))))

    val orderedEQ = bothZero | ((a.sign === b.sign) & (bothInfinite | commonEQMagnitude)) 
    val isMantissaNanA = a.isNan & !a.mantissa(a.mantissaWidth-2)
    val isMantissaNanB = b.isNan & !b.mantissa(b.mantissaWidth-2)
    val invalid = isMantissaNanA | isMantissaNanB | (signalling & !ordered)

    val LT = ordered & orderedLT
    val EQ = ordered & orderedEQ
    val GT = ordered & !orderedLT & !orderedEQ
    val exception = Mux(invalid, 
                        OpResultException.Invalid(),
                        OpResultException.NoException())

    (LT, EQ, GT, exception)
  }
}
