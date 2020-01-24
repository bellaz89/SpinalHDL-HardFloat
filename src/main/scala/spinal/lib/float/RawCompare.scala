//package spinal.lib.float
//
//import spina.core._
//
//object RawCompare {
//
//  def apply(a : RawFloat, 
//            b : RawFloat, 
//            signalling : Bool) : (Bool, Bool, Bool, OpResultExCeption.C) {
//
//    a.assertExponentMantissaLengthEqual(b)
//    val ordered = !a.isNan & !b.isNan
//    val bothInfinite =  a.isInf & b.isInf
//    val bothZero = a.isZero & b.isZero
//    val equalExponents = a.exponent === b.exponent
//    val commonLTMagnitude = ((a.exponent.asUInt < b.exponent.asUInt) | 
//                             (equalExponents & (a.mantissa.asUInt < b.asUInt)))
//    val commonEQMagnitude = equalExponents & a.mantissa === b.mantissa
//    val orderedLT = !bothZero &
//                    ((a.sign & !b.sign) |
//                      (!bothInfinite &
//                        ((a.sign & !commonLTMagnitude & !commonEQMagnitude)
//                          (!b.sign & commonLTMagnitude))))
//
//    val orederedEq = bothZero | ((a.sign === b.sign) & (bothInfinite | commonEQMagnitude))
//    
//    
//
//  }
//}
