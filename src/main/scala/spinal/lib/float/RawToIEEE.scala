//package spinal.lib.float
//import spinal.core._
//import spire.math._
//
//object RawToIEEE {
//  def apply(raw : RawFloat) : IEEEFloat[Null] = {
//    
//    val denormalizedShiftDistance =  - raw.exponent()
//    val denormalizedMantissa =
//  
//
//    val exponent = raw.isDenormalized
//    val mantissa =
//
//    val ieee = IEEEFloat(raw.mantissaWidth, raw.exponentWidth)
//    ieee.sign := raw.sign
//    ieee.exponent := exponent
//    ieee.mantissa := mantissa
//    ieee
//  }
//}
//
