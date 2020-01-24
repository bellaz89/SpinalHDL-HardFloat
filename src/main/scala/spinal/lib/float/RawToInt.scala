package spinal.lib.float

import spinal.core._
import spire.math.{pow, min}

object RawToInt {
  def apply(raw : RawFloat, 
            rm : RoundingMode.C, 
            signedOut : Bool, 
            intWidth: Int) : (Bits, OpResultException.C) =  {
  
    val roundingModeEven   = rm === RoundingMode.Even 
    val roundingModeOdd    = rm === RoundingMode.Odd
    val roundingModeMinMag = rm === RoundingMode.MinMag
    val roundingModeMaxMag = rm === RoundingMode.MaxMag
    val roundingModeMin    = rm === RoundingMode.Min
    val roundingModeMax    = rm === RoundingMode.Max
              
    
    val magnitudeGreaterThanOne = raw.exponent(raw.exponentWidth)
    val posExp = raw.exponent(raw.exponentWidth-1 downto 0)
    val magnitudeJustBelowOne = !magnitudeGreaterThanOne & posExp.andR

    // Explanation # if the value is greater than one, a '1' has to appended
    // to the mantissa
    val shiftedMantissa = ((magnitudeGreaterThanOne ## 
                            raw.mantissa(raw.mantissaWidth - 2 downto 0)) <<
                            Mux(magnitudeGreaterThanOne,
                                raw.exponent(min(raw.exponentWidth - 2,
                                                 log2Up(intWidth) - 1) downto 0),
                                B(0)).asUInt) 

    val alignedMantissa = ((shiftedMantissa >> raw.mantissaWidth) ##
                          shiftedMantissa(raw.mantissaWidth - 3 downto 0).orR)
    val unroundedInt = B(0, intWidth bits) | alignedMantissa
    val commonInexact = Mux(magnitudeGreaterThanOne,
                            alignedMantissa(1 downto 0).orR,
                            !raw.isZero)
    val roundIncrementEven = (magnitudeGreaterThanOne & 
                              (alignedMantissa(2 downto 1).orR | alignedMantissa(1 downto 0).orR)) |
                             (magnitudeJustBelowOne & alignedMantissa(1 downto 0).orR)
    val roundIncrementMaxMag = (magnitudeGreaterThanOne & alignedMantissa(1)) | magnitudeJustBelowOne
    val roundIncrement = ((roundingModeEven   & roundIncrementEven) |
                         (roundingModeMaxMag & roundIncrementMaxMag) |
                          ((roundingModeMin | roundingModeOdd) &
                           (raw.sign & commonInexact)) | 
                         (roundingModeMax & (!raw.sign & commonInexact)))
    val complementUnroundedInt = Mux(raw.sign, ~unroundedInt, unroundedInt)
    val roundedInt = (Mux(roundIncrement ^ raw.sign,
                          complementUnroundedInt.asUInt + U(1),
                          complementUnroundedInt.asUInt).asBits |
                      (roundingModeOdd & commonInexact).asBits.resized)

    val magnitudeGreaterThanOneAtOverflowEdge = (posExp === B(intWidth-1))
    val roundCarryBut2 = unroundedInt(intWidth - 3 downto 0).andR & roundIncrement
    val commonOverflow = Mux(magnitudeGreaterThanOne,
                             (posExp.asUInt >= U(intWidth)) |
                              Mux(signedOut,
                                Mux(raw.sign,
                                  magnitudeGreaterThanOneAtOverflowEdge &
                                    (unroundedInt(intWidth - 2 downto 0).orR | roundIncrement),
                                  magnitudeGreaterThanOneAtOverflowEdge |
                                    ((posExp === B(intWidth - 2)) & roundCarryBut2)),
                                raw.sign | (magnitudeGreaterThanOneAtOverflowEdge &
                                            unroundedInt(intWidth - 2) & roundCarryBut2)),
                              !signedOut & raw.sign & roundIncrement)

    val exception = Mux(raw.isNan | raw.isInfinite, OpResultException.Invalid,
                        Mux(commonOverflow,         OpResultException.Overflow,
                          Mux(commonInexact,        OpResultException.Inexact,
                                                    OpResultException.NoException)))
    val exceptionSign = !raw.isNan & raw.sign
    val exceptionOut = Mux(signedOut === exceptionSign,
                            B(pow(2, intWidth - 1)),
                            B(0)) |
                       Mux(!exceptionSign, B(pow(2, intWidth - 1) - 1), B(0))

    val result = Mux(exception === OpResultException.Invalid | 
                     exception === OpResultException.Overflow,
                     exceptionOut,
                     roundedInt)
    (result, exception)
  }
}
