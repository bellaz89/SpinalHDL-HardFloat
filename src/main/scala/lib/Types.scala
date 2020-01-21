
package spinal.lib.float
import java.nio.ByteBuffer
import spinal.core._
import spinal.lib._

case class HardFloatInterface(val mantissaLength : Int, 
                             val exponentLength : Int) extends Bundle {
  
  def := (that: Bits)   : Unit
  def := (that: BigInt) : Unit = that(B(that, 1 + mantissaLength + 
                                              exponentLength bits))
  
  def apply() : Bits
  val sign = Bool
  val exponent = Bits(exponentLength bits)
  val mantissa = Bits(mantissaLength bits) 

  def getExponentBias : Int
  def isMantissaZero = mantissa === B(0, mantissaLength bits)
  def isMantissaMax = mantissa === B(default -> true, 
                                     mantissaLength bits)
  def isExponentZero = exponent === B(0, exponentLength bits)
  def isExponentMax = exponent === B(default -> true, 
                                     exponentLength bits)
  def isNormalized : Bool
  def isDenormalized : Bool
  def isZero : Bool
  def isInfinite : Bool
  def isPositiveInfinite = isInfinite & isPositive
  def isNegativeInfinite = isInfinite & !isPositive
  def isPositive = !sign
  def isNegative = sign
  def isPositiveZero = isZero & isPositive
  def isNegativeZero = isZero & !isPositive
  def isNan : Bool
  def isSignallingNan : Bool
  def isQuietNan : Bool

abstract case class IEEEFloat(val mantissaLength : Int,
                              val exponentLength : Int,
                              val exponentBias : Int) 
     extends HardFloatInterface(mantissaLength, exponentLength) {
  
  def := (that: Bits) : Unit = {
    sign := that(0)
    exponent := that(1 to exponentLength)
    mantissa := that(exponentLength+1 to exponentLength+mantissaLength)

  }
  def apply() : Bits = sign ## exponent ## mantissa
  
  def getExponentBias = exponentBias

  def isNormalized = !isExponentZero & !isExponentMax
  def isDenormalized = isExponentZero & !isMantissaZero
  def isZero = (mantissa ## exponent) === B(0, mantissaLength +
                                               exponentLength bits)
  def isInfinite = isExponentMax & isMantissaZero
  def isNan = isExponentMax & !isMantissaZero
  def isSignallingNan = !mantissa(0) & isNan
  def isQuietNan = mantissa(0) & isExponentMax
}

case class IEEEFloat16  extends IEEEFloat(10, 5, 15)
case class IEEEFloat32  extends IEEEFloat(23, 8, 127)
case class IEEEFloat64  extends IEEEFloat(52, 11, 1023)
case class IEEEFloat128 extends IEEEFloat(112, 15, 16383)
case class IEEEFloat256 extends IEEEFloat(236, 19, 262143)

//case class RawFloating extends FloatingInterface
//case class RecFloating extends FloatingInterface
