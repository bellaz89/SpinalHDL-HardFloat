
package spinal.lib.float
import java.lang.Float.floatToRawIntBits
import java.lang.Double.doubleToRawLongBits
import spinal.core._
import spinal.lib._
import spire.math._

object FloatType extends SpinalEnum {
  val Normalized   = newElement()
  val Denormalized = newElement() 
  val Zero         = newElement()
  val Infinite     = newElement()
  val SNan         = newElement()
  val QNan         = newElement()
}

trait HardFloat[A <: HardFloat[A]] extends Bundle {
  
  val sign = Bool
  def mantissaLength : Int
  def exponentLength : Int

  def := (that: Bits) : Unit = {
    sign := that(exponentLength+mantissaLength)
    exponent := that(exponentLength+mantissaLength-1 downto mantissaLength)
    mantissa := that(mantissaLength-1 downto 0)
  }

  def apply() = sign ## exponent ## mantissa
  
  val exponent = Bits(exponentLength bits)
  val mantissa = Bits(mantissaLength bits) 

  def getExponentBias : BigInt
  def isMantissaZero = mantissa === B(default -> false)
  def isMantissaMax = mantissa === B(default -> true)
  def isExponentZero = exponent === B(default -> false)
  def isExponentMax = exponent === B(default -> true)
  def isNormalized : Bool
  def isDenormalized = isValue & !isNormalized  & !isZero
  def isZero : Bool
  def isInfinite : Bool
  def isPositiveInfinite = isInfinite & isPositive
  def isNegativeInfinite = isInfinite & isNegative
  def isPositive = !isNegative
  def isNegative = sign
  def isPositiveZero = isZero & isPositive
  def isNegativeZero = isZero & isNegative
  def isNan : Bool
  def isSNan : Bool
  def isQNan = isNan & !isSNan
  def isSpecial = isNan | isInfinite
  def isValue = !isSpecial
  def floatType = {
    
    val ret = FloatType()
    
    when(isNormalized){
      ret := FloatType.Normalized
    }.elsewhen(isDenormalized) {
      ret := FloatType.Denormalized
    }.elsewhen(isZero) {
      ret := FloatType.Zero
    }.elsewhen(isInfinite) {
      ret := FloatType.Infinite
    }.elsewhen(isSNan) {
      ret := FloatType.SNan
    }.otherwise{
      ret := FloatType.QNan
    }
    
    ret
  }

//  def ===[A](that : [A]) : Bool
//  def =/=[A](that : [A]) : Bool = !(this === that)
//  def >[A]
//  def <[A]
//  def >=[A]
//  def <=[A]
//  def bitEquals[A](that : [A]) : Bool = (this === that) & (this.sign === that.sign)
//  def defaultZero : A
//  def defaultOne : A
//  def defaultNan : A
//  def defaultInf : A 
//  def defaultSNan : A
//  def defaultQNan : A
}

class IEEEFloat(val mantissaWidth : Int,
                val exponentWidth : Int) 
     extends HardFloat[IEEEFloat] {

  def mantissaLength = mantissaWidth-1
  def exponentLength = exponentWidth

  def getExponentBias = pow(2, BigInt(exponentWidth-1))-1

  def isNormalized = !isExponentZero & !isExponentMax
  def isZero = (exponent ## mantissa) === B(default -> false)
  def isInfinite = isExponentMax & isMantissaZero
  def isNan = isExponentMax & !isMantissaZero
  def isSNan = !mantissa.msb & isNan
}

class IEEEFloat16 extends IEEEFloat(11, 5)
class IEEEFloat32 extends IEEEFloat(24, 8) {

  def assignFromFloat(value : Float) : Unit = {
    
    val rawVal = floatToRawIntBits(value)
    val rawSign = (rawVal >> 31) & 0x1
    val rawExponent = (rawVal >> 23) & 0xFF
    val rawMantissa = rawVal & 0x7FFFFF
    
    sign := B(rawSign)(0)
    exponent := B(rawExponent)
    rawMantissa := B(rawMantissa)
  }
}

class IEEEFloat64 extends IEEEFloat(53, 11) {

  def assignFromDouble(value : Double) : Unit = {
  
    val rawVal = doubleToRawLongBits(value)
    val rawSign = (rawVal >> 63) & 0x1
    val rawExponent = (rawVal >> 52) & 0x7FF
    val rawMantissa = rawVal & (BigInt(0xFFFFFF) | (BigInt(0xFFFFFFF) << 24)) 
    
    sign := B(rawSign)(0)
    exponent := B(rawExponent)
    rawMantissa := B(rawMantissa)
  }
}

class IEEEFloat128 extends IEEEFloat(113, 15)
class IEEEFloat256 extends IEEEFloat(237, 19)

class RecFloat(val mantissaWidth : Int,
               val exponentWidth : Int) 
      extends HardFloat[RecFloat] { 

  def mantissaLength = mantissaWidth-1
  def exponentLength = exponentWidth+1

  def getExponentBias = pow(2, BigInt(exponentWidth-1))*2 + 1
  def getSignedExponent = (exponent.asUInt - U(getExponentBias, exponent.getWidth bits)).asSInt
  
  def isNormalized = (exponent.asUInt >= pow(2, exponentWidth-2).toInt + 2) & !isZero & !isInfinite & !isNan
  def isZero = exponent(exponentWidth-1 downto exponentWidth-4) === B"000"
  def isInfinite = exponent(exponentWidth-1 downto exponentWidth-4) === B"110"
  def isNan = exponent(exponentWidth-1 downto exponentWidth-4) === B"111"
  def isSNan = !mantissa.msb & isNan
  def isValidRecFloat = !isInvalidRecFloat
  def isInvalidRecFloat = ((isZero & mantissa =/= B(default -> false)) |
                          (isNormalized & exponent.asUInt > 3*pow(2, exponentWidth-2).toInt-1) |
                          (isDenormalized & exponent.asUInt < pow(2, exponentWidth).toInt + 2 - mantissaWidth))

}

class RecFloat17()  extends RecFloat(11, 5)
class RecFloat33()  extends RecFloat(24, 8)
class RecFloat65()  extends RecFloat(53, 11)
class RecFloat129() extends RecFloat(113, 15)
class RecFloat257() extends RecFloat(237, 19)

class RawFloat(val mantissaWidth : Int,
               val exponentWidth : Int)
      extends HardFloat[RawFloat] {
  def mantissaLength = mantissaWidth-1
  def exponentLength = exponentWidth+2
  
  def getExponentBias = pow(2, BigInt(exponentWidth-1))*2 + 1
  def getSignedExponent = (exponent.asUInt - U(getExponentBias, exponent.getWidth bits)).asSInt

  val normalized = Bool
  val zero = Bool
  val infinite = Bool
  val nan = Bool 
  val snan = Bool
  
  def isNormalized = normalized
  def isZero = zero
  def isInfinite = infinite
  def isNan = nan
  def isSNan = snan
}
