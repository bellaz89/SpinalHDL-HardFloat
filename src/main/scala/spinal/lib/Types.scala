
package spinal.lib.float
import math._
import java.nio.ByteBuffer
import spinal.core._
import spinal.lib._

object FloatType extends SpinalEnum {
  val Normalized   = newElement()
  val Denormalized = newElement() 
  val Zero         = newElement()
  val Infinite     = newElement()
  val SNan         = newElement()
  val QNan         = newElement()
}

abstract class HardFloatInterface(val mantissaLength : Int, 
                                  val exponentLength : Int) extends Bundle {
 
  def := (that: Bits) : Unit = {
    sign := that(exponentLength+mantissaLength)
    exponent := that(exponentLength+mantissaLength-1 downto mantissaLength)
    mantissa := that(mantissaLength-1 downto 0)
  }

  def apply() = sign ## exponent ## mantissa
  
  val sign = Bool
  val exponent = Bits(exponentLength bits)
  val mantissa = Bits(mantissaLength bits) 

  def getExponentBias : Int
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
}

class IEEEFloat(override val mantissaLength : Int,
                override val exponentLength : Int,
                val exponentBias : Int) 
     extends HardFloatInterface(mantissaLength, exponentLength) {
  
  def getExponentBias = exponentBias

  def isNormalized = !isExponentZero & !isExponentMax
  def isZero = (exponent ## mantissa) === B(default -> false)
  def isInfinite = isExponentMax & isMantissaZero
  def isNan = isExponentMax & !isMantissaZero
  def isSNan = !mantissa.msb & isNan
}

class IEEEFloat16()  extends IEEEFloat(10, 5, 15)
class IEEEFloat32()  extends IEEEFloat(23, 8, 127)
class IEEEFloat64()  extends IEEEFloat(52, 11, 1023)
class IEEEFloat128() extends IEEEFloat(112, 15, 16383)
class IEEEFloat256() extends IEEEFloat(236, 19, 262143)

class RecFloat(override val mantissaLength : Int,
               override val exponentLength : Int,
               val exponentBias : Int) 
      extends HardFloatInterface(mantissaLength, exponentLength) {

  def getExponentBias = exponentBias
  def isNormalized = (exponent.asUInt >= pow(2, exponentLength-2).toInt + 2) & !isZero & !isInfinite & !isNan
  def isZero = exponent(exponentLength-1 downto exponentLength-4) === B"000"
  def isInfinite = exponent(exponentLength-1 downto exponentLength-4) === B"110"
  def isNan = exponent(exponentLength-1 downto exponentLength-4) === B"111"
  def isSNan = !mantissa.msb & isNan
  def isValidRecFloat = !isInvalidRecFloat
  def isInvalidRecFloat = ((isZero & mantissa =/= B(default -> false)) |
                          (isNormalized & exponent.asUInt > 3*pow(2, exponentLength-2).toInt-1) |
                          (isDenormalized & exponent.asUInt < pow(2, exponentLength).toInt + 2 - mantissaLength))

}

class RecFloat17()  extends RecFloat(10, 6, 15)
class RecFloat33()  extends RecFloat(23, 9, 127)
class RecFloat65()  extends RecFloat(52, 12, 1023)
class RecFloat129() extends RecFloat(112, 16, 16383)
class RecFloat257() extends RecFloat(236, 20, 262143)

