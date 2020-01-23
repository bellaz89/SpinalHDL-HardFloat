
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

abstract class HardFloat[A <: HardFloat[A]] extends Bundle {
 
  def mantissaLength : Int
  def exponentLength : Int

  def assertExponentMantissaLengthEqual[D <: HardFloat[D]](that : D) : Unit = {
    assert(this.mantissaLength == that.mantissaLength, 
          "mantissaLength should be equal [this = " + 
          this.mantissaLength + " bits, that = " + 
          that.mantissaLength + " bits]")
    assert(this.exponentLength == that.exponentLength, 
          "exponentLength should be equal [this = " + 
          this.mantissaLength + " bits, that = " + 
          that.mantissaLength + " bits]")
  }

  val sign = Bool
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
  def assignDataExceptSign(that : A) : Unit

  def abs : A = {
    val absVal = cloneOf(this.asInstanceOf[A])
    absVal.sign := False
    absVal.assignDataExceptSign(this.asInstanceOf[A])
    absVal
  }

  def neg : A =  {
    val negVal = cloneOf(this.asInstanceOf[A])
    negVal.sign := !this.sign
    negVal.assignDataExceptSign(this.asInstanceOf[A])
    negVal
  }


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

class IEEEFloat[B](val mantissaWidth : Int,
                   val exponentWidth : Int) 
     extends HardFloat[IEEEFloat[B]] {

  def mantissaLength = mantissaWidth-1
  def exponentLength = exponentWidth

  def getExponentBias = pow(2, BigInt(exponentWidth-1))-1

  def isNormalized = !isExponentZero & !isExponentMax
  def isZero = (exponent ## mantissa) === B(default -> false)
  def isInfinite = isExponentMax & isMantissaZero
  def isNan = isExponentMax & !isMantissaZero
  def isSNan = !mantissa.msb & isNan
  def toBaseIEEEFloat : IEEEFloat[Null] = {
    val ret = IEEEFloat(this.mantissaWidth, this.exponentWidth)
    ret.sign     := this.sign
    ret.exponent := this.exponent
    ret.mantissa := this.mantissa
    ret
  }
  
  def :=[C <: IEEEFloat[C]](that : IEEEFloat[C]) : Unit = {
    this.assertExponentMantissaLengthEqual(that)
    this.assignFromBits(that.asBits)
  }

  def assignDataExceptSign(that : IEEEFloat[B]) : Unit = {
    this.assertExponentMantissaLengthEqual(that)
    this.exponent := that.exponent
    this.mantissa := that.mantissa
  }
}

object IEEEFloat {
  def apply(mantissaWidth : Int,
            exponentWidth : Int) : IEEEFloat[Null] = new IEEEFloat(mantissaWidth,
                                                                   exponentWidth)
}

class IEEEFloat16 extends IEEEFloat[IEEEFloat16](11, 5)
object IEEEFloat16 { def apply = new IEEEFloat16 }
class IEEEFloat32 extends IEEEFloat[IEEEFloat32](24, 8) {

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
object IEEEFloat32 { def apply = new IEEEFloat32 }

class IEEEFloat64 extends IEEEFloat[IEEEFloat64](53, 11) {

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
object IEEEFloat64 { def apply = new IEEEFloat64 }

class IEEEFloat128 extends IEEEFloat[IEEEFloat128](113, 15)
object IEEEFloat128 { def apply = new IEEEFloat128 }
class IEEEFloat256 extends IEEEFloat[IEEEFloat256](237, 19)
object IEEEFloat256 { def apply = new IEEEFloat256 }

class RecFloat[B](val mantissaWidth : Int,
               val exponentWidth : Int) 
      extends HardFloat[RecFloat[B]] { 

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

  def toBaseRecFloat : RecFloat[Null] = {
    val ret = RecFloat(this.mantissaWidth, this.exponentWidth)
    ret.sign     := this.sign
    ret.exponent := this.exponent
    ret.mantissa := this.mantissa
    ret
  }

  def :=[C <: RecFloat[C]](that : RecFloat[C]) : Unit = {
    this.assertExponentMantissaLengthEqual(that)
    this.assignFromBits(that.asBits)
  }


  def assignDataExceptSign(that : RecFloat[B]) : Unit = {
    this.assertExponentMantissaLengthEqual(that)
    this.exponent := that.exponent
    this.mantissa := that.mantissa
  }
}


object RecFloat {
  def apply(mantissaWidth : Int,
            exponentWidth : Int) : RecFloat[Null] = new RecFloat(mantissaWidth,
                                                                exponentWidth)
}

class RecFloat17 extends RecFloat[RecFloat17](11, 5)
object RecFloat17 { def apply = new RecFloat17 }
class RecFloat33 extends RecFloat[RecFloat33](24, 8)
object RecFloat33 { def apply = new RecFloat33 }
class RecFloat65 extends RecFloat[RecFloat65](53, 11)
object RecFloat65 { def apply = new RecFloat65 }
class RecFloat129 extends RecFloat[RecFloat129](113, 15)
object RecFloat129 { def apply = new RecFloat129 }
class RecFloat257 extends RecFloat[RecFloat257](237, 19)
object RecFloat257 { def apply = new RecFloat257 }

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

  def assignDataExceptSign(that : RawFloat) : Unit = {
    this.assertExponentMantissaLengthEqual(that)
    this.exponent := that.exponent
    this.mantissa := that.mantissa
    this.normalized := that.normalized
    this.zero := that.zero
    this.infinite := that.infinite
    this.nan := that.nan
    this.snan := that.snan
  }
}
