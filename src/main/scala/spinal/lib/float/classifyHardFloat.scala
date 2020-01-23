
package spinal.lib.float

import spinal.core._

object classifyHardFloat {
  def apply[A <: HardFloat[A]](hardFloat : HardFloat[A]) : Bits = {
    val classifyWord = Bits(32 bits)
    classifyWord(31 downto 10) := B(0)
    classifyWord(0) := hardFloat.isNegativeInfinite
    classifyWord(1) := hardFloat.isNegative & hardFloat.isNormalized 
    classifyWord(2) := hardFloat.isNegative & hardFloat.isDenormalized
    classifyWord(3) := hardFloat.isNegativeZero
    classifyWord(4) := hardFloat.isPositiveZero
    classifyWord(5) := hardFloat.isPositive & hardFloat.isDenormalized
    classifyWord(6) := hardFloat.isPositive & hardFloat.isNormalized
    classifyWord(7) := hardFloat.isPositiveInfinite
    classifyWord(8) := hardFloat.isSNan
    classifyWord(9) := hardFloat.isQNan
    classifyWord
  }
}
