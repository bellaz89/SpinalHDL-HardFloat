package spinal.lib.float

import spinal.core._
import spinal.lib._


object priorityEncoderUnbalanceLadder {
  def apply(in: Seq[Bool]) : UInt = PriorityMux(in, (0 to in.size).map(x => U(x, log2Up(in.size) bits)))
  def apply(in: Bits) : UInt = apply(in.asBools)
}


object countLeadingZeros { 
  
  def apply(value : Bits) : UInt = {
    val reversed_value = Bits(value.getWidth bits)
    (0 to value.getWidth-1).foreach(i => reversed_value(i) := value(value.getWidth - i)) 
    priorityEncoderUnbalanceLadder(reversed_value)
  }
}
