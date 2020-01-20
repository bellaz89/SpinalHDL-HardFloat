package spinal.lib

import spinal.core._
import spinal.lib._


object PriorityEncoderUnbalanceLadder {
  def apply(in: Seq[Bool]): UInt = PriorityMux(in, (0 to in.size-1).map(x => U(x, log2Up(in.size-1) bits)))
  def apply(in: Bits): UInt = apply(in.asBools)
}


