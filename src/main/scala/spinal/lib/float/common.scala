package spinal.lib.float

import spinal.core._

object RoundingMode extends SpinalEnum {

  val Even = newElement()
  val Odd = newElement()
  val MinMag = newElement()
  val MaxMag = newElement()
  val Min = newElement()
  val Max = newElement()
}

object OpResultException extends SpinalEnum {
  val NoException = newElement()
  val Invalid = newElement()
  val Overflow = newElement()
  val Inexact = newElement()
}
