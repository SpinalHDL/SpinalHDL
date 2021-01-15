package spinal.lib.com.pmod

import spinal.core._

abstract class PMODBundle extends Bundle {
  val pin1 = Bits(1 bit)
  val pin2 = Bits(1 bit)
  val pin3 = Bits(1 bit)
  val pin4 = Bits(1 bit)
  val pin7 = Bits(1 bit)
  val pin8 = Bits(1 bit)
  val pin9 = Bits(1 bit)
  val pin10 = Bits(1 bit)

  def build()
}

object pmod {
  def apply[T <: PMODBundle](i: T) = {
    i.build()
    i
  }
}
