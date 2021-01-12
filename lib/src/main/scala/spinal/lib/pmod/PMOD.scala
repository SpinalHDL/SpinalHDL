package spinal.lib.pmod

import spinal.core._

abstract class PMODBundle extends Bundle {
  val pin1 = Bool
  val pin2 = Bool
  val pin3 = Bool
  val pin4 = Bool
  val pin7 = Bool
  val pin8 = Bool
  val pin9 = Bool
  val pin10 = Bool

  def build()
}

object pmod {
  def apply[T <: PMODBundle](i: T) = {
    i.build()
    i
  }
}
