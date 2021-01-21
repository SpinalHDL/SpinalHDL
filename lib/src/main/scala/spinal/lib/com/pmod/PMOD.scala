package spinal.lib.com.pmod

import spinal.core._
import spinal.lib._

abstract class PMODBundle extends Bundle with IMasterSlave {
  val pin1 = Bits(1 bit)
  val pin2 = Bits(1 bit)
  val pin3 = Bits(1 bit)
  val pin4 = Bits(1 bit)
  val pin7 = Bits(1 bit)
  val pin8 = Bits(1 bit)
  val pin9 = Bits(1 bit)
  val pin10 = Bits(1 bit)
}
