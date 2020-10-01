package spinal.lib.bus.bsb

import spinal.core._
import spinal.lib._
case class BsbParameter(byteCount : Int,
                        sourceWidth : Int,
                        sinkWidth : Int,
                        withMask : Boolean = true){

}

case class BsbTransaction(p : BsbParameter) extends Bundle{
  val data = Bits(p.byteCount*8 bits)
  val mask = Bits(p.byteCount bits)
  val source = UInt(p.sourceWidth bits)
  val sink = UInt(p.sinkWidth bits)
  val last = Bool()
}

object Bsb{
  def apply(p : BsbParameter) = new Bsb(BsbTransaction(p))
}