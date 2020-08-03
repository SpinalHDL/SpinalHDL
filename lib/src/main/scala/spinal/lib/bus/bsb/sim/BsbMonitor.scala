package spinal.lib.bus.bsb.sim

import spinal.core.ClockDomain
import spinal.lib.bus.bsb.Bsb
import spinal.lib.sim.StreamMonitor
import spinal.core.sim._

object BsbMonitor{
//  def apply(bsb : Bsb, cd : ClockDomain)(body : (Byte, Int,Int) => Unit) = new BsbMonitor(bsb, cd) {
//    override def onByte(value: Byte, source : Int, sink : Int): Unit = body(value, source, sink)
//  }
}

abstract class BsbMonitor(bsb : Bsb, cd : ClockDomain) extends StreamMonitor(bsb, cd){
  addCallback{ p =>
    val mask = p.mask.toBigInt
    val data = p.data.toBigInt
    val source = p.source.toInt
    val sink = p.sink.toInt
    val withLast = bsb.last.toBoolean
    for(i <- 0 until bsb.p.byteCount){
      if(mask.testBit(i)) onByte((data >> i*8).toByte, source, sink)
    }
    if(withLast) onLast(source,sink)
  }

  def onByte(value : Byte, source : Int, sink : Int): Unit
  def onLast(source : Int, sink : Int): Unit
}
