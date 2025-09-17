package spinal.lib.system.dma.sg2.sim

import spinal.core.{BooleanPimped, IntPimped}
import spinal.lib._
import spinal.lib.bus.tilelink.sim.{MasterAgent, MemoryAgent}

import java.nio.ByteBuffer

class SimCtrl(ctrl : MasterAgent, offset : Long) {
  def getReg(address : Int) = ctrl.getInt(0, address + offset)
  def putReg(address : Int, data : Int) = ctrl.putInt(0, address + offset, data)

  def start(): Unit = {
    putReg(0x0, 1)
  }
  def setNext(next : Long): Unit = {
    putReg(0x10, next.toInt)
    putReg(0x14, (next >> 32).toInt)
  }
  def busy(): Boolean = (getReg(0) & 1).toBoolean
  def stop(): Unit = {
    putReg(0, 2)
    while(busy()){}
  }

  def enableIrq(): Unit = {
    putReg(0x0, 0x100)
  }
  def setIrq(idle : Boolean = false, delay : Option[Int] = Option.empty[Int], counter : Option[Int] = Option.empty[Int]): Unit = {
    var v = 0
    v |= idle.toInt
    delay.foreach(d => v |= 2 | (d << 16))
    counter.foreach(d => v |= 4 | (d << 24))
    putReg(0x4, v)
  }
}
