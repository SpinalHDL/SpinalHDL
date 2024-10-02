package spinal.lib.system.dma.sg2.sim

import spinal.core.IntPimped
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
    putReg(0x14, next >> 32 toInt)
  }
  def busy(): Boolean = (getReg(0) & 1).toBoolean
}
