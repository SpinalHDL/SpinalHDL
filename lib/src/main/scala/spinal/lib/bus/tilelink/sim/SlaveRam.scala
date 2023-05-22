package spinal.lib.bus.tilelink.sim

import spinal.core.ClockDomain
import spinal.lib.bus.tilelink.Bus
import spinal.lib.sim.SparseMemory


class SlaveRam(bus: Bus, cd: ClockDomain) extends SlaveAgent(bus, cd) {
  val mem = SparseMemory()

  override def onGet(debugId: Long, source: Int, address: Long, bytes: Int) = {
    accessAckData(source, address, mem.readBytes(address, bytes))
  }

  override def onPutPartialData(source: Int, address: Long, size: Int, mask: Array[Boolean], data: Array[Byte]) = {
    val burstBytes = (1 << size)
    val isLast = (address & (burstBytes - 1)) >= burstBytes - bus.p.dataBytes
    mem.write(address, data, mask)
    if (isLast) accessAck(source, size)
  }
}