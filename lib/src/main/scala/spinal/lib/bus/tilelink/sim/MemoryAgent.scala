package spinal.lib.bus.tilelink.sim

import spinal.core.ClockDomain
import spinal.lib.bus.tilelink.{Bus, Opcode}
import spinal.lib.sim.SparseMemory

import scala.util.Random


class MemoryAgent(bus: Bus, cd: ClockDomain, seed : Long = Random.nextInt())(implicit idCallback : IdCallback) extends MonitorSubscriber{
  val mem = SparseMemory(seed)

  val monitor = new Monitor(bus, cd).add(this)
  val driver = new SlaveDriver(bus, cd)

  override def onA(a: TransactionA) = a.opcode match {
    case Opcode.A.GET => {
      idCallback.call(a.debugId)(new OrderingArgs(a.address, a.bytes))
      val d = TransactionD(a)
      d.opcode = Opcode.D.ACCESS_ACK_DATA
      d.data = mem.readBytes(a.address.toLong, a.bytes)
      driver.scheduleD(d)
    }
    case Opcode.A.PUT_PARTIAL_DATA | Opcode.A.PUT_PARTIAL_DATA => {
      idCallback.call(a.debugId)(new OrderingArgs(a.address, a.bytes))
      mem.write(a.address.toLong, a.data, a.mask)
      val d = TransactionD(a)
      d.opcode = Opcode.D.ACCESS_ACK
      driver.scheduleD(d)
    }
  }
  override def onD(d: TransactionD) = {}
}