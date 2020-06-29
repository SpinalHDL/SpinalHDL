package spinal.lib.bus.bmb.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.bmb.Bmb
import spinal.lib.sim.{SparseMemory, StreamDriver, StreamMonitor, StreamReadyRandomizer}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


abstract class BmbMonitor(bus : Bmb, clockDomain : ClockDomain) {
  def getByte(address : Long, value : Byte)
  def setByte(address : Long, value : Byte)

  var cmdBeat = 0

  val rspQueue =  Array.fill(1 << bus.p.access.sourceWidth)(mutable.Queue[() => Unit]())


  StreamMonitor(bus.rsp, clockDomain){ payload =>
    rspQueue(payload.source.toInt).dequeue().apply()
  }

//    new SimStreamAssert(bus.cmd,clockDomain)
  StreamMonitor(bus.cmd, clockDomain) { payload =>
    val opcode = bus.cmd.opcode.toInt
    val last = bus.cmd.last.toBoolean
    val source = bus.cmd.source.toInt
    val context = bus.cmd.context.toLong
    opcode match {
      case Bmb.Cmd.Opcode.READ => {
        assert(bus.p.access.sources(source).canRead)
        val length = bus.cmd.length.toLong
        val address = bus.cmd.address.toLong
        val startByte = (address & (bus.p.access.byteCount - 1))
        val endByte = startByte + length + 1
        val rspBeatCount = ((endByte + bus.p.access.byteCount - 1) / bus.p.access.byteCount).toInt
        for (rspBeat <- 0 until rspBeatCount) {
          rspQueue(source).enqueue{ () =>
            val beatAddress = (address & ~(bus.p.access.byteCount - 1)) + rspBeat * bus.p.access.byteCount
            val data = bus.rsp.data.toBigInt
            for (byteId <- 0 until bus.p.access.byteCount) {
              getByte(beatAddress + byteId, (data >> byteId * 8).toByte)
            }
          }
        }
      }
      case Bmb.Cmd.Opcode.WRITE => {
        assert(bus.p.access.sources(source).canWrite)
        val mask = bus.cmd.mask.toLong
        val address = bus.cmd.address.toLong
        val data = bus.cmd.data.toBigInt
        val beatAddress = (address & ~(bus.p.access.byteCount - 1)) + cmdBeat * bus.p.access.byteCount
        for (byteId <- 0 until bus.p.access.byteCount) if ((mask & (1l << byteId)) != 0) {
          setByte(beatAddress + byteId, (data >> byteId * 8).toByte)
        }
        if(bus.cmd.last.toBoolean) rspQueue(source).enqueue{ () => }
      }
      case _ => simFailure("Bad opcode")
    }

    cmdBeat += 1
    if (last) {
      cmdBeat = 0
    }
  }

}
