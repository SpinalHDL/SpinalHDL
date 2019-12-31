package spinal.lib.bus.bmb.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.sim.{Phase, StreamDriver, StreamMonitor, StreamReadyRandomizer}
import scala.collection.mutable
import scala.util.Random
import spinal.lib.bus.misc.SizeMapping


class BmbMemoryAgent(val memorySize : BigInt) {
  val memory = new Array[Byte](memorySize.toInt)
  Random.nextBytes(memory)

  def getByteAsInt(address : Long) = memory(address.toInt).toInt & 0xFF
  def getByte(address : Long) = memory(address.toInt)
  def setByte(address : Long, value : Byte) = memory(address.toInt) = value

  def addPort(bus : Bmb, busAddress : Long, clockDomain : ClockDomain, withDriver : Boolean) = {
    var cmdBeat = 0

    if(withDriver) StreamReadyRandomizer(bus.cmd, clockDomain)

    val rspQueue =  Array.fill(1 << bus.p.sourceWidth)(mutable.Queue[() => Unit]())
    val rspQueueNonEmpty = mutable.ArrayBuffer[mutable.Queue[() => Unit]]()
    def addRsp(source : Int)(body : => Unit) = if(withDriver) {

      if(rspQueue(source).isEmpty) rspQueueNonEmpty += rspQueue(source)
      rspQueue(source) += (() => body)
    }
    if(withDriver) StreamDriver(bus.rsp, clockDomain){ _ =>
      if(rspQueueNonEmpty.nonEmpty){
        val queue = rspQueueNonEmpty(Random.nextInt(rspQueueNonEmpty.length))
        queue.dequeue()()
        if(queue.isEmpty) rspQueueNonEmpty -= queue
        true
      } else {
        false
      }
    }

    StreamMonitor(bus.cmd, clockDomain) { payload =>
      delayed(0) { //To be sure the of timing relation ship between CMD and RSP
        val opcode = bus.cmd.opcode.toInt
        val last = bus.cmd.last.toBoolean
        val source = bus.cmd.source.toInt
        val context = bus.cmd.context.toInt
        opcode match {
          case Bmb.Cmd.Opcode.READ => {
            assert(bus.p.canRead)
            val length = bus.cmd.length.toLong
            val address = bus.cmd.address.toLong + busAddress
            val startByte = (address & (bus.p.byteCount - 1))
            val endByte = startByte + length + 1
            val rspBeatCount = ((endByte + bus.p.byteCount - 1) / bus.p.byteCount).toInt
            for (rspBeat <- 0 until rspBeatCount) {
              addRsp(source) {
                val beatAddress = (address & ~(bus.p.byteCount - 1)) + rspBeat * bus.p.byteCount
                bus.rsp.last #= rspBeat == rspBeatCount - 1
                bus.rsp.opcode #= Bmb.Rsp.Opcode.SUCCESS
                bus.rsp.source  #= source
                bus.rsp.context #= context
                var data = BigInt(0)
                for (byteId <- 0 until bus.p.byteCount) {
                  val byteAddress = beatAddress + byteId
                  val byte = getByteAsInt(byteAddress)
                  data |= (BigInt(byte) << byteId * 8)
                }
                bus.rsp.data #= data
              }
            }
          }
          case Bmb.Cmd.Opcode.WRITE => {
            assert(bus.p.canWrite)
            val mask = bus.cmd.mask.toLong
            val address = bus.cmd.address.toLong + busAddress
            val data = bus.cmd.data.toBigInt
            val beatAddress = (address & ~(bus.p.byteCount - 1)) + cmdBeat * bus.p.byteCount
            for (byteId <- 0 until bus.p.byteCount) if ((mask & (1l << byteId)) != 0) {
              setByte(beatAddress + byteId, (data >> byteId * 8).toByte)
            }
            if (last) {
              addRsp(source) {
                bus.rsp.last #= true
                bus.rsp.opcode #= Bmb.Rsp.Opcode.SUCCESS
                bus.rsp.source  #= source
                bus.rsp.context #= context
              }
            }
          }
          case _ => simFailure("Bad opcode")
        }

        cmdBeat += 1
        if (last) {
          cmdBeat = 0
        }
      }
    }
  }
}
