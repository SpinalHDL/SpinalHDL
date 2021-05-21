package spinal.lib.bus.bmb.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.sim.{Phase, SimStreamAssert, SparseMemory, StreamDriver, StreamMonitor, StreamReadyRandomizer}

import scala.collection.mutable
import scala.util.Random
import spinal.lib.bus.misc.SizeMapping

import scala.collection.mutable.ArrayBuffer


class BmbMemoryAgent(val memorySize : BigInt = 0) {
  val memory = SparseMemory()


  def getByteAsInt(address : Long) = getByte(address).toInt & 0xFF
  def getByte(address : Long) = memory.read(address)
  def setByte(address : Long, value : Byte) = memory.write(address, value)
  def writeNotification(address : Long, value : Byte) = {}//memory.write(address, value)

  def addPort(bus : Bmb,
              busAddress : Long,
              clockDomain : ClockDomain,
              withDriver : Boolean,
              withStall : Boolean = true) = {
    var cmdBeat = 0
    var writeFragments = ArrayBuffer[() => Unit]() //Allow to apply write data on their rsp (out of oder)

    if(withDriver) {
      bus.cmd.ready #= true
      if(withStall)StreamReadyRandomizer(bus.cmd, clockDomain)
    }

    val rspQueue =  Array.fill(1 << bus.p.access.sourceWidth)(mutable.Queue[mutable.Queue[() => Unit]]())
    var rspActive =  mutable.Queue[() => Unit]()

    def addRsp(source : Int,rsps : mutable.Queue[() => Unit]) = if(withDriver) {
      rspQueue(source).enqueue(rsps)
    }

    if(withDriver) {
      val driver = StreamDriver(bus.rsp, clockDomain){ _ =>
        if(rspActive.isEmpty){
          val threads = rspQueue.filter(_.nonEmpty)
          if(threads.nonEmpty){
            val thread = threads(Random.nextInt(threads.size))
            rspActive = thread.dequeue()
          }
        }
        if(rspActive.nonEmpty){
          rspActive.dequeue().apply()
          true
        } else {
          false
        }
      }
      if(!withStall) driver.transactionDelay = () => 0
    }

//    new SimStreamAssert(bus.cmd,clockDomain)
    StreamMonitor(bus.cmd, clockDomain) { payload =>
      delayed(0) { //To be sure the of timing relation ship between CMD and RSP
        val opcode = bus.cmd.opcode.toInt
        val last = bus.cmd.last.toBoolean
        val source = bus.cmd.source.toInt
        val context = bus.cmd.context.toLong
        opcode match {
          case Bmb.Cmd.Opcode.READ => {
            assert(bus.p.access.sources(source).canRead)
            val length = bus.cmd.length.toLong
            val address = bus.cmd.address.toLong + busAddress
            val startByte = (address & (bus.p.access.byteCount - 1))
            val endByte = startByte + length + 1
            val rspBeatCount = ((endByte + bus.p.access.byteCount - 1) / bus.p.access.byteCount).toInt
            val rsps = mutable.Queue[() => Unit]()
            for (rspBeat <- 0 until rspBeatCount) {
              rsps.enqueue{ () =>
                val beatAddress = (address & ~(bus.p.access.byteCount - 1)) + rspBeat * bus.p.access.byteCount
                bus.rsp.last #= rspBeat == rspBeatCount - 1
                bus.rsp.opcode #= Bmb.Rsp.Opcode.SUCCESS
                bus.rsp.source  #= source
                bus.rsp.context #= context
                var data = BigInt(0)
                for (byteId <- 0 until bus.p.access.byteCount) {
                  val byteAddress = beatAddress + byteId
                  val byte = getByteAsInt(byteAddress)
                  data |= (BigInt(byte) << byteId * 8)
                }
                bus.rsp.data #= data
              }
            }
            addRsp(source, rsps)
          }
          case Bmb.Cmd.Opcode.WRITE => {
            assert(bus.p.access.sources(source).canWrite)
            val mask = bus.cmd.mask.toLong
            val address = bus.cmd.address.toLong + busAddress
            val data = bus.cmd.data.toBigInt
            val beatAddress = (address & ~(bus.p.access.byteCount - 1)) + cmdBeat * bus.p.access.byteCount
            for (byteId <- 0 until bus.p.access.byteCount) if ((mask & (1l << byteId)) != 0) {
              writeNotification(beatAddress + byteId, (data >> byteId * 8).toByte)
            }
            writeFragments += {() =>
              for (byteId <- 0 until bus.p.access.byteCount) if ((mask & (1l << byteId)) != 0) {
                setByte(beatAddress + byteId, (data >> byteId * 8).toByte)
              }
            }
            if (last) {
              writeFragments.foreach(_.apply())
              writeFragments.clear()

              val rsps = mutable.Queue[() => Unit]()
              rsps += { () =>
                bus.rsp.last #= true
                bus.rsp.opcode #= Bmb.Rsp.Opcode.SUCCESS
                bus.rsp.source  #= source
                bus.rsp.context #= context
              }
              addRsp(source, rsps)
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
