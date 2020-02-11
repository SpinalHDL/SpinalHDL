package spinal.lib.bus.bmb.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.bmb.Bmb
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.sim.{StreamDriver, StreamMonitor, StreamReadyRandomizer}

import scala.collection.mutable
import scala.util.Random



abstract class BmbMasterAgent(bus : Bmb, clockDomain: ClockDomain, cmdFactor : Float = 0.5f, rspFactor : Float = 0.5f){
  val cmdQueue = mutable.Queue[() => Unit]()
  val rspQueue = Array.fill(1 << bus.p.sourceWidth)(mutable.Queue[() => Unit]())

  StreamReadyRandomizer(bus.rsp, clockDomain).factor = rspFactor

  def regionAllocate(sizeMax : Int) : SizeMapping
  def regionFree(region : SizeMapping) : Unit
  def regionIsMapped(region : SizeMapping, opcode : Int) : Boolean

  def onRspRead(address : BigInt, data : Seq[Byte]) : Unit = {}
  def onRspRead(address : BigInt, data : Byte) : Unit = {}
  def onCmdWrite(address : BigInt, data : Byte) : Unit = {}

  def getCmd(): () => Unit = {
    //Generate a new CMD if none is pending
    if(cmdQueue.isEmpty) {
      val region = regionAllocate(1 << bus.p.lengthWidth)
      if(region == null) return null
      val length = region.size.toInt-1
      val context = bus.cmd.context.randomizedInt
      val source = bus.cmd.source.randomizedInt
      val address = region.base
      val opcode = List(Bmb.Cmd.Opcode.READ, Bmb.Cmd.Opcode.WRITE)(Random.nextInt(2))
//      val opcode = List(Bmb.Cmd.Opcode.READ)(Random.nextInt(1))
      val startAddress = address
      val endAddress = address + length + 1
      val beatCount = ((((endAddress + bus.p.wordMask) & ~bus.p.wordMask) - (startAddress & ~bus.p.wordMask)) / bus.p.byteCount).toInt
      val mapped = regionIsMapped(region, opcode)

      opcode match {
        case Bmb.Cmd.Opcode.READ => {
          //READ CMD
          cmdQueue.enqueue { () =>
            bus.cmd.address #= address
            bus.cmd.opcode #= Bmb.Cmd.Opcode.READ
            bus.cmd.context #= context
            bus.cmd.source #= source
            bus.cmd.length #= length
            bus.cmd.last #= true
          }

          //READ RSP
          val rspReadData = new Array[Byte](length + 1)
          for(beat <- 0 until beatCount) rspQueue(source).enqueue{ () =>
            val beatAddress = (startAddress & ~(bus.p.byteCount-1)) + beat*bus.p.byteCount
            assert(bus.rsp.context.toInt == context)
            assert(bus.rsp.source.toInt == source)
            assert(bus.rsp.opcode.toInt == (if(mapped) Bmb.Rsp.Opcode.SUCCESS else Bmb.Rsp.Opcode.ERROR))
            val data = bus.rsp.data.toBigInt
            for(byteId <- 0 until bus.p.byteCount; byteAddress = beatAddress + byteId) if(byteAddress >= startAddress && byteAddress < endAddress){
              val byte = (data >> byteId*8).toByte
              rspReadData((byteAddress-startAddress).toInt) = byte
              onRspRead(byteAddress, byte)
            }

            if(beat == beatCount-1){
              assert(bus.rsp.last.toBoolean)
              if(mapped) onRspRead(address, rspReadData)
              regionFree(region)
            } else {
              assert(!bus.rsp.last.toBoolean)
            }
          }
        }
        case Bmb.Cmd.Opcode.WRITE => {
          //WRITE CMD
          for (beat <- 0 until beatCount) cmdQueue.enqueue { () =>
            val beatAddress = (startAddress & ~(bus.p.byteCount - 1)) + beat * bus.p.byteCount
            val data = bus.cmd.data.randomizedBigInt()
            bus.cmd.address #= address
            bus.cmd.opcode #= Bmb.Cmd.Opcode.WRITE
            bus.cmd.data #= data
            bus.cmd.context #= context
            bus.cmd.source #= source
            bus.cmd.length #= length
            bus.cmd.last #= beat == beatCount - 1
            var mask = 0l
            for (byteId <- 0 until bus.p.byteCount; byteAddress = beatAddress + byteId) if (byteAddress >= startAddress && byteAddress < endAddress) {
              if (maskRandom()) {
                mask |= 1l << byteId
                if (mapped) onCmdWrite(byteAddress, (data >> byteId * 8).toByte)
              }

            }
            bus.cmd.mask #= mask
          }

          //WRITE RSP
          rspQueue(source).enqueue { () =>
            assert(bus.rsp.context.toInt == context)
            assert(bus.rsp.source.toInt == source)
            assert(bus.rsp.opcode.toInt == (if (mapped) Bmb.Rsp.Opcode.SUCCESS else Bmb.Rsp.Opcode.ERROR))
            regionFree(region)
          }
        }
      }
    }
    if(cmdQueue.nonEmpty) cmdQueue.dequeue() else null
  }

  def maskRandom() = Random.nextBoolean()
  val driver = StreamDriver(bus.cmd, clockDomain){ _ =>
    val cmd = getCmd()
    if(cmd != null) cmd()
    cmd != null
  }

  val rspMonitor = StreamMonitor(bus.rsp, clockDomain){_ =>
    rspQueue(bus.rsp.source.toInt).dequeue()()
  }
}