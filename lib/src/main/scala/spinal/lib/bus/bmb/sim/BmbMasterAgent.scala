package spinal.lib.bus.bmb.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.bmb.Bmb
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.sim.{StreamDriver, StreamMonitor, StreamReadyRandomizer}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random



abstract class BmbMasterAgent(bus : Bmb, clockDomain: ClockDomain, cmdFactor : Float = 0.5f, rspFactor : Float = 0.5f){
  val cmdQueue = mutable.Queue[() => Unit]()
  val rspQueue = Array.fill(1 << bus.p.access.sourceWidth)(mutable.Queue[() => Unit]())

  var pendingMax = 50
  var pendingCounter = 0
  StreamReadyRandomizer(bus.rsp, clockDomain).factor = rspFactor

  def regionAllocate(sizeMax : Int) : SizeMapping
  def regionFree(region : SizeMapping) : Unit
  def regionIsMapped(region : SizeMapping, opcode : Int) : Boolean

  def onRspRead(address : BigInt, data : Seq[Byte]) : Unit = {}
  def onRspRead(address : BigInt, data : Byte) : Unit = {}
  def onCmdWrite(address : BigInt, data : Byte) : Unit = {}



  def getCmd(): () => Unit = {
    //Generate a new CMD if none is pending
    if(cmdQueue.isEmpty && pendingCounter < pendingMax) {
      val sourceId = bus.p.access.randSource()
      bus.cmd.source #= sourceId
      val ap = bus.p.access.sources(sourceId)
      val region = regionAllocate(1 << ap.lengthWidth)
      if(region == null) return null
      pendingCounter += 1
      val length = region.size.toInt-1
      val context = bus.cmd.context.randomizedLong
      val address = region.base
      var usableOpcodes = ArrayBuffer[Int]()
      if(ap.canRead)  usableOpcodes += Bmb.Cmd.Opcode.READ
      if(ap.canWrite) usableOpcodes += Bmb.Cmd.Opcode.WRITE
      val opcode = usableOpcodes(Random.nextInt(usableOpcodes.size))
      val startAddress = address
      val endAddress = address + length + 1
      val beatCount = ((((endAddress + bus.p.access.wordMask) & ~bus.p.access.wordMask) - (startAddress & ~bus.p.access.wordMask)) / bus.p.access.byteCount).toInt
      val mapped = regionIsMapped(region, opcode)

      opcode match {
        case Bmb.Cmd.Opcode.READ => {
          //READ CMD
          cmdQueue.enqueue { () =>
            bus.cmd.address #= address
            bus.cmd.opcode #= Bmb.Cmd.Opcode.READ
            bus.cmd.context #= context
            bus.cmd.source #= sourceId
            bus.cmd.length #= length
            bus.cmd.last #= true
          }

          //READ RSP
          val rspReadData = new Array[Byte](length + 1)
          for(beat <- 0 until beatCount) rspQueue(sourceId).enqueue{ () =>
            val beatAddress = (startAddress & ~(bus.p.access.byteCount-1)) + beat*bus.p.access.byteCount
            assert(bus.rsp.source.toInt == sourceId)
            assert(bus.rsp.context.toLong == context)
            assert(bus.rsp.opcode.toInt == (if(mapped) Bmb.Rsp.Opcode.SUCCESS else Bmb.Rsp.Opcode.ERROR))
            val data = bus.rsp.data.toBigInt
            for(byteId <- 0 until bus.p.access.byteCount; byteAddress = beatAddress + byteId) if(byteAddress >= startAddress && byteAddress < endAddress){
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
            val beatAddress = (startAddress & ~(bus.p.access.byteCount - 1)) + beat * bus.p.access.byteCount
            val data = bus.cmd.data.randomizedBigInt()
            bus.cmd.address #= address
            bus.cmd.opcode #= Bmb.Cmd.Opcode.WRITE
            bus.cmd.data #= data
            bus.cmd.context #= context
            bus.cmd.source #= sourceId
            bus.cmd.length #= length
            bus.cmd.last #= beat == beatCount - 1
            var mask = 0l
            for (byteId <- 0 until bus.p.access.byteCount; byteAddress = beatAddress + byteId) if (byteAddress >= startAddress && byteAddress < endAddress) {
              if (maskRandom()) {
                mask |= 1l << byteId
                if (mapped) onCmdWrite(byteAddress, (data >> byteId * 8).toByte)
              }

            }
            bus.cmd.mask #= mask
          }

          //WRITE RSP
          rspQueue(sourceId).enqueue { () =>
            assert(bus.rsp.source.toInt == sourceId)
            assert(bus.rsp.context.toLong == context)
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

  var rspSourceLocked = false
  var rspSourceId = 0
  val rspMonitor = StreamMonitor(bus.rsp, clockDomain){_ =>
    val source = bus.rsp.source.toInt
    if(rspSourceLocked){
      assert(source == rspSourceId)
    } else {
      rspSourceLocked = true
      rspSourceId = source
    }
    if(bus.rsp.last.toBoolean) {
      pendingCounter -= 1
      rspSourceLocked = false
    }
    rspQueue(source).dequeue()()
  }
}