package spinal.lib.bus.pcie.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{Phase, SimStreamAssert, SparseMemory, StreamDriver, StreamMonitor, StreamReadyRandomizer}

import scala.collection.mutable
import scala.util.Random
import spinal.lib.bus.misc.SizeMapping

import scala.collection.mutable.ArrayBuffer
import spinal.lib.bus.pcie.Tlp
import spinal.lib.bus.pcie.Header


class PcieMemoryAgent(readCmd: Stream[Fragment[Tlp]], writeCmd: Stream[Fragment[Tlp]], rsp: Stream[Fragment[Tlp]], clockDomain: ClockDomain, withDriver: Boolean = true, withStall: Boolean = true) {
  val memory = SparseMemory()

  def getByteAsInt(address : Long) = getByte(address).toInt & 0xFF
  def getByte(address : Long) = memory.read(address)
  def setByte(address : Long, value : Byte) = memory.write(address, value)
  def writeNotification(address : Long, value : Byte) = {}//memory.write(address, value)
  
  val tagWidth = 8
  val pageAlignBits = 12
  val dwAlign = pageAlignBits >> 2
  val tagCount = 1 << 8
  val pcieIfDwWidth = writeCmd.payload.fragment.config.dwCount

  if(withDriver) {
    writeCmd.ready #= true
    readCmd.ready #= true
    if(withStall){
      StreamReadyRandomizer(writeCmd, clockDomain)
      StreamReadyRandomizer(readCmd, clockDomain)
    }
  }

  val rspQueue =  Array.fill(tagCount)(mutable.Queue[() => Unit]())

  if(withDriver) {
    val driver = StreamDriver(rsp, clockDomain){ _ =>
      val threads = rspQueue.filter(_.nonEmpty)
      if(threads.nonEmpty) {
        val id = Random.nextInt(threads.size)
        threads(id).dequeue().apply()
        assert(threads(id).isEmpty)
        true
      } else {
        false
      }
    }
    if(!withStall) driver.transactionDelay = () => 0
  }


  def calcByteCount(realDwCount: Int, firstBe: Int, lastBe: Int): Int = {
    val firstBeSum = Util.countOne(firstBe)
    val lastBeSum = Util.countOne(lastBe)
    if(realDwCount == 1) {
      firstBeSum
    } else {
      (realDwCount-2)*4 + firstBeSum + lastBeSum
    }
  }

  def calcLowerAddr(dwAddr: BigInt, firstBe: Int): Int = {
    val addrMasked = ((dwAddr & ((1<<5)-1)) << 2).toInt
    assert(firstBe <= 16)
    if((firstBe & 1)==0|| firstBe == 0) {
      addrMasked + 1
    } else if((firstBe & 2) == 0) {
      addrMasked + 2
    } else if((firstBe & 4) == 0) {
      addrMasked + 4
    } else if((firstBe & 8) == 0) {
      addrMasked + 8
    } else {
      SpinalError("????")
    }
  }

  StreamMonitor(readCmd, clockDomain) { payload =>

    delayed(0) {
      assert(payload.last, "read req tlp is only 1 beat")
      val cmdHdr = MemCmdHeader.parse(payload.hdr.toBigInt)
      assert(cmdHdr.fmt==1 && cmdHdr.typ==0, "format error")

      val realDwCount = if(cmdHdr.length == 0) 1 << dwAlign else cmdHdr.length
      val trans = (0 until realDwCount).grouped(pcieIfDwWidth).toArray
      var addr = (cmdHdr.addr << 2).toLong
      val allData = mutable.ArrayBuffer[Byte]()
      for(curDws <- trans) {
        val data = mutable.ArrayBuffer[Byte]()
        for((dw, id) <- curDws.zipWithIndex) {
          for(i <- 0 until 4) {
            var byte: Byte = Byte.MinValue
            if(dw == 0) {
              if((cmdHdr.firstBe & (1<<i))!=0) {
                byte = getByte(addr)
              }
            } else if(dw == realDwCount-1) {
              if((cmdHdr.lastBe & (1<<i))!=0) {
                byte = getByte(addr)
              }
            } else {
              byte = getByte(addr)
            }
            data.append(byte)
            addr = addr+1
          }
        }
        allData.appendAll(data)
        val rspHdr = CplHeader.createCplDSimple(0, cmdHdr.reqId, cmdHdr.tag, calcLowerAddr(cmdHdr.addr, cmdHdr.firstBe), cmdHdr.length, calcByteCount(realDwCount, cmdHdr.firstBe, cmdHdr.lastBe))
        
        rspQueue(cmdHdr.tag).enqueue{()=>
          rsp.last #= (curDws == trans.last)
          rsp.data #= data.toArray
          rsp.payload.fragment.hdr #= rspHdr.buildBits()
          rsp.config.useStrb generate rsp.strb #= curDws.map(x => 1 << (x%4)).reduce(_|_)
          rsp.config.withBarFunc generate {
            rsp.bar_id #= 0
            rsp.func_num #= 0
          }
          rsp.config.useError generate rsp.error #= 0
          rsp.config.useSeq generate rsp.seq #= 0
        }
      }
      println(s"[INFO]: receive read request: ${cmdHdr}, data: ${allData}")
    }
  }

  var first = true
  var header: BigInt = 0
  val data = mutable.ArrayBuffer[Byte]()
  StreamMonitor(writeCmd, clockDomain) { payload =>
    delayed(0) {
      if(first) {
        header = payload.hdr.toBigInt
      }
      val cmdHdr = MemCmdHeader.parse(payload.hdr.toBigInt)
      assert(cmdHdr.fmt==3 && cmdHdr.typ==0, "format error")

      val realDwCount = if(cmdHdr.length == 0) 1 << dwAlign else cmdHdr.length
      val trans = (0 until realDwCount).grouped(pcieIfDwWidth).toArray
      var addr = (cmdHdr.addr << 2).toLong

      val dataBytes = payload.data.toBytes

      for((dw, id) <- payload.strb.toBooleans.zipWithIndex) {
        if(dw) {
          for(i <- 0 until 4) {
            data.append(dataBytes(id*4+i))
          }
        }
      }

      if(payload.last.toBoolean) {
        val dataFinal = data.toArray
        assert(dataFinal.size == cmdHdr.length)
        println(s"[INFO]: receive write request: ${cmdHdr}, data: ${dataFinal}")
        for((byte, id) <- dataFinal.zipWithIndex) {

          if(id<4) {
            if((cmdHdr.firstBe & (1<<id))!=0) {
              setByte(addr+id, byte)
            }
          } else if(id>=dataFinal.size-4) {
            if((cmdHdr.lastBe & (1<<(id-dataFinal.size+4)))!=0) {
              setByte(addr+id, byte)
            }
          } else {
            setByte(addr+id, byte)
          }
        }
      }

      if(first) {
        first = false
      }
      if(payload.last.toBoolean) {
        first = true
        data.clear()
      }
    }
  }
}
