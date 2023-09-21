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


class PcieMemoryAgent(val memorySize : BigInt = 0) {
  val memory = SparseMemory()


  def getByteAsInt(address : Long) = getByte(address).toInt & 0xFF
  def getByte(address : Long) = memory.read(address)
  def setByte(address : Long, value : Byte) = memory.write(address, value)
  def writeNotification(address : Long, value : Byte) = {}//memory.write(address, value)

  def addPort(cmd : Stream[Fragment[Tlp]],
              rsp : Stream[Fragment[Tlp]],
              clockDomain : ClockDomain,
              withDriver : Boolean,
              withStall : Boolean = true) = {
    // var cmdBeat = 0
    // var writeFragments = ArrayBuffer[() => Unit]() //Allow to apply write data on their rsp (out of oder)
    val tagWidth = 8
    val pageAlignBits = 12
    val dwAlign = pageAlignBits >> 2
    val tagCount = 1 << 8
    val pcieIfDwWidth = cmd.payload.fragment.config.dwCount

    if(withDriver) {
      cmd.ready #= true
      if(withStall)StreamReadyRandomizer(cmd, clockDomain)
    }

    val rspQueue =  Array.fill(tagCount)(mutable.Queue[() => Unit]())

    def addRsp(tag : Int, rsp : () => Unit) = if(withDriver) {
      rspQueue(tag).enqueue(rsp)
    }

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

    var first = true
    var header: BigInt = 0
    StreamMonitor(cmd, clockDomain) { payload =>
      delayed(0) {
        if(first) {
          header = payload.hdr.toBigInt
        }
        val cmdHdr = MemCmdHeader.parse(header)
        val realDwCount = if(cmdHdr.length == 0) 1 << dwAlign else cmdHdr.length
        val trans = (0 until realDwCount).grouped(pcieIfDwWidth).toArray

        (cmdHdr.fmt, cmdHdr.typ) match {
          case (1, 0) => {
            // read
            assert(first && payload.last.toBoolean)
            var addr = (cmdHdr.addr << 2).toLong
            for(curDws <- trans) {
              rspQueue(cmdHdr.tag).enqueue {() =>
                val data = mutable.ArrayBuffer[Byte]()
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
                val rspHdr = CplHeader.createCplDSimple(0, cmdHdr.reqId, cmdHdr.tag, calcLowerAddr(cmdHdr.addr, cmdHdr.firstBe), cmdHdr.length, calcByteCount(realDwCount, cmdHdr.firstBe, cmdHdr.lastBe))
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
                    addr = addr + 1
                  }
                }
                rsp.last #= (curDws == trans.last)
                rsp.data #= data.toArray
                rsp.strb #= curDws.map(x => 1 << (x%4)).reduce(_|_)
                rsp.payload.fragment.hdr #= rspHdr.buildBits()
                rsp.config.withBarFunc generate {
                  rsp.bar_id #= 0
                  rsp.func_num #= 0
                }
                rsp.config.useError generate rsp.error #= 0
                rsp.config.useSeq generate rsp.seq #= 0

              }
            }
          }
          case (3, 0) => {
            // write
            var addr = (cmdHdr.addr << 2).toLong
            for(curDws <- trans) { //transaction level
              for((dw, id) <- curDws.zipWithIndex) { // dw level
                for(i <- 0 until 4) { // byte level
                  if(dw == 0) {
                    if((cmdHdr.firstBe & (1<<i))!=0) {
                      setByte(addr, payload.data.toBytes(4*dw+i))
                    }
                  } else if(dw == realDwCount-1) {
                    if((cmdHdr.lastBe & (1<<i))!=0) {
                      setByte(addr, payload.data.toBytes(4*dw+i))
                    }
                  } else {
                    setByte(addr, payload.data.toBytes(4*dw+i))                    
                  }
                  addr = addr+1
                }
              }
            }
          }
          case _ => SpinalError("do not support this kind of tlp currently")
        }

        if(first) {
          first = false
        }
        if(payload.last.toBoolean) {
          first = true
        }
      }
    }

  }
}
