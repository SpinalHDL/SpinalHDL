package spinal.lib.bus.pcie.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.Stream
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Ar, Axi4Aw, Axi4B, Axi4R, Axi4ReadOnly, Axi4W, Axi4WriteOnly}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.sim.{SimData, StreamDriver, StreamMonitor, StreamReadyRandomizer}

import scala.collection.mutable
import scala.util.Random
import spinal.lib.Fragment
import spinal.lib.bus.pcie.Tlp
// 用户应用必须根据集成块中配置的最大有效载荷大小来限制单一请求内传输的有效载荷大小，
// 并且必须确保有效载荷不超过 4 KB 的界限。对于不超过 2 个 Dword 的存储器写入，first_be 和 last_be 上值为 1 的位可能不连续。
// 另一种特殊情况是对于长度为 0 的存储器写入请求，用户应用必须提供虚拟有效载荷，
// 其中包含 1 个 Dword 且 first_be 和 last_be 都全部设置为 0。对于仅含 1 个 DW 的传输，
// last_be[3:0] 应为 0，且 first_be[3:0] 可指示有效字节。在所有其它情况下，first_be 和 last_be 中值为 1 的位必须连续。

abstract class PcieCompleterMasterAgent(cmd: Stream[Fragment[Tlp]], rsp: Stream[Fragment[Tlp]], clockDomain: ClockDomain) {
  assert(!canWrite || (cmd.payload.fragment.config.useData && cmd.payload.fragment.config.useStrb)
    , "write stream must have paylaod and strb")
  assert(canRead || canWrite)
  
  val tagWidth = 8
  val pageAlignBits = 12
  val dwAlign = pageAlignBits >> 2
  val tagCount = 1 << 8
  val pcieIfDwWidth = cmd.payload.fragment.config.dwCount

  def canRead = true
  def canWrite = true

  var allowGen = false

  var writeCmdCounter = 0
  var readCmdCounter = 0
  var rspCounter = 0

  val cmdQueue = mutable.Queue[() => Unit]()
  val rspQueue = Array.fill(tagCount)(mutable.Queue[() => Unit]())

  def pending = (0 until (1 << tagWidth)).exists(x => rspQueue.isEmpty)
  def availTag = (0 until (1 << tagWidth)).find(x => rspQueue.isEmpty)
  
  def mappingAllocate(mapping : SizeMapping) : Boolean
  def mappingFree(mapping : SizeMapping) : Unit

  def onCmdWrite(address : BigInt, data : Byte) : Unit = {}

  case class CompleterRequest(dwAddr: BigInt, dwCount: Int, firstBe: Int, lastBe: Int)

  def applyRead(cr: CompleterRequest): Boolean = {
    assert(Util.notCrossAlignment(cr.dwAddr, cr.dwCount, 1<<dwAlign))
    if(!mappingAllocate(SizeMapping(cr.dwAddr << 2, cr.dwCount << 2))) return false

    val tag = availTag.getOrElse(SpinalError("no enough tag"))
    val hdr =MemCmdHeader.createReadSimple(cr.dwAddr, cr.dwCount, cr.firstBe, cr.lastBe, tag)
    
    cmdQueue.enqueue{() => 
      readCmdCounter = readCmdCounter+1
      cmd.payload.fragment.config.withBarFunc generate {
        cmd.payload.fragment.bar_id #= 0
        cmd.payload.fragment.func_num #= 0
      }
      cmd.payload.fragment.config.useError generate {
        cmd.payload.fragment.error #= 0
      }
      cmd.payload.fragment.config.useSeq generate {
        cmd.payload.fragment.seq #= 0
      }

      cmd.payload.fragment.data.randomize()
      cmd.payload.fragment.hdr #= hdr.buildBits()
      cmd.payload.fragment.strb #= 0
      cmd.payload.last #= true
    }

    val realDwCount = if(cr.dwCount == 0) 1 << dwAlign else cr.dwCount
    val trans = (0 until realDwCount).grouped(pcieIfDwWidth).toArray
    for(dwCur <- trans) {
      rspQueue(tag).enqueue {()=>
        if(dwCur == trans.last) {
          mappingFree(SizeMapping(cr.dwAddr << 2, cr.dwCount << 2))
          rspCounter = rspCounter + 1
        }
      }
    }
    true
  }

  def applyWrite(cr: CompleterRequest): Boolean = {
    assert(Util.notCrossAlignment(cr.dwAddr, cr.dwCount, 1<<dwAlign))
    if(!mappingAllocate(SizeMapping(cr.dwAddr << 2, cr.dwCount << 2))) return false

    val hdr = MemCmdHeader.createWriteSimple(cr.dwAddr, cr.dwCount, cr.firstBe, cr.lastBe)
    val realDwCount = if(cr.dwCount == 0) 1 << dwAlign else cr.dwCount
    val trans = (0 until realDwCount).grouped(pcieIfDwWidth).toArray
    for(dwCur <- trans) {
      cmdQueue.enqueue {()=>
        if(dwCur == trans.last) writeCmdCounter = writeCmdCounter+1
        cmd.payload.fragment.config.withBarFunc generate {
          cmd.payload.fragment.bar_id #= 0
          cmd.payload.fragment.func_num #= 0
        }
        cmd.payload.fragment.config.useError generate {
          cmd.payload.fragment.error #= 0
        }
        cmd.payload.fragment.config.useSeq generate {
          cmd.payload.fragment.seq #= 0
        }

        cmd.payload.fragment.data.randomize()
        cmd.payload.fragment.hdr #= hdr.buildBits()
        cmd.payload.fragment.strb #= dwCur.map(x => 1 << (x % pcieIfDwWidth)).reduce(_|_)
        cmd.payload.last #= (dwCur == trans.last)
      }
    }
    true
  }

  def normalReq: CompleterRequest = {
    val dwAddr = (BigInt(Random.nextInt(1 << 31)) * BigInt(Random.nextInt(1 << 31)))
    CompleterRequest(
      dwAddr = dwAddr,
      // do not cross 4k
      dwCount = Random.nextInt((1<<dwAlign)-1 - (dwAddr & ((1<<dwAlign)-1)).toInt),
      firstBe = List(8, 12, 14, 15).randomPick(),
      lastBe = List(1, 3, 7, 15).randomPick()
    )
  }

  def zeroLenReq: CompleterRequest = {
    CompleterRequest(
      dwAddr = BigInt(Random.nextInt(1 << 31)) * BigInt(Random.nextInt(1 << 31)),
      dwCount = 0,
      firstBe = 0,
      lastBe = 0
    )
  }

  def regReq: CompleterRequest = {
    
    CompleterRequest(
      dwAddr = BigInt(Random.nextInt(1 << 31)) * BigInt(Random.nextInt(1 << 31)),
      // // !may cause bug
      // dwCount = List(1, 2).randomPick(),
      dwCount = List(1).randomPick(),
      firstBe = Random.nextInt((1 << 4)-1),
      lastBe = Random.nextInt((1 << 4)-1)
    )
  }

  def genCmd() {
    if(!allowGen) return
    var cr = normalReq
    if(Random.nextBoolean()) {
      while(!applyRead(cr)) {cr = normalReq}
    } else {
      while(!applyWrite(cr)) {cr = normalReq}
    }
  }

  val cmdDriver = StreamDriver(cmd, clockDomain){ _ =>
    if(cmdQueue.isEmpty) genCmd()
    if(cmdQueue.nonEmpty) { cmdQueue.dequeue().apply(); true } else false
  }

  val rspDriver = StreamReadyRandomizer(rsp, clockDomain)

  var first = true
  var tag = 0
  val rspMonitor = StreamMonitor(rsp, clockDomain){_ =>
    if(first) {
      val hdr = CplHeader.parse(rsp.hdr.toBigInt)
      tag = hdr.tag
      first = false
    }
    if(rsp.last.toBoolean) {
      first = true
    }
    rspQueue(tag).dequeue().apply()
    if(first) {
      assert(rspQueue(tag).isEmpty)
    }
  }

  
  def reset(){
    first = true
    readCmdCounter = 0
    writeCmdCounter = 0
    rspCounter = 0
    cmdQueue.clear()
    rspQueue.foreach(_.clear())
    cmdDriver.reset()
  }

}

abstract class PcieCompleterMonitor(cmd: Stream[Fragment[Tlp]], rsp: Stream[Fragment[Tlp]], clockDomain: ClockDomain) {
  val tagWidth = 8
  val pageAlignBits = 12
  val dwAlign = pageAlignBits >> 2
  val tagCount = 1 << 8
  val pcieIfDwWidth = cmd.payload.fragment.config.dwCount

  def onWriteStart(address: BigInt, id: Int, size: Int, len: Int, burst: Int): Unit = {}
  def onWriteByteAlways(address: BigInt, data: Byte, strobe: Boolean, id: Int): Unit = {}
  def onWriteByte(address : BigInt, data : Byte, id: Int) : Unit = {}

  def onReadStart(address: BigInt, id: Int, size: Int, len: Int, burst: Int): Unit = {}
  def onReadByteAlways(address: BigInt, data: Byte, id: Int): Unit = {}
  def onReadByte(address : BigInt, data : Byte, id : Int) : Unit = {}
  def onResponse(address: BigInt, id: Int, last: Boolean, resp: Byte): Unit = {}

  case class ReadRequestTransaction(hdr: BigInt)
  case class WriteRequestTransaction(hdr: BigInt, data: Array[Byte])
  case class CplDRequestTransaction(hdr: BigInt, data: Array[Byte])
  val rspQueue = Array.fill(tagCount)(mutable.Queue[()=>Unit]())

  val cmdMonitor = StreamMonitor(cmd, clockDomain) {_=>
    
  }

  val rspMonitor = StreamMonitor(rsp, clockDomain) {_=>

  }
  
  def reset() {
    rspQueue.foreach(_.clear())
    cmdMonitor.reset()
    rspMonitor.reset()
  }


}


// abstract class PcieRequesterReadOnlyMonitor(cmd: Stream[Fragment[Tlp]], rsp: Stream[Fragment[Tlp]], clockDomain: ClockDomain) {
//   // todo
// }

// abstract class PcieRequesterWriteOnlyMonitor(cmd: Stream[Fragment[Tlp]], rsp: Stream[Fragment[Tlp]], clockDomain: ClockDomain) {
//   // todo
// }
