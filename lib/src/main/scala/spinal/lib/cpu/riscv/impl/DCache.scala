package spinal.lib.cpu.riscv.impl

import java.text.AttributedString


import spinal.core._
import spinal.lib._
import spinal.lib.bus.avalon.{AvalonMMBus, AvalonMMConfig}


case class DataCacheConfig( cacheSize : Int,
                            bytePerLine : Int,
                            wayCount : Int,
                            addressWidth : Int,
                            cpuDataWidth : Int,
                            memDataWidth : Int){
  def burstSize = bytePerLine*8/memDataWidth
  def getAvalonConfig() = AvalonMMConfig.bursted(
      addressWidth = addressWidth,
      dataWidth = memDataWidth,
    burstCountWidth = log2Up(burstSize + 1)).getReadOnlyConfig.copy(
      constantBurstBehavior = true,
      burstOnBurstBoundariesOnly = true
    )
  val burstLength = bytePerLine/(memDataWidth/8)
}


case class DataCacheCpuCmd(implicit p : DataCacheConfig) extends Bundle{
  val wr = Bool
  val address = UInt(p.addressWidth bit)
  val data = Bits(p.cpuDataWidth bit)
  val mask = Bits(p.cpuDataWidth/8 bit)
  val bypass = Bool
}
case class DataCacheCpuRsp(implicit p : DataCacheConfig) extends Bundle{
  val data = Bits(p.cpuDataWidth bit)
}

case class DataCacheCpuBus(implicit p : DataCacheConfig) extends Bundle with IMasterSlave{
  val cmd = Stream (DataCacheCpuCmd())
  val rsp = Flow (DataCacheCpuRsp())

  override def asMaster(): this.type = {
    master(cmd)
    slave(rsp)
    this
  }

  override def asSlave(): this.type = asMaster.flip()
}


case class DataCacheMemCmd(implicit p : DataCacheConfig) extends Bundle{
  val wr = Bool
  val address = UInt(p.addressWidth bit)
  val data = Bits(p.memDataWidth bits)
  val mask = Bits(p.memDataWidth/8 bits)
  val length = UInt(log2Up(p.burstLength+1) bit)
}
case class DataCacheMemRsp(implicit p : DataCacheConfig) extends Bundle{
  val data = Bits(p.memDataWidth bit)
}

case class DataCacheMemBus(implicit p : DataCacheConfig) extends Bundle with IMasterSlave{
  val cmd = Stream (DataCacheMemCmd())
  val rsp = Flow (DataCacheMemRsp())

  override def asMaster(): this.type = {
    master(cmd)
    slave(rsp)
    this
  }

  override def asSlave(): this.type = asMaster.flip()

 /* def toAvalon(): AvalonMMBus = {
    val avalonConfig = p.getAvalonConfig()
    val mm = AvalonMMBus(avalonConfig)
    mm.read := cmd.valid
    mm.burstCount := U(p.burstSize)
    mm.address := cmd.address
    cmd.ready := mm.waitRequestn
    rsp.valid := mm.readDataValid
    rsp.data := mm.readData
    mm
  }*/
}

case class DataCacheFlushBus() extends Bundle with IMasterSlave{
  val cmd = Event
  val rsp = Bool

  override def asMaster(): DataCacheFlushBus.this.type = {
    master(cmd)
    in(rsp)
    this
  }
}

class DataCache(implicit p : DataCacheConfig) extends Component{
  import p._
  assert(wayCount == 1)
  assert(cpuDataWidth == memDataWidth)
  val io = new Bundle{
    val flush = slave(DataCacheFlushBus())
    val cpu = slave(DataCacheCpuBus())
    val mem = master(DataCacheMemBus())
  }
  val haltCpu = False
  val lineWidth = bytePerLine*8
  val lineCount = cacheSize/bytePerLine
  val wordWidth = Math.max(memDataWidth,cpuDataWidth)
  val wordWidthLog2 = log2Up(wordWidth)
  val wordPerLine = lineWidth/wordWidth
  val bytePerWord = wordWidth/8
  val wayLineCount = lineCount/wayCount
  val wayLineLog2 = log2Up(wayLineCount)
  val wayWordCount = wayLineCount * wordPerLine

  val tagRange = addressWidth-1 downto log2Up(wayLineCount*bytePerLine)
  val lineRange = tagRange.low-1 downto log2Up(bytePerLine)
  val wordRange = log2Up(bytePerLine)-1 downto log2Up(bytePerWord)


  class LineInfo() extends Bundle{
    val used = Bool
    val dirty = Bool
    val address = UInt(tagRange.length bit)
  }

  val ways = Vec(new Bundle{
    val tags = Mem(new LineInfo(),wayLineCount)
    val tagsWrite = tags.writePort

    val data = Mem(Bits(wordWidth bit),wayWordCount) //TODO write mask
    val dataWrite = data.writePort
    val dataRead = data.readSyncPort
    val dataReadRsp = RegNext(dataRead.rsp)
  },wayCount)


  val manager = new Area {
    val request = io.cpu.cmd.haltWhen(haltCpu).m2sPipe()
    request.ready := True

    val waysHitValid = False
    val waysHitOneHot = Bits(wayCount bits)
    val waysHitId = OHToUInt(waysHitOneHot)
    val waysHitDirty = Bool.assignDontCare()
    val waysHitWord = Bits(wordWidth bit).assignDontCare()

    val doWaysRead = !request.isStall
    val waysRead = Vec(for ((way,id) <- ways.zipWithIndex) yield new Bundle { //Trololol
      val readAddress = Mux(doWaysRead, request.address, io.cpu.cmd.address)
      val tag = way.tags.readSync(readAddress,doWaysRead) //TODO write first
      waysHitOneHot(id) := tag.used && tag.address === request.address(tagRange)
      way.dataRead.cmd.valid := doWaysRead
      way.dataRead.cmd.payload := readAddress(lineRange.high downto wordRange.low)
      when(waysHitOneHot(id)) {
        waysHitValid := True
        waysHitDirty := tag.dirty
        waysHitWord := way.dataRead.rsp
      }
    })


    val realocatedWayId = U(0)
    val realocatedWay = waysRead(realocatedWayId)

    val cpuRspIn = Stream(wrap(new Bundle{
      val fromBypass = Bool
      val wayId = UInt(log2Up(wayCount) bits)
    }))

    cpuRspIn.valid := False
    cpuRspIn.fromBypass := False
    cpuRspIn.wayId := waysHitId

    //Loader interface
    val loaderValid = False
    val loaderReady = False
    val loadingDone = RegNext(loaderValid && loaderReady) init(False)

    when(request.valid) {
      when(request.bypass){
        io.mem.cmd.valid := !(!request.wr && !cpuRspIn.ready)
        io.mem.cmd.wr := request.wr
        io.mem.cmd.address := request.address
        io.mem.cmd.mask := request.mask
        io.mem.cmd.length := 1

        cpuRspIn.valid := request.wr && io.mem.cmd.fire
        cpuRspIn.fromBypass := True

        request.ready := io.mem.cmd.fire
      } otherwise {
        when(waysHitValid){
          when(request.wr){
            for ((way,id) <- ways.zipWithIndex){
              way.dataWrite.valid := waysHitOneHot(id)
              way.dataWrite.address := request.address(lineRange.high downto wordRange.low)
              way.dataWrite.data := request.data
              
              way.tagsWrite.valid := waysHitOneHot(id)
              way.tagsWrite.address := request.address(lineRange.high downto wordRange.low)
              way.tagsWrite.data.used := True
              way.tagsWrite.data.dirty := True
              way.tagsWrite.data.address := waysRead(id).tag.address
            }
          }otherwise{
            cpuRspIn.valid := True
            request.ready := cpuRspIn.ready
          }
        } otherwise{
          loaderValid := !loadingDone
          doWaysRead := loadingDone
          request.ready := False
          when(realocatedWay.tag.used && realocatedWay.tag.dirty){

          }
        }
      }
    }

    val cpuRsp = cpuRspIn.m2sPipe()
    val cpuRspIsWaitingMemRsp = cpuRsp.valid && io.mem.rsp.valid
    io.cpu.rsp.valid := cpuRsp.fire
    io.cpu.rsp.data := Mux(cpuRsp.fromBypass,io.mem.rsp.data,ways(cpuRsp.wayId).dataReadRsp)
    cpuRsp.ready := !(cpuRsp.fromBypass && !io.mem.rsp.valid)
  }

  val loader = new Area{
    val valid = RegNext(manager.loaderValid)
    val wayId = RegNext(manager.realocatedWayId)
    val way = ways(wayId)
    val baseAddress =  manager.request.address

    val memCmdSent = RegInit(False)
    when(valid && !memCmdSent) {
      io.mem.cmd.valid := True
      io.mem.cmd.wr := False
      io.mem.cmd.address := baseAddress(tagRange.high downto lineRange.low) @@ U(0,lineRange.low bit)
      io.mem.cmd.length := p.burstLength
    }

    when(io.mem.cmd.fire){
      memCmdSent := True
    }

    val counter = Counter(p.bytePerLine / (p.memDataWidth/8))
    when(io.mem.rsp.valid && !manager.cpuRspIsWaitingMemRsp){ //TODO all way assignement opt
      way.dataWrite.valid := True
      way.dataWrite.address := baseAddress(lineRange) @@ counter
      way.dataWrite.data := io.mem.rsp.data
      counter.increment()
    }

    when(counter.willOverflow){
      memCmdSent := False
      valid := False
      way.tagsWrite.valid := True
      way.tagsWrite.address := baseAddress(lineRange)
      way.tagsWrite.data.used := True
      way.tagsWrite.data.dirty := False
      way.tagsWrite.data.address := baseAddress(tagRange)
      manager.loaderReady := True
    }
  }

  io.flush.cmd.ready := False  // TODO
}

object DataCacheMain{
  class TopLevel extends Component{
    implicit val p = DataCacheConfig(
      cacheSize =4096,
      bytePerLine =32,
      wayCount = 1,
      addressWidth = 32,
      cpuDataWidth = 32,
      memDataWidth = 32)
    val io = new Bundle{
      val cpu = slave(DataCacheCpuBus())
      val mem = master(DataCacheMemBus())
    }
    val cache = new DataCache()(p)

    cache.io.cpu.cmd <-< io.cpu.cmd
    cache.io.mem.cmd >-> io.mem.cmd
    cache.io.mem.rsp <-< io.mem.rsp
    cache.io.cpu.rsp >-> io.cpu.rsp
//    when(cache.io.cpu.rsp.valid){
//      cache.io.cpu.cmd.valid := RegNext(cache.io.cpu.cmd.valid)
//      cache.io.cpu.cmd.address := RegNext(cache.io.cpu.cmd.address)
//    }
  }
  def main(args: Array[String]) {

    SpinalVhdl(new TopLevel)
  }
}