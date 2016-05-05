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
  val keepMemUpdated = Bool //TODO
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
  val memTransactionPerLine = p.bytePerLine / (p.memDataWidth/8)

  val tagRange = addressWidth-1 downto log2Up(wayLineCount*bytePerLine)
  val lineRange = tagRange.low-1 downto log2Up(bytePerLine)
  val wordRange = log2Up(bytePerLine)-1 downto log2Up(bytePerWord)


  class LineInfo() extends Bundle{
    val used = Bool
    val dirty = Bool
    val address = UInt(tagRange.length bit)
  }


  val tagsWriteCmd = Flow(new Bundle{
    val way = UInt(log2Up(wayCount) bits)
    val address = UInt(log2Up(wayLineCount) bits)
    val data = new LineInfo()
  })
  val tagsWriteLastCmd = RegNext(tagsWriteCmd)

  val dataReadCmd =  UInt(log2Up(wayWordCount) bits)
  val dataWriteCmd = Flow(new Bundle{
    val way = UInt(log2Up(wayCount) bits)
    val address = UInt(log2Up(wayWordCount) bits)
    val data = Bits(wordWidth bits)
  })

  tagsWriteCmd.valid := False
  tagsWriteCmd.payload.assignDontCare()
  dataWriteCmd.valid := False
  dataWriteCmd.payload.assignDontCare()
  io.mem.cmd.valid := False
  io.mem.cmd.payload.assignDontCare()




  val ways = Array.tabulate(wayCount)(id => new Area{
    val tags = Mem(new LineInfo(),wayLineCount)
    val data = Mem(Bits(wordWidth bit),wayWordCount) //TODO write mask

    when(tagsWriteCmd.valid && tagsWriteCmd.way === id){
      tags(tagsWriteCmd.address) := tagsWriteCmd.data
    }
    when(dataWriteCmd.valid && dataWriteCmd.way === id){
      data(dataWriteCmd.address) := dataWriteCmd.data
    }
    val dataReadRsp = data.readSync(dataReadCmd)
  })

  val dataReadedValue = Vec(id => RegNext(ways(id).dataReadRsp),ways.length)

  
  

  val victim = new Area{
    val requestIn = Stream(cloneable(new Bundle{
      val way = UInt(log2Up(wayCount) bits)
      val address = UInt(p.addressWidth bits)
    }))
    requestIn.valid := False
    requestIn.payload.assignDontCare()

    val request = requestIn.stage()
    request.ready := False

    val buffer = Mem(Bits(p.memDataWidth bits),memTransactionPerLine).add(new AttributeString("ramstyle","M4K"))

    //Send line read commands to fill the buffer
    val readLineCmdCounter = Reg(UInt(log2Up(memTransactionPerLine + 1) bits)) init(0)
    val dataReadCmdOccure = False
    when(request.valid && !readLineCmdCounter.msb){
      readLineCmdCounter := readLineCmdCounter + 1
      //dataReadCmd := request.address(lineRange.high downto wordRange.low)   Done in the manager
      dataReadCmdOccure := True
    }

    //Fill the buffer with line read responses
    val readLineRspCounter = Reg(UInt(log2Up(memTransactionPerLine + 1) bits)) init(0)
    when(readLineCmdCounter >= 2 && !readLineRspCounter.msb && Delay(dataReadCmdOccure,2)){
      buffer(readLineRspCounter.resized) := dataReadedValue(request.way)
      readLineRspCounter := readLineRspCounter + 1
    }

    //Send buffer read commands
    val bufferReadCounter = Reg(UInt(log2Up(memTransactionPerLine + 1) bits)) init(0)
    val bufferReadStream = Stream(buffer.addressType)
    bufferReadStream.valid := readLineRspCounter > bufferReadCounter
    bufferReadStream.payload := bufferReadCounter.resized
    when(bufferReadStream.fire){
      bufferReadCounter := bufferReadCounter + 1
    }
    val bufferReaded = buffer.streamReadSync(bufferReadStream).stage
    bufferReaded.ready := False

    //Send memory writes from bufffer read responses
    val bufferReadedCounter = Reg(UInt(log2Up(memTransactionPerLine) bits)) init(0)
    val memCmdAlreadyUsed = False
    when(bufferReaded.valid) {
      io.mem.cmd.valid := True
      io.mem.cmd.wr := True
      io.mem.cmd.address := request.address(tagRange.high downto lineRange.low) @@ U(0,lineRange.low bit)
      io.mem.cmd.length := p.burstLength
      io.mem.cmd.data := bufferReaded.payload

      when(!memCmdAlreadyUsed && io.mem.cmd.ready){
        bufferReaded.ready := True
        bufferReadedCounter := bufferReadedCounter + 1
        when(bufferReadedCounter === bufferReadedCounter.maxValue){
          request.ready := True
        }
      }
    }


    val counter = Counter(memTransactionPerLine)
    when(request.ready){
      readLineCmdCounter.msb := False
      readLineRspCounter.msb := False
    }
  }

  val manager = new Area {
    val request = io.cpu.cmd.haltWhen(haltCpu).stage()
    request.ready := True

    val waysHitValid = False
    val waysHitOneHot = Bits(wayCount bits)
    val waysHitId = OHToUInt(waysHitOneHot)
    val waysHitInfo = new LineInfo().assignDontCare()

   // val doWaysRead = !request.isStall
    val waysRead = for ((way,id) <- ways.zipWithIndex) yield new Area {
      val readAddress = Mux(/*!doWaysRead*/request.isStall, request.address, io.cpu.cmd.address)
      val tag = way.tags.readSync(readAddress(lineRange)/*,doWaysRead*/)
      //Write first
      when(tagsWriteLastCmd.valid && tagsWriteLastCmd.way === id && tagsWriteLastCmd.address === RegNext(readAddress(lineRange))){
        tag := tagsWriteLastCmd.data
      }
      waysHitOneHot(id) := tag.used && tag.address === request.address(tagRange)
      dataReadCmd := readAddress(lineRange.high downto wordRange.low)
      when(victim.dataReadCmdOccure){
        dataReadCmd := victim.request.address(lineRange) @@ victim.readLineCmdCounter(victim.readLineCmdCounter.high-1 downto 0)
      }
      when(waysHitOneHot(id)) {
        waysHitValid := True
        waysHitInfo := tag
      }
    }


    val realocatedWayId = U(0)
    val realocatedWayInfo = Vec(waysRead.map(_.tag))(realocatedWayId)

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

    val victimSent = RegNext(victim.requestIn.fire && request.isStall)

    when(request.valid) {
      when(request.bypass){
        when(!victim.request.valid) { //Can't insert mem cmd into a victim write burst
          io.mem.cmd.valid := !(!request.wr && !cpuRspIn.ready)
          io.mem.cmd.wr := request.wr
          io.mem.cmd.address := request.address
          io.mem.cmd.mask := request.mask
          io.mem.cmd.data := request.data
          io.mem.cmd.length := 1

          cpuRspIn.valid := !request.wr && io.mem.cmd.fire
          cpuRspIn.fromBypass := True

          request.ready := io.mem.cmd.fire
        } otherwise{
          request.ready := False
        }
      } otherwise {
        when(waysHitValid && !loadingDone){ // !loadingDone => don't solve the request directly after loader (data write to read latency)
          when(request.wr){
            dataWriteCmd.valid := True
            dataWriteCmd.way := waysHitId
            dataWriteCmd.address := request.address(lineRange.high downto wordRange.low)
            dataWriteCmd.data := request.data

            tagsWriteCmd.valid := True
            tagsWriteCmd.way := waysHitId
            tagsWriteCmd.address := request.address(lineRange)
            tagsWriteCmd.data.used := True
            tagsWriteCmd.data.dirty := True
            tagsWriteCmd.data.address := waysHitInfo.address
          }otherwise{
            cpuRspIn.valid := True
            request.ready := cpuRspIn.ready
          }
        } otherwise{
          request.ready := False //Exit this state automaticly (tags read port write first logic)
          loaderValid := !loadingDone && !(!victimSent && victim.request.isStall) //Wait previous victim request to be completed
          when(realocatedWayInfo.used && realocatedWayInfo.dirty){
            victim.requestIn.valid := !victimSent
            victim.requestIn.way := realocatedWayId
            victim.requestIn.address := realocatedWayInfo.address @@ request.address(lineRange) @@ U((lineRange.low-1 downto 0) -> false)

           // doWaysRead := loadingDone && victimSent
          } otherwise{
           // doWaysRead := loadingDone
          }
        }
      }
    }


    val cpuRsp = cpuRspIn.m2sPipe()
    val cpuRspIsWaitingMemRsp = cpuRsp.valid && io.mem.rsp.valid
    io.cpu.rsp.valid := cpuRsp.fire
    io.cpu.rsp.data := Mux(cpuRsp.fromBypass,io.mem.rsp.data,dataReadedValue(cpuRsp.wayId))
    cpuRsp.ready := !(cpuRsp.fromBypass && !io.mem.rsp.valid)
  }

  //The whole life of a loading task, the corresponding manager request is present
  val loader = new Area{
    val valid = RegNext(manager.loaderValid)
    val wayId = RegNext(manager.realocatedWayId)
    val baseAddress =  manager.request.address

    val memCmdSent = RegInit(False)
    when(valid && !memCmdSent) {
      io.mem.cmd.valid := True
      io.mem.cmd.wr := False
      io.mem.cmd.address := baseAddress(tagRange.high downto lineRange.low) @@ U(0,lineRange.low bit)
      io.mem.cmd.length := p.burstLength
    }

    when(valid && io.mem.cmd.ready){
      memCmdSent := True
    }

    when(valid && !memCmdSent) {
      victim.memCmdAlreadyUsed := True
    }

    val counter = Counter(memTransactionPerLine)
    when(io.mem.rsp.valid && !manager.cpuRspIsWaitingMemRsp){ //TODO all way assignement opt
      dataWriteCmd.valid := True
      dataWriteCmd.way := wayId
      dataWriteCmd.address := baseAddress(lineRange) @@ counter
      dataWriteCmd.data := io.mem.rsp.data
      counter.increment()
    }

    when(counter.willOverflow){
      memCmdSent := False
      valid := False
      tagsWriteCmd.valid := True
      tagsWriteCmd.way := wayId
      tagsWriteCmd.address := baseAddress(lineRange)
      tagsWriteCmd.data.used := True
      tagsWriteCmd.data.dirty := False
      tagsWriteCmd.data.address := baseAddress(tagRange)
      manager.loaderReady := True
    }
  }

  //Avoid read after write data hazard
  when(io.cpu.cmd.address === manager.request.address && manager.request.valid && io.cpu.cmd.valid){
    haltCpu := True
  }

  io.flush.cmd.ready := False  // TODO
  io.flush.rsp := False
}

object DataCacheMain{
  def main(args: Array[String]) {

    SpinalVhdl({
      implicit val p = DataCacheConfig(
        cacheSize =4096,
        bytePerLine =32,
        wayCount = 1,
        addressWidth = 32,
        cpuDataWidth = 32,
        memDataWidth = 32)
      new WrapWithReg.Wrapper(new DataCache()(p)).setDefinitionName("TopLevel")
    })
    SpinalVhdl({
      implicit val p = DataCacheConfig(
        cacheSize =4096,
        bytePerLine =32,
        wayCount = 1,
        addressWidth = 32,
        cpuDataWidth = 32,
        memDataWidth = 32)
      new DataCache()(p)
    },_.setLibrary("lib_DataCache"))
  }
}