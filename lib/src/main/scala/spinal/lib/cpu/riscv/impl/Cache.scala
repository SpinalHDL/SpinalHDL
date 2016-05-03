package spinal.lib.cpu.riscv.impl

import java.text.AttributedString


import spinal.core._
import spinal.lib._
import spinal.lib.bus.avalon.{AvalonMMBus, AvalonMMConfig}


case class InstructionCacheConfig( cacheSize : Int,
                                       bytePerLine : Int,
                                       wayCount : Int,
                                       wrappedMemAccess : Boolean,
                                       addressWidth : Int,
                                       cpuDataWidth : Int,
                                       memDataWidth : Int,
                                       stableMemCmd : Boolean = true){
  def burstSize = bytePerLine*8/memDataWidth
  def getAvalonConfig() = AvalonMMConfig.bursted(
    addressWidth = addressWidth,
    dataWidth = memDataWidth,
    burstCountWidth = log2Up(burstSize + 1)).getReadOnlyConfig.copy(
    linewrapBursts = wrappedMemAccess,
    constantBurstBehavior = true
    )
}


case class InstructionCacheCpuCmd(implicit p : InstructionCacheConfig) extends Bundle{
  val address = UInt(p.addressWidth bit)
}
case class InstructionCacheCpuRsp(implicit p : InstructionCacheConfig) extends Bundle{
  val address = UInt(p.addressWidth bit)
  val data = Bits(32 bit)
}

case class InstructionCacheCpuBus(implicit p : InstructionCacheConfig) extends Bundle with IMasterSlave{
  val cmd = Stream (InstructionCacheCpuCmd())
  val rsp = Stream (InstructionCacheCpuRsp())

  override def asMaster(): this.type = {
    master(cmd)
    slave(rsp)
    this
  }

  override def asSlave(): this.type = asMaster.flip()
}


case class InstructionCacheMemCmd(implicit p : InstructionCacheConfig) extends Bundle{
  val address = UInt(p.addressWidth bit)
}
case class InstructionCacheMemRsp(implicit p : InstructionCacheConfig) extends Bundle{
  val data = Bits(32 bit)
}

case class InstructionCacheMemBus(implicit p : InstructionCacheConfig) extends Bundle with IMasterSlave{
  val cmd = Stream (InstructionCacheMemCmd())
  val rsp = Flow (InstructionCacheMemRsp())

  override def asMaster(): this.type = {
    master(cmd)
    slave(rsp)
    this
  }

  override def asSlave(): this.type = asMaster.flip()

  def toAvalon(): AvalonMMBus = {
    val avalonConfig = p.getAvalonConfig()
    val mm = AvalonMMBus(avalonConfig)
    mm.read := cmd.valid
    mm.burstCount := U(p.burstSize)
    mm.address := cmd.address
    cmd.ready := mm.waitRequestn
    rsp.valid := mm.readDataValid
    rsp.data := mm.readData
    mm
  }
}

case class InstructionCacheFlushBus() extends Bundle with IMasterSlave{
  val cmd = Event
  val rsp = Bool

  override def asMaster(): InstructionCacheFlushBus.this.type = {
    master(cmd)
    in(rsp)
    this
  }
}

class InstructionCache(implicit p : InstructionCacheConfig) extends Component{
  import p._
  assert(wayCount == 1)
  assert(cpuDataWidth == memDataWidth)
  val io = new Bundle{
    val flush = slave(InstructionCacheFlushBus())
    val cpu = slave(InstructionCacheCpuBus())
    val mem = master(InstructionCacheMemBus())
  }
  val haltCpu = False
  val lineWidth = bytePerLine*8
  val lineCount = cacheSize/bytePerLine
  val wordWidth = Math.max(memDataWidth,32)
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
    val valid = Bool
    val address = UInt(tagRange.length bit)
  }

  val ways = Array.fill(wayCount)(new Area{
    val tags = Mem(new LineInfo(),wayLineCount)
    val datas = Mem(Bits(wordWidth bit),wayWordCount)
  })


  val lineLoader = new Area{
    val requestIn = Stream(wrap(new Bundle{
      val addr = UInt(addressWidth bit)
    }))


    if(wrappedMemAccess)
      io.mem.cmd.address := requestIn.addr(tagRange.high downto wordRange.low) @@ U(0,wordRange.low bit)
    else
      io.mem.cmd.address := requestIn.addr(tagRange.high downto lineRange.low) @@ U(0,lineRange.low bit)


    val flushCounter = Reg(UInt(log2Up(wayLineCount) + 1 bit)) init(0)
    when(!flushCounter.msb){
      haltCpu := True
      flushCounter := flushCounter + 1
    }
    when(!RegNext(flushCounter.msb)){
      haltCpu := True
    }
    val flushFromInterface = RegInit(False)
    when(io.flush.cmd.valid){
      haltCpu := True
      when(io.flush.cmd.ready){
        flushCounter := 0
        flushFromInterface := True
      }
    }

    io.flush.rsp := flushCounter.msb.rise && flushFromInterface

    val lineInfoWrite = new LineInfo()
    lineInfoWrite.valid := flushCounter.msb
    lineInfoWrite.address := requestIn.addr(tagRange)
    when(requestIn.fire || !flushCounter.msb){
      val tagsAddress = Mux(flushCounter.msb,requestIn.addr(lineRange),flushCounter(flushCounter.high-1 downto 0))
      ways(0).tags(tagsAddress) := lineInfoWrite  //TODO
    }


    val request = requestIn.haltWhen(!io.mem.cmd.ready).m2sPipe()
    io.mem.cmd.valid := requestIn.valid && !request.isStall
    val wordIndex = Reg(UInt(log2Up(wordPerLine) bit))
    val loadedWordsNext = Bits(wordPerLine bit)
    val loadedWords = RegNext(loadedWordsNext)
    val loadedWordsReadable = RegNext(loadedWords)
    loadedWordsNext := loadedWords
    when(io.mem.rsp.fire){
      wordIndex := wordIndex + 1
      loadedWordsNext(wordIndex) := True
      ways(0).datas(request.addr(lineRange) @@ wordIndex) := io.mem.rsp.data   //TODO
    }

    val readyDelay = Reg(UInt(1 bit))
    when(loadedWordsNext === B(loadedWordsNext.range -> true)){
      readyDelay := readyDelay + 1
    }
    request.ready := readyDelay === 1

    when(requestIn.ready){
      wordIndex := io.mem.cmd.address(wordRange)
      loadedWords := 0
      loadedWordsReadable := 0
      readyDelay := 0
    }
  }

  val task = new Area{
    val request = io.cpu.cmd.haltWhen(haltCpu).m2sPipe()
    request.ready := io.cpu.rsp.fire
    val waysHitValid = False
    val waysHitWord = Bits(wordWidth bit)
    waysHitWord.assignDontCare()

    val waysRead = for(way <- ways) yield new Area{
//      val readAddress = Mux(request.isStall,request.address,io.cpu.cmd.address)
//      val tag = way.tags.readSync(readAddress(lineRange))
//      val data = way.datas.readSync(readAddress(lineRange.high downto wordRange.low))
      val readAddress = request.address
      val tag = way.tags.readAsync(readAddress(lineRange))
      val data = way.datas.readAsync(readAddress(lineRange.high downto wordRange.low))
      way.tags.add(new AttributeString("ramstyle","no_rw_check"))
      way.datas.add(new AttributeString("ramstyle","no_rw_check"))
      when(tag.valid && tag.address === request.address(tagRange)) {
        waysHitValid := True
        waysHitWord := data
      }
    }

    val loaderHitValid = lineLoader.request.valid && lineLoader.request.addr(tagRange) === request.address(tagRange)
    val loaderHitReady = lineLoader.loadedWordsReadable(request.address(wordRange))


    io.cpu.rsp.valid := request.valid && waysHitValid && !(loaderHitValid && !loaderHitReady)
    io.cpu.rsp.data := waysHitWord //TODO
    io.cpu.rsp.address := request.address
    lineLoader.requestIn.valid := request.valid && ! waysHitValid
    lineLoader.requestIn.addr := request.address
  }

  io.flush.cmd.ready := !(lineLoader.request.valid || task.request.valid)
}

object InstructionCacheMain{
  class TopLevel extends Component{
    implicit val p = InstructionCacheConfig(
      cacheSize =4096,
      bytePerLine =32,
      wayCount = 1,
      wrappedMemAccess = true,
      addressWidth = 32,
      cpuDataWidth = 32,
      memDataWidth = 32)
    val io = new Bundle{
      val cpu = slave(InstructionCacheCpuBus())
      val mem = master(InstructionCacheMemBus())
    }
    val cache = new InstructionCache()(p)

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