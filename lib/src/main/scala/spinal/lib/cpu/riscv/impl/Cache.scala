package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.avalon.mm.{AvalonMMBus, AvalonMMConfig}


case class InstructionCacheParameters( cacheSize : Int,
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


case class InstructionCacheCpuCmd(implicit p : InstructionCacheParameters) extends Bundle{
  val address = UInt(p.addressWidth bit)
}
case class InstructionCacheCpuRsp(implicit p : InstructionCacheParameters) extends Bundle{
  val data = Bits(32 bit)
}

case class InstructionCacheCpuBus(implicit p : InstructionCacheParameters) extends Bundle with IMasterSlave{
  val cmd = Flow (InstructionCacheCpuCmd())
  val rsp = Flow (InstructionCacheCpuRsp())

  override def asMaster(): this.type = {
    master(cmd)
    slave(rsp)
    this
  }

  override def asSlave(): this.type = asMaster.flip()
}


case class InstructionCacheMemCmd(implicit p : InstructionCacheParameters) extends Bundle{
  val address = UInt(p.addressWidth bit)
}
case class InstructionCacheMemRsp(implicit p : InstructionCacheParameters) extends Bundle{
  val data = Bits(32 bit)
}

case class InstructionCacheMemBus(implicit p : InstructionCacheParameters) extends Bundle with IMasterSlave{
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

class InstructionCache(implicit p : InstructionCacheParameters) extends Component{
  import p._
  assert(wayCount == 1)
  assert(cpuDataWidth == memDataWidth)
  val io = new Bundle{
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

  val mem_cmd = Stream (InstructionCacheMemCmd())
  if(stableMemCmd)
    io.mem.cmd <-< mem_cmd
  else
    io.mem.cmd << mem_cmd

  class LineInfo() extends Bundle{
    val valid = Bool
    val address = UInt(tagRange.length bit)
  }

  val ways = Array.fill(wayCount)(new Area{
    val tags = Mem(new LineInfo(),wayLineCount)
    val datas = Mem(Bits(wordWidth bit),wayWordCount)
  })

  val loader = new Area{
    val requestIn = Stream(wrap(new Bundle{
      val addr = UInt(addressWidth bit)
    }))


    if(wrappedMemAccess)
      mem_cmd.address := requestIn.addr(tagRange.high downto wordRange.low) @@ U(0,wordRange.low bit)
    else
      mem_cmd.address := requestIn.addr(tagRange.high downto lineRange.low) @@ U(0,lineRange.low bit)


    val initCounter = Reg(UInt(log2Up(wayLineCount) + 1 bit)) init(0)
    when(!initCounter.msb){
      haltCpu := True
      initCounter := initCounter + 1
    }
    when(RegNext(!initCounter.msb)){
      haltCpu := True
    }

    val lineInfoWrite = new LineInfo()
    lineInfoWrite.valid := initCounter.msb
    lineInfoWrite.address := requestIn.addr(tagRange)
    when(requestIn.fire || !initCounter.msb){
      val tagsAddress = Mux(initCounter.msb,requestIn.addr(lineRange),initCounter(initCounter.high-1 downto 0))
      ways(0).tags(tagsAddress) := lineInfoWrite  //TODO
    }


    val request = requestIn.haltWhen(!mem_cmd.ready).m2sPipe()
    mem_cmd.valid := requestIn.valid && !request.isStall
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
      wordIndex := mem_cmd.address(wordRange)
      loadedWords := 0
      loadedWordsReadable := 0
      readyDelay := 0
    }
  }

  val task = new Area{
    val request = RegNext(io.cpu.cmd)
    val waysHitValid = False
    val waysHitWord = Bits(wordWidth bit)
    waysHitWord.assignDontCare()

    val waysRead = for(way <- ways) yield new Area{
      val tag = way.tags.readSync(io.cpu.cmd.address(lineRange))
      val data = way.datas.readSync(io.cpu.cmd.address(lineRange.high downto wordRange.low))
      when(!haltCpu) {
        when(tag.valid && tag.address === request.address(tagRange)) {
          waysHitValid := True
          waysHitWord := data
        }
      }
    }

    val loaderHitValid = loader.request.valid && loader.request.addr(tagRange) === request.address(tagRange)
    val loaderHitReady = loader.loadedWordsReadable(request.address(wordRange))


    io.cpu.rsp.valid := False
    io.cpu.rsp.data := waysHitWord //TODO
    loader.requestIn.valid := False
    loader.requestIn.addr := request.address
    when(request.valid) {
      when(waysHitValid) {
        when(!(loaderHitValid && !loaderHitReady)) {
          io.cpu.rsp.valid := True
        }
      } otherwise{
        loader.requestIn.valid := !haltCpu
      }
    }
  }
}

object InstructionCacheMain{
  class TopLevel extends Component{
    implicit val p = InstructionCacheParameters(
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
  }
  def main(args: Array[String]) {

    SpinalVhdl(new TopLevel)
  }
}