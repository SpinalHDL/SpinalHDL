package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._


case class AxiReadOnlyCacheParameters( cacheSize : Int,
                                       bytePerLine : Int,
                                       wayCount : Int,
                                       cmdParameters : AxiReadConfig,
                                       memDataWidth : Int){
  val memParameters = AxiReadConfig(
    addressWidth = cmdParameters.addressWidth,
    dataWidth = memDataWidth,
    useBurst = true,
    useLen = true,
    lenWidth = log2Up(bytePerLine/(memDataWidth/8))
  )
}

class AxiReadOnlyCache(p : AxiReadOnlyCacheParameters) extends Component{
  import p._
  val io = new Bundle{
    val cpu = slave(AxiReadOnly(cmdParameters))
    val mem = master(AxiReadOnly(memParameters))
  }
  val haltCpu = False
  val lineWidth = bytePerLine*8
  val lineCount = cacheSize/bytePerLine
  val wordWidth = Math.max(memDataWidth,cmdParameters.dataWidth)
  val wordWidthLog2 = log2Up(wordWidth)
  val wordPerLine = lineWidth/wordWidth
  val bytePerWord = wordWidth/8
  val wayLineCount = lineCount/wayCount
  val wayLineLog2 = log2Up(wayLineCount)
  val wayWordCount = wayLineCount * wordPerLine

  val tagRange = cmdParameters.addressWidth-1 downto log2Up(wayLineCount*bytePerLine)
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

  val loader = new Area{
    val requestIn = Stream(wrap(new Bundle{
      val addr = UInt(cmdParameters.addressWidth bit)
    }))

    io.mem.readCmd.valid := requestIn.fire
    io.mem.readCmd.len := wordPerLine-1
    io.mem.readCmd.addr := requestIn.addr(tagRange.high downto wordRange.low) @@ U(0,wordRange.low bit)
    io.mem.readCmd.burst := AxiBurst.WRAP
    io.mem.readCmd.prot := 0

    io.mem.readRsp.ready := True


    val initCounter = Reg(UInt(log2Up(wayLineCount) + 1 bit)) init(0)
    when(!initCounter.msb){
      haltCpu := True
      initCounter := initCounter + 1
    }

    val lineInfoWrite = new LineInfo()
    lineInfoWrite.valid := initCounter.msb
    lineInfoWrite.address := requestIn.addr(tagRange)
    when(requestIn.fire || !initCounter.msb){
      val tagsAddress = Mux(initCounter.msb,requestIn.addr(lineRange),initCounter(initCounter.high-1 downto 0))
      ways(0).tags(tagsAddress) := lineInfoWrite  //TODO
    }


    val request = requestIn.haltWhen(!io.mem.readCmd.ready).m2sPipe()
    val wordIndex = Reg(UInt(log2Up(wordPerLine) bit))
    val loadedWordsNext = Bits(wordPerLine bit)
    val loadedWords = RegNext(loadedWordsNext)
    val loadedWordsReadable = RegNext(loadedWords)
    loadedWordsNext := loadedWords
    when(io.mem.readRsp.fire){
      wordIndex := wordIndex + 1
      loadedWordsNext(wordIndex) := True
      ways(0).datas(request.addr(lineRange) @@ wordIndex) := io.mem.readRsp.data   //TODO
    }

    val readyDelay = Reg(UInt(1 bit))
    when(loadedWordsNext === B(loadedWordsNext.range -> true)){
      readyDelay := readyDelay + 1
    }
    request.ready := readyDelay === 1

    when(requestIn.ready){
      wordIndex := io.mem.readCmd.addr(wordRange)
      loadedWords := 0
      loadedWordsReadable := 0
      readyDelay := 0
    }
  }

  val task = new Area{
    val request = io.cpu.readCmd.haltWhen(haltCpu).m2sPipe()
    val waysHitValid = False
    val waysHitWord = Bits(wordWidth bit)
    waysHitWord.assignDontCare()

    val memoryAddress = Mux(request.isStall,request.addr,io.cpu.readCmd.addr)
    val waysRead = for(way <- ways) yield new Area{
      val tag = way.tags.readSync(memoryAddress(lineRange))
      val data = way.datas.readSync(memoryAddress(lineRange.high downto wordRange.low))
      when(tag.valid && tag.address === request.addr(tagRange)){
        waysHitValid := True
        waysHitWord := data
      }
    }

    val loaderHitValid = loader.request.valid && loader.request.addr(tagRange) === request.addr(tagRange)
    val loaderHitReady = loader.loadedWordsReadable(request.addr(wordRange))

    request.ready := False
    io.cpu.readRsp.valid := False
    io.cpu.readRsp.data := waysHitWord //TODO
    loader.requestIn.valid := False
    loader.requestIn.addr := request.addr
    when(request.valid) {
      when(waysHitValid) {
        when(!(loaderHitValid && !loaderHitReady)) {
          request.ready := io.cpu.readRsp.ready
          io.cpu.readRsp.valid := True
        }
      } otherwise{
        loader.requestIn.valid := True
      }
    }
  }


}

object AxiReadOnlyCacheMain{
  def main(args: Array[String]) {
    val p = AxiReadOnlyCacheParameters(
      cacheSize =4096,
      bytePerLine =32,
      wayCount = 1,
      cmdParameters = AxiReadConfig(32,32),
      memDataWidth = 32)
    SpinalVhdl(new AxiReadOnlyCache(p),_.setLibrary("lib_AxiReadOnlyCache"))
  }
}