package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.sbl._
import spinal.lib.cpu.riscv.impl.Utils.InstructionCtrl


/*

address
wr
mask
data



address
wr
length
size


data
mask
last
      val write = slave Flow (SblWriteCmd(cpuConfig))
      val cmd = slave Stream (SblReadCmd(cpuConfig))
      val rsp = master Stream (SblReadRet(cpuConfig))
 */
case class AxiReadOnlyCacheParameters( cacheSize : Int,
                                       bytePerLine : Int,
                                       wayCount : Int,
                                       cmdParameters : AxiReadConfig,
                                       memDataWidth : Int){
  val memParameters = AxiReadConfig(
    addressWidth = cmdParameters.addressWidth,
    dataWidth = memDataWidth,
    useBurst = true,
    lenWidth = log2Up(bytePerLine/(memDataWidth/8))
  )
}

class AxiReadOnlyCache(p : AxiReadOnlyCacheParameters) extends Component{
  import p._
  val io = new Bundle{
    val cpu = AxiReadOnly(cmdParameters)
    val mem = AxiReadOnly(memParameters)
  }
  val lineWidth = bytePerLine*8
  val lineCount = cacheSize/bytePerLine
  val wordWidth = Math.max(memDataWidth,cmdParameters.dataWidth)
  val wordWidthLog2 = log2Up(wordWidth)
  val wordPerLine = lineWidth/wordWidth
  val wayLineCount = lineCount/wayCount
  val wayLineLog2 = log2Up(wayLineCount)
  val wayWordCount = wayLineCount * wordPerLine

  val tagRange = cmdParameters.addressWidth-1 downto log2Up(wayLineCount*bytePerLine)
  val lineRange = tagRange.low-1 downto log2Up(bytePerLine)
  val wordRange = tagRange.low-1 downto log2Up(wordWidth/8)

  class LineInfo() extends Bundle{
    val valid = Bool
    val address = UInt(tagRange.length bit)
  }

  val ways = Array.fill(wayCount)(new Area{
    val tags = Mem(new LineInfo(),wayLineCount)
    val datas = Mem(Bits(wordWidth bit),wayWordCount)
    val tag = tags.readSync(io.cpu.readCmd.addr(lineRange),io.cpu.readCmd.ready)
    val data = datas.readSync(io.cpu.readCmd.addr(wordRange),io.cpu.readCmd.ready)

//    val address = RegNext(io.cpu.readCmd.addr)
//    when(tag.valid && tag.address === address){
//      stage1.hitValid := True
//      stage1.hitWord := data
//    }
  })

  val loader = new Area{
    val requestIn = Stream(wrap(new Bundle{
      val addr = UInt(cmdParameters.addressWidth bit)
    }))

    io.mem.readCmd.len := wordPerLine-1
    io.mem.readCmd.addr(io.mem.readCmd.addr.high downto log2Up(memDataWidth)) := requestIn.addr(io.mem.readCmd.addr.high downto log2Up(memDataWidth))
    io.mem.readCmd.burst := AxiBurst.WRAP

    io.mem.readData.ready := True

    val pendingRequest = requestIn.haltWhen(!io.mem.readCmd.ready).m2sPipe()


  }

  val stage1 = new Area{
    val request = io.cpu.readCmd.m2sPipe()
    val hitValid = False
    val hitWord = Bits(wordWidth bit)
    hitWord.assignDontCare()

    val memRequestSent = RegInit(False)
    val memResponseCounter = Reg(UInt(log2Up(wordPerLine) bit))
    when(request.ready){
      memRequestSent := False
    }


    when(request.valid) {
      when(hitValid) {
        request.ready := io.cpu.readData.ready
        io.cpu.readData.valid := True
        io.cpu.readData.data := hitWord //TODO
      } otherwise {
        when(!memRequestSent) {
          io.mem.readCmd.valid := True
          memRequestSent := io.mem.readCmd.ready
          memResponseCounter := 0
        } otherwise {
          when(io.mem.readData.valid){
            memResponseCounter := memResponseCounter + 1
            when(memResponseCounter === wordPerLine){

            }
          }
        }
      }
    }
  }


}

object InstructionCacheMain{
  def main(args: Array[String]) {
  // SpinalVhdl(new InstructionCache(8192,32,1,SblConfig(32,32)).setDefinitionName("TopLevel"))
  }
}