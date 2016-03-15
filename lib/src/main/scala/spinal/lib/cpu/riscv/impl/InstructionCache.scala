package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib._
import spinal.lib.bus.sbl._
import spinal.lib.cpu.riscv.impl.Utils.InstructionCtrl

class InstructionCache(cacheSize : Int,bytePerLine : Int,wayCount : Int,cpuConfig : SblConfig) extends Component{
  val io = new Bundle{
    val cpu = new Bundle {
      val write = slave Flow (SblWriteCmd(cpuConfig))
      val cmd = slave Stream (SblReadCmd(cpuConfig))
      val rsp = master Stream (SblReadRet(cpuConfig))
    }

    val mem = new Bundle {
//      val cmd = master Stream (SblReadCmd(cpuConfig))
//      val rsp = slave Flow (SblReadRet(cpuConfig))
    }
  }
  val cachesWordWidth = cpuConfig.dataWidth
  val cachesLineCount = cacheSize/bytePerLine
  val cacheLineCount = cachesLineCount/wayCount
  val cacheLineLog2 = log2Up(cacheLineCount)

  val tagRange = cpuConfig.addressWidth-1 downto cacheLineLog2
  val wayRange = cacheLineLog2-1 downto 0

  class LineInfo() extends Bundle{
    val valid = Bool
    val dirty = Bool
    val address = UInt(tagRange.length bit)
  }


  val rsp = Stream (SblReadRet(cpuConfig))

  rsp.valid := False
  rsp.data.assignDontCare()

  val ways = Array.fill(wayCount)(new Area{
    val tags = Mem(new LineInfo(),cacheLineCount)
    val datas = Mem(Bits(cachesWordWidth bit),cacheLineCount)
    val tag = tags.readSync(io.cpu.cmd.address(wayRange))
    val data = datas.readSync(io.cpu.cmd.address(wayRange))

    val address = RegNext(io.cpu.cmd.address)
    val hit = tag.valid && tag.address === address

    when(hit){
      rsp.valid := True
      rsp.data := data
    }

    val tagWrite = new LineInfo()
    tagWrite.address := io.cpu.cmd.address(tagRange)
    tagWrite.valid := True
    tagWrite.dirty := True
    tags(io.cpu.cmd.address(wayRange)) := tagWrite
    datas(io.cpu.write.address(wayRange)) := io.cpu.write.data

  })


}

object InstructionCacheMain{
  def main(args: Array[String]) {
    SpinalVhdl(new InstructionCache(8192,32,1,SblConfig(32,32)).setDefinitionName("TopLevel"))
  }
}