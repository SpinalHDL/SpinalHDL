package spinal.lib.bus.amba3.ahb

import spinal.core._
import spinal.lib._

class Ahb3OnChipRam(ahb3Config: Ahb3Config,byteCount : Int) extends Component{
  val io = new Bundle {
    val ahb = slave(Ahb3Slave(ahb3Config))
  }

  val wordCount = byteCount / ahb3Config.bytePerWord
  val ram = Mem(ahb3Config.dataType,wordCount)
  val wordRange = log2Up(wordCount) + log2Up(ahb3Config.bytePerWord)-1 downto log2Up(ahb3Config.bytePerWord)

  //Address/control phase to write data phase
  val writeFlow = Reg(Flow(new Bundle{
    val address = ram.addressType
    val mask    = Bits(ahb3Config.bytePerWord bits)
  }))
  writeFlow.valid init(False)
  when(io.ahb.HREADYIN){
    writeFlow.valid := io.ahb.HSEL && io.ahb.HTRANS(1) && io.ahb.HWRITE
    writeFlow.address := io.ahb.HADDR(wordRange)
    writeFlow.mask := io.ahb.writeMask
  }

  io.ahb.setOKEY

  //Avoid write to read hazards
  io.ahb.HREADYOUT := !(io.ahb.HSEL && io.ahb.HTRANS(1) && !io.ahb.HWRITE && writeFlow.valid && io.ahb.HADDR(wordRange) === writeFlow.address)

  io.ahb.HRDATA := ram.readSync(
    address = io.ahb.HADDR(wordRange),
    enable = io.ahb.HSEL && io.ahb.HTRANS(1) && !io.ahb.HWRITE
  )

  ram.write(
    enable = writeFlow.valid && io.ahb.HREADYIN,
    address = writeFlow.address,
    mask = writeFlow.mask,
    data = io.ahb.HWDATA
  )
}


object Ahb3OnChipRam{
  def main(args: Array[String]) {
    SpinalVhdl(new Ahb3OnChipRam(Ahb3Config(32,32),1024))
  }
}