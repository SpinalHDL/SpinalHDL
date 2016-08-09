package spinal.lib.bus.amba3.ahblite

import spinal.core._
import spinal.lib._

case class AhbLite3OnChipRam(AhbLite3Config: AhbLite3Config,byteCount : Int) extends Component{
  val io = new Bundle {
    val ahb = slave(AhbLite3(AhbLite3Config))
  }

  val wordCount = byteCount / AhbLite3Config.bytePerWord
  val ram = Mem(AhbLite3Config.dataType,wordCount)
  val wordRange = log2Up(wordCount) + log2Up(AhbLite3Config.bytePerWord)-1 downto log2Up(AhbLite3Config.bytePerWord)

  //Address/control phase to write data phase
  val pendingWrite = Reg(new Bundle{
    val valid = Bool
    val address = ram.addressType
    val mask    = Bits(AhbLite3Config.bytePerWord bits)
  })
  
  pendingWrite.valid init(False)
  when(io.ahb.HREADY){
    pendingWrite.valid := io.ahb.HSEL && io.ahb.HTRANS(1) && io.ahb.HWRITE
    pendingWrite.address := io.ahb.HADDR(wordRange)
    pendingWrite.mask := io.ahb.writeMask
  }

  io.ahb.setOKEY

  //Avoid write to read hazards
  io.ahb.HREADYOUT := !(io.ahb.HSEL && io.ahb.HTRANS(1) && !io.ahb.HWRITE && pendingWrite.valid && io.ahb.HADDR(wordRange) === pendingWrite.address)
  io.ahb.HRDATA := ram.readSync(
    address = io.ahb.HADDR(wordRange),
    enable = io.ahb.HSEL && io.ahb.HTRANS(1) && !io.ahb.HWRITE && io.ahb.HREADY
  )

  ram.write(
    enable = pendingWrite.valid && io.ahb.HREADY,
    address = pendingWrite.address,
    mask = pendingWrite.mask,
    data = io.ahb.HWDATA
  )
}


object AhbLite3OnChipRam{
  def main(args: Array[String]) {
    SpinalVhdl(new AhbLite3OnChipRam(AhbLite3Config(32,32),1024))
  }
}