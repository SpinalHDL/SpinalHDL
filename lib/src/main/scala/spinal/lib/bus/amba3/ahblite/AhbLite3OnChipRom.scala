package spinal.lib.bus.amba3.ahblite

import spinal.core._
import spinal.lib._

class AhbLite3OnChipRom(AhbLite3Config: AhbLite3Config,content : => Seq[Bits]) extends Component{
  val io = new Bundle {
    val ahb = slave(AhbLite3(AhbLite3Config))
  }

  val ram = Mem(content)
  val wordRange = ram.addressWidth + log2Up(AhbLite3Config.bytePerWord)-1 downto log2Up(AhbLite3Config.bytePerWord)

  io.ahb.setOKEY

  io.ahb.HREADYOUT := True
  io.ahb.HRDATA := ram.readSync(
    address = io.ahb.HADDR(wordRange),
    enable = io.ahb.HSEL && io.ahb.HTRANS(1) && !io.ahb.HWRITE && io.ahb.HREADY
  )
}

