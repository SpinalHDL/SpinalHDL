package spinal.lib.misc.pdm

import spinal.core._
import spinal.lib._

class PDMCore(width: Int) extends Component {
  val io = new Bundle {
    val enable = in Bool
    val density = in UInt(width+1 bits)
    val output = out Bool
  }

  val counter = Reg(UInt(width bits))
  val outReg = Reg(Bool) init(False)
  when(io.enable) {
    val nextCounter = (U(0, 1 bit) @@ counter) + io.density
    counter := nextCounter(width-1 downto 0)
    outReg := nextCounter(width)
  } otherwise {
    counter := 0
    outReg := False
  }
  io.output := outReg
}
