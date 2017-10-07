package spinal.tester

import spinal.core._
import spinal.lib._


object PlayDevMem{
  class TopLevel extends Component {
    val mem = Mem(Bits(32 bits), 64)
    val p0 = new Bundle{
      val address = in(mem.addressType)
      val data = in(mem.wordType)
      val enable = in Bool()
    }
    mem.write(p0.address,p0.data,p0.enable)

    val p1 = new Bundle{
      val address = in(mem.addressType)
      val data = out(mem.wordType)
    }
    p1.data := mem.readSync(p1.address, True)
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
  }
}
