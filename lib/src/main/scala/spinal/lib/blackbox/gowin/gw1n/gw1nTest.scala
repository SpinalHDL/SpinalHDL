import spinal.core._
import spinal.lib.blackbox.gowin.gw1n._


case class Gw1nTest() extends Component {
  val io = new Bundle {
    val x = in Bits (4 bit)
    val s = in Bits (4 bit)
    val y = out Bool()
  }
  val mux4 = MUX4()
  mux4.I0 := io.x(0).asBits
  mux4.I1 := io.x(1).asBits
  mux4.I2 := io.x(2).asBits
  mux4.I3 := io.x(3).asBits
  mux4.S0 := io.s(0).asBits
  mux4.S1 := io.s(1).asBits
  io.y := mux4.O(0)
}

object Test{
  def main(args: Array[String]) {
    spinal.core.SpinalVerilog(Gw1nTest())
  }
}