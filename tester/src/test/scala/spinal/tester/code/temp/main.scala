package spinal.tester.temp
import spinal.core._
import spinal.lib._ 
import spinal.lib.bus.amba4.axilite._
import spinal.tester.code.temp._

class ZynqTest extends Component {
  val io = new Bundle {
    val axi_slv = slave(AxiLite4(AxiLite4Config(addressWidth=4,
        dataWidth=32)))
    val leds = out Bits(4 bits)
  }

  val busCtrl = new AxiLite4SlaveFactory(io.axi_slv)

  for (led <- 0 to 3) {
    var pwmController = new PwmModulator()
    busCtrl.driveAndRead(pwmController.io.duty, address=led*4)
    io.leds(led) := pwmController.io.pwm
  }

}

object MyMain {
  def main(args: Array[String]) {
    SpinalVhdl(new ZynqTest)
    SFix(2 exp, 3 exp).miaou(4)

  }
}

