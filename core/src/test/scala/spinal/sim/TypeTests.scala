
package spinal.sim

import spinal.sim.SimManagerApi._
import spinal.core._
import scala.util.continuations.suspendable


object TypeTests {

  class Dut extends Component {
    val io = new Bundle {
      val bool = in Bool
      val u1 = in UInt (1 bits)
      val u8 = in UInt (8 bits)
      val u16 = in UInt (16 bits)
      val u32 = in UInt (32 bits)
      val u64 = in UInt (64 bits)
      val u128 = in UInt (128 bits)


      val s4 = out SInt (4 bits)
    }
    io.s4 := -2
  }

  def main(args: Array[String]): Unit = {
    SimVerilatorManaged(new Dut) { dut =>
      sleep(10)
      println(dut.io.s4.toLong)
    }
  }
}
