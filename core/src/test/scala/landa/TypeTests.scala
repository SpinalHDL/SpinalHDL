package landa;

import landa.SimManagedTest.Dut
import spinal.core._

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

      val u8_o = out UInt (8 bits)

//      val s4 = out SInt (4 bits)
    }

    io.u8_o := 42
//    io.s4 := -2
  }

  import spinal.core.sim._
  def main(args: Array[String]): Unit = {
//    val (sim, dut) = SpinalSimVerilator(new Dut)
//    val manager = new SimManager(sim)
//    manager.run{
//      sleep(10)
//      sleep(10)
//      println(peak(dut.io.u8_o))
//    }

    SimConfig.doSim(new Dut) { dut =>
      sleep(10)
      println((dut.io.u8_o.toLong))
    }
  }
}
