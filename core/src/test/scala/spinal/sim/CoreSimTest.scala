package spinal.sim

import spinal.core._


object CoreSimTest {
  class Dut extends Component{
    val io = new Bundle{
      val a,b,c = in UInt(8 bits)
      val result = out UInt(8 bits)
    }
    io.result := io.a + io.b - io.c
  }

  def main(args: Array[String]): Unit = {
    val sim = VerilatorSim(new Dut)
    import sim._
    var counter = 0
    var idx = 1000000
    while (idx != 0) {
      idx -= 1
      poke(sim.dut.io.a, 3)
      poke(sim.dut.io.b, 5)
      poke(sim.dut.io.c, 1)
      step()
      counter += peak(sim.dut.io.result)
    }
    println(counter)
  }
}
