//package spinal.sim
//
//
//import spinal.core._
//
//
//import SimRaw._
//object CoreSimTest {
//  class Dut extends Component{
//    val io = new Bundle{
//      val a,b,c = in UInt(8 bits)
//      val result = out UInt(8 bits)
//    }
//    val tmp = (0 until 1).map(i => RegNext(io.a + io.b - io.c)).reduceLeft(_ | _)
//    io.result := tmp
//  }
//
//  def main(args: Array[String]): Unit = {
//    val sim = SimVerilator(new Dut)
//    import sim.dut
//    SpinalProgress("Sim")
//    Bench(1000000) {
//      var counter = 0l
//      var idx = 1000000
//
//
//      while (idx != 0) {
//        idx -= 1
//        counter += dut.io.result.toLong
//        dut.io.a :<< dut.io.a.toLong + 1
//        dut.io.b :<< 5
//        dut.io.c :<< 1
//        sleep(5); sim.dut.clockDomain.fallingEdge;eval()
//        sleep(5); sim.dut.clockDomain.risingEdge ;eval()
//      }
//      println(counter)
//      SpinalProgress("Done")
//    }
//    end()
//  }
//}
