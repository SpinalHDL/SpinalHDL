package spinal.sim

import SimManagedContext._
import spinal.sim.CoreSimTest.Dut
import SimManagedContext._

object SimManagedTest {
  def main(args: Array[String]): Unit = {
//    val times = 4000000
//    Bench(times) {
      val sim = SimVerilator(new Dut)
      val manager = new SimManaged(sim).run{
        fork {
          var idx = 0
          while (idx < 20) {
            sim.dut.io.a :<< sim.dut.io.a.toLong + 1
            sleep(10)
            println(idx)
            idx += 1
          }
        }

        fork{
          var idx = 0
          while(idx < 20){
            sim.dut.io.b :<< sim.dut.io.b.toLong + 1
            sleep(20)
            idx += 1
          }
        }

        fork{
          var idx = 0
          while(idx < 20){
            sim.dut.io.c :<< sim.dut.io.c.toLong + 1
            sleep(30)
            idx += 1
          }
        }
      }
//    }
  }
}
