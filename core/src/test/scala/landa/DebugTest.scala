package landa

import spinal.core._
import spinal.sim._
import spinal.core.SimManagedApi._

object DebugTest {
  class Dut extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (7 bits)
      val result = out UInt (7 bits)
    }
    val tmp = Bool
    io.result := RegNext(io.a + io.b - io.c)
  }
  def main(args: Array[String]): Unit = {
    def yolo() = 3
    SimConfig(rtl = new Dut).withWave.doManagedSim{ dut =>
      val x = yolo()
      yolo()

      println(dut.io.a.toLong)
      dut.io.a #= 42l
      println(dut.io.a.toLong)
      sleep(1)
      println(dut.io.a.toLong)
      dut.io.a #= 54l
      println(dut.io.a.toLong)
      sleep(1)
      println(dut.io.a.toLong)
    }
  }
}
