package landa

import spinal.core.SimManagedApi._
import spinal.core._
import spinal.sim._

import scala.util.Random


object SimDemoCombinatorial {
  class Dut extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }
    io.result := io.a + io.b - io.c
  }

  def main(args: Array[String]): Unit = {
    SimConfig(rtl = new Dut).withWave.doManagedSim{ dut =>
      var idx = 0
      while(idx < 100){
        val a, b, c = Random.nextInt(256)
        dut.io.a #= a
        dut.io.b #= b
        dut.io.c #= c
        sleep(1)
        assert(dut.io.result.toInt == ((a+b-c) & 0xFF))
        idx += 1
      }
    }
  }
}
