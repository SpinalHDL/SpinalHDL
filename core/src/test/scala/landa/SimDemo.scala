package landa

import spinal.sim._
import spinal.core._
import spinal.core.sim._

import scala.util.Random


object SimDemo {
  class Dut extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }
    io.result := RegNext(io.a + io.b - io.c) init(0)
//    def rec(that : UInt, level : Int) : UInt = if(level != 0)
//      rec(RegNext(that), level -1)
//    else
//      that
//    io.result := rec(io.a + io.b - io.c, 1000)
  }

  def main(args: Array[String]): Unit = {
    //For alternatives ways of running the sim, see note at the end of the file
    for (t <- 0 to 4) {
      new Thread {
        override def run() = {
          for (i <- 0 to 4) {
            //Little memory leak (300KB)
            println("\n"*3)
//            println(s"Free memory => ${Runtime.getRuntime.freeMemory()/1024}/${Runtime.getRuntime.totalMemory()/1024}")
//            System.gc()
            println(s"Free memory => ${Runtime.getRuntime.freeMemory()/1024}/${Runtime.getRuntime.totalMemory()/1024}")
            println("\n"*3)
            try {
              SimConfig
                .withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC)))
                //        .compile()
                .withWave
                .doSim(new Dut().setDefinitionName(s"Dut_${t}_${i}")) { dut =>
                  dut.clockDomain.forkStimulus(period = 10)

                  for(repeat <- 0 until 100) {
                    val a, b, c = Random.nextInt(256)
                    dut.io.a #= a
                    dut.io.b #= b
                    dut.io.c #= c
                    dut.clockDomain.waitActiveEdge()
                    if (dut.clockDomain.isResetDeasserted) assert(dut.io.result.toInt == ((a + b - c ) & 0xFF))
                  }
                }
            } catch {
              case e : Throwable => {
                println(e)
                println("FAILURE")
                System.exit(1)
              }
            }
          }
        }
      }.start()
      Thread.sleep(1000)
    }
  }
}

//Note that there is two ways to run the sim :
// SimConfig(rtl = new Dut).withWave.doSim{ dut =>
// SimConfig(rtl = SpinalVerilog(new Dut)).withWave.doSim{ dut =>
