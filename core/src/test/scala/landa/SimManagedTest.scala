package landa


import spinal.core.sim._
import spinal.core._
import spinal.sim._

object SimManagedTest {
  class Dut extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }
    io.result := RegNext(io.a + io.b - io.c)
  }

  def main(args: Array[String]): Unit = {
    SimConfig.doSim(rtl = new Dut){ dut =>
      fork {
        var counter = 0l
        var lastTime = System.nanoTime()
        while(true) {
          dut.clockDomain.waitActiveEdge()
          counter += 1
          if((counter & 0x1000) == 0){
            val newTime = System.nanoTime()
            val deltaTime = newTime - lastTime
            if(deltaTime > 1000000000){
              lastTime = newTime;
              println(f"[INFO] Simulation speed : ${counter / (deltaTime*1e-9) * 1e-3}%5.0f kHz/sec")
              counter = 0
            }
          }
        }
      }




      val t1 = fork {
        var idx = 0
        sleep(2)
        while (idx < 20) {
          dut.io.a #= dut.io.a.toLong + 1
          sleep(10)
          println(idx)
          idx += 1
        }
      }


      val clkGen = fork {
        var idx = 0
        while (idx < 2000000) {
          dut.clockDomain.fallingEdge
          sleep(10)
          dut.clockDomain.risingEdge
          sleep(10)
          idx += 1
        }
      }

      val t2 = fork {
        var idx = 0
        while (idx < 20) {
          dut.clockDomain.waitRisingEdge
          dut.io.b #= dut.io.b.toLong + 1
          idx += 1
        }
      }

      val t3 = fork {
        sleep(2)
        var idx = 0
        while (idx < 20) {
          dut.io.c #= dut.io.c.toLong + 1
          sleep(30)
          idx += 1
        }
      }

      val t4 = fork{
        waitUntil(dut.io.a.toLong == 66)
        dut.io.b #= 77l
        waitUntil(dut.io.a.toLong == 88)
        dut.io.b #= 99l
        sleep(10)
      }

      t1.join()
      sleep(50)
      dut.io.a #= 42l
      sleep(10)

      def doStuff(bt: UInt, value : Long): Long = {
        bt #= value
        sleep(40)
        bt.toLong + 1
      }

      t3.join()
      println(doStuff(dut.io.a, 66))
      println(doStuff(dut.io.a, 88))

      clkGen.join()

      fork{
        for(i <- 0 until 100){
          dut.io.a #= dut.io.a.toLong + 1
          sleep(1)
        }
      }.join()
    }
  }
}
