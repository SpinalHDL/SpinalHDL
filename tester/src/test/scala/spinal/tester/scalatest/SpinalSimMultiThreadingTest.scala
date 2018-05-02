package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.sim._
import spinal.core._
import spinal.core.sim._

import scala.util.Random


class SpinalSimMultiThreadingDut(offset : Int) extends Component {
  val io = new Bundle {
    val a, b, c = in UInt (8 bits)
    val result = out UInt (8 bits)
  }
  io.result := RegNext(io.a + io.b - io.c + offset) init (0)
  //    def rec(that : UInt, level : Int) : UInt = if(level != 0)
  //      rec(RegNext(that), level -1)
  //    else
  //      that
  //    io.result := rec(io.a + io.b - io.c, 1000)
}

class SpinalSimMultiThreadingTest extends FunSuite {

  test("Test1") {
    var faild = false
    val threads = for (t <- 0 to 4) yield{
      new Thread {
        override def run() = {
          for (i <- 0 to 8) {
            try {
              SimConfig
                .withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC)))
                //        .compile()
//                .withWave
                .doSim (new SpinalSimMultiThreadingDut(i + t).setDefinitionName(s"SpinalSimMultiThreadingDut_${t}_${i}")){ dut =>
                  dut.clockDomain.forkStimulus(period = 10)

                  Suspendable.repeat(times = 1000000) {
                    val a, b, c = Random.nextInt(256)
                    dut.io.a #= a
                    dut.io.b #= b
                    dut.io.c #= c
                    dut.clockDomain.waitActiveEdge(); sleep(0)
                    if (dut.clockDomain.isResetDeasserted) assert(dut.io.result.toInt == ((a + b - c + i + t) & 0xFF))
                    ()
                  }
                }
            } catch {
              case e : Throwable => {
                faild = true
                println(e)
                println("FAILURE")
                throw e
              }
            }
          }
        }
      }
    }

    for(thread <- threads){
      thread.start()
      Thread.sleep(1000)
    }

    for(thread <- threads){
      thread.join()
    }

    assert(!faild)
  }
}
