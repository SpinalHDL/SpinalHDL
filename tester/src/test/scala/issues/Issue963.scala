package issues

import org.scalatest.funsuite.AnyFunSuite

import spinal.core._
import spinal.core.sim._
import spinal.lib._

// unit test for issue #963

// [info] - Minimal example for Issue963 which fails Spinal elaboration *** FAILED ***
// [info]   java.lang.AssertionError: assertion failed
// [info]   at scala.Predef$.assert(Predef.scala:156)
// [info]   at spinal.core.package$.assert(core.scala:463)
// [info]   at spinal.lib.whenIndexed$.apply(Utils.scala:1320)
// [info]   at spinal.lib.StreamFragmentWidthAdapter$$anon$56$$anon$41$$anonfun$41.apply$mcV$sp(Stream.scala:1601)
// [info]   at spinal.core.when$.apply(when.scala:90)
// [info]   at spinal.lib.StreamFragmentWidthAdapter$$anon$56$$anon$41.<init>(Stream.scala:1600)
// [info]   at spinal.lib.StreamFragmentWidthAdapter$$anon$56.<init>(Stream.scala:1562)
// [info]   at spinal.lib.StreamFragmentWidthAdapter$.apply(Stream.scala:1541)
// [info]   at spinal.tester.scalatest.Issue963$$anonfun$1$Dut$1.<init>(Issue963.scala:40)
// [info]   at spinal.tester.scalatest.Issue963$$anonfun$1$$anonfun$apply$mcV$sp$1$$anon$2.<init>(Issue963.scala:45)
// [info]   ...
// [info] Run completed in 532 milliseconds.
// [info] Total number of tests run: 1
// [info] Suites: completed 1, aborted 0
// [info] Tests: succeeded 0, failed 1, canceled 0, ignored 0, pending 0
// [info] *** 1 TEST FAILED ***

class Issue963 extends AnyFunSuite {
  test("Minimal example for Issue963 which fails Spinal elaboration") {
    class Dut extends Component {
      val io = new Bundle {
        val sink = slave Stream (Fragment(Bits(8 bits)))
        val source = master Stream (Fragment(Bits(16 bits)))
      }
      val x = Stream(Fragment(Bits(16 bits)))

      StreamFragmentWidthAdapter(io.sink, x, earlyLast = true)
      io.source <-< x
    }

    SimConfig
      .withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(4000 Hz)))
      .compile(new Component {
        val issue = new Dut()
      })
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.waitSampling()
      }
  }
}
