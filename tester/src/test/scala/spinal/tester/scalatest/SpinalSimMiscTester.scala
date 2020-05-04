package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.sim._
import spinal.core.sim.{SpinalSimConfig, _}
import spinal.lib.BufferCC
import spinal.tester
import spinal.tester.scalatest

import scala.concurrent.{Await, Future}
import scala.util.Random

object SpinalSimMiscTester{
  class SpinalSimMiscTesterCounter extends Component{
    val io = new Bundle{
      val enable = in Bool
      val value = out UInt(8 bits)
    }

    val reg = Reg(UInt(8 bits)) init(0)
    when(io.enable){
      reg := reg + 1
    }
    io.value := reg
  }

}

abstract class SpinalSimTester{
  def SimConfig : SpinalSimConfig
  def durationFactor : Double
  def designFactor : Double
  def prefix : String
}

object SpinalSimTesterGhdl extends SpinalSimTester{
  override def SimConfig: SpinalSimConfig = spinal.core.sim.SimConfig.withGhdl
  override def durationFactor: Double = 0.01
  override def designFactor: Double = 0.1
  override def prefix: String = "ghdl_"
}

object SpinalSimTesterVerilator extends SpinalSimTester{
  override def SimConfig: SpinalSimConfig = spinal.core.sim.SimConfig.withVerilator
  override def durationFactor: Double = 1.0
  override def designFactor: Double = 1.0
  override def prefix: String = "verilator_"
}

object SpinalSimTester{

  def apply(body :  => SpinalSimTester => Unit): Unit = {
    body(SpinalSimTesterGhdl)
    body(SpinalSimTesterVerilator)
  }
}

class SpinalSimTesterTest extends FunSuite {
  SpinalSimTester{ env =>
    import env._

    test(prefix + "a"){
      println(SimConfig._backend + " " + durationFactor)
    }
  }
}

class SpinalSimFunSuite extends FunSuite{
  var SimConfig : SpinalSimConfig = null
  var durationFactor = 0.0
  def test(testName: String)(testFun: => Unit): Unit = {
    super.test("ghdl_" + testName) {
      SimConfig = SpinalSimTesterGhdl.SimConfig
      durationFactor = SpinalSimTesterGhdl.durationFactor
      testFun
    }
    super.test("verilator_" + testName) {
      SimConfig = SpinalSimTesterVerilator.SimConfig
      durationFactor = SpinalSimTesterVerilator.durationFactor
      testFun
    }
  }
}

class SpinalSimMiscTester extends FunSuite {
  SpinalSimTester { env =>
    import env._
    var compiled: SimCompiled[tester.scalatest.SpinalSimMiscTester.SpinalSimMiscTesterCounter] = null

    test(prefix + "testForkSensitive") {
      SimConfig.compile(new Component {
        val a, b = in UInt (8 bits)
        val result = out UInt (8 bits)
        result := RegNext(a + b) init (0)
      }).doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        var counter = 0
        while (counter < 100) {
          dut.a.randomize()
          dut.b.randomize()
          dut.clockDomain.waitSampling()
          val buffer = (dut.a.toInt + dut.b.toInt) & 0xFF
          dut.clockDomain.onNextSampling {
            assert(dut.result.toInt == buffer)
            counter += 1
          }
        }
      }
    }


    test(prefix + "compile") {
      compiled = SimConfig.withWave.compile(new tester.scalatest.SpinalSimMiscTester.SpinalSimMiscTesterCounter)
    }

    def doStdtest(name: String): Unit = {
      test(prefix + name) {
        compiled.doSim("testStd")(dut => {
          dut.clockDomain.forkStimulus(10)

          var counterModel = 0
          for (repeat <- 0 until 100) {
            dut.io.enable.randomize()
            dut.clockDomain.waitActiveEdge(); sleep(0)
            if (dut.io.enable.toBoolean) {
              counterModel = (counterModel + 1) & 0xFF
            }
            assert(dut.io.value.toInt == counterModel)
          }
        })
      }
    }

    def doStdTestUnnamed(name: String): Unit = {
      test(prefix + name) {
        compiled.doSim(dut => {
          dut.clockDomain.forkStimulus(10)

          var counterModel = 0
          for (repeat <- 0 until 100) {
            dut.io.enable.randomize()
            dut.clockDomain.waitActiveEdge(); sleep(0)
            if (dut.io.enable.toBoolean) {
              counterModel = (counterModel + 1) & 0xFF
            }
            assert(dut.io.value.toInt == counterModel)
          }
        })
      }
    }

    doStdtest(prefix + "testStd1")
    doStdtest(prefix + "testStd2")
    doStdtest(prefix + "testStd3")
    doStdTestUnnamed("testStd4")
    doStdTestUnnamed("testStd5")
    doStdTestUnnamed("testStd6")


    test(prefix + "testSimSuccess") {
      compiled.doSim(dut => {
        dut.clockDomain.forkStimulus(10)

        var counterModel = 0
        var counter = 0
        while (true) {
          dut.io.enable.randomize()
          dut.clockDomain.waitActiveEdge();
          sleep(0)
          if (dut.io.enable.toBoolean) {
            counterModel = (counterModel + 1) & 0xFF
          }
          assert(dut.io.value.toInt == counterModel)
          counter += 1
          if (counter == 1000) simSuccess()
        }
      })
    }

    test(prefix + "testSimFailure") {
      intercept[SimFailure] {
        compiled.doSim(dut => {
          dut.clockDomain.forkStimulus(10)

          var counterModel = 0
          for (repeat <- 0 until 100) {
            dut.io.enable.randomize()
            dut.clockDomain.waitActiveEdge()
            if (dut.io.enable.toBoolean) {
              counterModel = (counterModel + 2) & 0xFF
            }
            if (dut.io.value.toInt != counterModel) {
              simFailure("miaou")
            }
          }
        })
      }
    }


    test(prefix + "testWaitSampling") {
      compiled.doSim(dut => {
        dut.clockDomain.forkStimulus(10)

        var counterModel = 0
        for (repeat <- 0 until 1000) {
          dut.io.enable.randomize()
          dut.clockDomain.waitSampling(); sleep(0)
          if (dut.io.enable.toBoolean) {
            counterModel = (counterModel + 1) & 0xFF
          }
          assert(dut.io.value.toInt == counterModel)
        }
      })
    }

    test(prefix + "testdoSimUntilVoid") {
      var counterCheck = 0
      var counterClock = 0
      compiled.doSimUntilVoid("testdoSimUntilVoid")(dut => {
        fork {
          dut.clockDomain.deassertReset()
          sleep(0)
          dut.clockDomain.fallingEdge()
          dut.clockDomain.assertReset()
          sleep(10)
          dut.clockDomain.deassertReset()
          sleep(10)

          for (repeat <- 0 until 2000) {
            dut.clockDomain.risingEdge()
            sleep(10)
            dut.clockDomain.fallingEdge()
            sleep(10)
            counterClock += 1
          }
        }

        fork {
          var counterModel = 0
          for (repeat <- 0 until 1000) {
            dut.io.enable.randomize()
            dut.clockDomain.waitActiveEdge(); sleep(0)
            if (dut.io.enable.toBoolean) {
              counterModel = (counterModel + 1) & 0xFF
            }
            assert(dut.io.value.toInt == counterModel)
            counterCheck += 1
          }
        }
        ()
      })
      assert(counterCheck == 1000)
      assert(counterClock == 2000)
    }

    test(prefix + "testRecompile1") {
      SimConfig.withWave.doSim(new tester.scalatest.SpinalSimMiscTester.SpinalSimMiscTesterCounter)(dut => {
        dut.clockDomain.forkStimulus(10)

        var counterModel = 0
        for (repeat <- 0 until 100) {
          dut.io.enable.randomize()
          dut.clockDomain.waitActiveEdge(); sleep(0)
          if (dut.io.enable.toBoolean) {
            counterModel = (counterModel + 1) & 0xFF
          }
          if (dut.io.value.toInt != counterModel) {
            simFailure("miaou")
          }
        }
      })
    }


    test(prefix + "testRecompile2") {
      SimConfig.withWave.doSim(new tester.scalatest.SpinalSimMiscTester.SpinalSimMiscTesterCounter)(dut => {
        dut.clockDomain.forkStimulus(10)

        var counterModel = 0
        for (repeat <- 0 until 100) {
          dut.io.enable.randomize()
          dut.clockDomain.waitActiveEdge(); sleep(0)
          if (dut.io.enable.toBoolean) {
            counterModel = (counterModel + 1) & 0xFF
          }
          if (dut.io.value.toInt != counterModel) {
            simFailure("miaou")
          }
        }
      })
    }


    test(prefix + "testCompInWhen") {
      SimConfig.compile(new Component {
        val src = in Bool()
        val sel = in Bool()
        val dst = out Bool()

        dst := False
        when(sel) {
          dst := BufferCC(src, False)
        }
      }).doSim { dut =>
        dut.clockDomain.forkStimulus(10)

        var modelA, modelB = false
        for (repeat <- 0 until 100) {
          dut.src.randomize()
          dut.sel.randomize()
          dut.clockDomain.waitSampling()
          assert(dut.dst.toBoolean == (modelB && dut.sel.toBoolean))
          modelB = modelA
          modelA = dut.src.toBoolean
        }
      }
    }

    test(prefix + "NonBB") {
      SimConfig.compile(new Component {
        val bb = new BlackBox {
          clearBlackBox()
          val a, b = in UInt (8 bits)
          val result = out UInt (8 bits)
          result := a ^ b
        }
        val a, b = in UInt (8 bits)
        val result = out UInt (8 bits)

        bb.a <> a
        bb.b <> b
        bb.result <> result
      }).doSim { dut =>
        for (i <- 0 to 10) {
          dut.a.randomize()
          dut.b.randomize()
          sleep(1)
          assert(dut.result.toInt == (dut.a.toInt ^ dut.b.toInt))
        }
      }
    }


    test(prefix + "testCatchAssert") {
      var i = 35

      try {
        SimConfig.doSim(new Component {
          val a = in UInt (8 bits)
          spinal.core.assert(a =/= 42, FAILURE)
        }) { dut =>
          dut.clockDomain.forkStimulus(10)
          while (i < 50) {
            dut.a #= i
            dut.clockDomain.waitSampling()
            i += 1
          }
          throw new Exception()
        }
      } catch {
        case e: Exception =>
      }
      assert(i == 43)
    }


    test(prefix + "intLongBigInt") {
      SimConfig.withWave.doSim(new Component {
        val x = out Bits (31 bits)
        val y = out Bits (63 bits)
        val z = out Bits (128 bits)
        x := 0x12345678
        y := 0x1234567812345678l
        z := BigInt("12345678123456781234567812345678", 16)
      }) { dut =>
        //      dut.clockDomain.forkStimulus(10)
        //      sleep(10)
        assert(dut.x.toInt == 0x12345678)
        assert(dut.y.toLong == 0x1234567812345678l)
        assert(dut.z.toBigInt == BigInt("12345678123456781234567812345678", 16))
      }
    }

  }
}
