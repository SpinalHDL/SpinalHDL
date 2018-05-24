package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.sim._
import spinal.core.sim._
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

class SpinalSimMiscTester extends FunSuite {
  var compiled : SimCompiled[tester.scalatest.SpinalSimMiscTester.SpinalSimMiscTesterCounter] = null

  test("compile"){
    compiled = SimConfig.withWave.compile(new tester.scalatest.SpinalSimMiscTester.SpinalSimMiscTesterCounter)
  }

  def doStdTest(name : String): Unit ={
    test(name){
      compiled.doSim("testStd")(dut => {
        dut.clockDomain.forkStimulus(10)

        var counterModel = 0
        Suspendable.repeat(100) {
          dut.io.enable.randomize()
          dut.clockDomain.waitActiveEdge(); sleep(0)
          if (dut.io.enable.toBoolean) {
            counterModel = (counterModel + 1) & 0xFF
          }
          assert(dut.io.value.toInt == counterModel)
          ()
        }
      })
    }
  }

  def doStdTestUnamed(name : String): Unit ={
    test(name){
      compiled.doSim(dut => {
        dut.clockDomain.forkStimulus(10)

        var counterModel = 0
        Suspendable.repeat(100) {
          dut.io.enable.randomize()
          dut.clockDomain.waitActiveEdge(); sleep(0)
          if (dut.io.enable.toBoolean) {
            counterModel = (counterModel + 1) & 0xFF
          }
          assert(dut.io.value.toInt == counterModel)
          ()
        }
      })
    }
  }

  doStdTest("testStd1")
  doStdTest("testStd2")
  doStdTest("testStd3")
  doStdTestUnamed("testStd4")
  doStdTestUnamed("testStd5")
  doStdTestUnamed("testStd6")


  test("testSimSuccess"){
    compiled.doSim(dut => {
      dut.clockDomain.forkStimulus(10)

      var counterModel = 0
      var counter = 0
      while(true) {
        dut.io.enable.randomize()
        dut.clockDomain.waitActiveEdge(); sleep(0)
        if (dut.io.enable.toBoolean) {
          counterModel = (counterModel + 1) & 0xFF
        }
        assert(dut.io.value.toInt == counterModel)
        counter += 1
        if(counter == 1000) simSuccess()
      }
    })
  }

  test("testSimFailure"){
    intercept[SimFailure] {
      compiled.doSim(dut => {
        dut.clockDomain.forkStimulus(10)

        var counterModel = 0
        Suspendable.repeat(100) {
          dut.io.enable.randomize()
          dut.clockDomain.waitActiveEdge()
          if (dut.io.enable.toBoolean) {
            counterModel = (counterModel + 2) & 0xFF
          }
          if(dut.io.value.toInt != counterModel){
            simFailure("miaou")
          }
        }
      })
    }
  }


  test("testWaitSampling"){
    compiled.doSim(dut => {
      dut.clockDomain.forkStimulus(10)

      var counterModel = 0
      Suspendable.repeat(1000){
        dut.io.enable.randomize()
        dut.clockDomain.waitSampling(); sleep(0)
        if (dut.io.enable.toBoolean) {
          counterModel = (counterModel + 1) & 0xFF
        }
        assert(dut.io.value.toInt == counterModel)
        ()
      }
    })
  }

  test("testdoSimUntilVoid"){
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

        Suspendable.repeat(2000) {
          dut.clockDomain.risingEdge()
          sleep(10)
          dut.clockDomain.fallingEdge()
          sleep(10)
          counterClock += 1
        }
      }

      fork {
        var counterModel = 0
        Suspendable.repeat(1000) {
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

  test("testRecompile1"){
    SimConfig.withWave.doSim(new tester.scalatest.SpinalSimMiscTester.SpinalSimMiscTesterCounter)(dut => {
      dut.clockDomain.forkStimulus(10)

      var counterModel = 0
      Suspendable.repeat(100) {
        dut.io.enable.randomize()
        dut.clockDomain.waitActiveEdge(); sleep(0)
        if (dut.io.enable.toBoolean) {
          counterModel = (counterModel + 1) & 0xFF
        }
        if(dut.io.value.toInt != counterModel){
          simFailure("miaou")
        }
      }
    })
  }




  test("testRecompile2"){
    SimConfig.withWave.doSim(new tester.scalatest.SpinalSimMiscTester.SpinalSimMiscTesterCounter)(dut => {
      dut.clockDomain.forkStimulus(10)

      var counterModel = 0
      Suspendable.repeat(100) {
        dut.io.enable.randomize()
        dut.clockDomain.waitActiveEdge(); sleep(0)
        if (dut.io.enable.toBoolean) {
          counterModel = (counterModel + 1) & 0xFF
        }
        if(dut.io.value.toInt != counterModel){
          simFailure("miaou")
        }
      }
    })
  }


  test("testLegacy"){
    SimConfig(new tester.scalatest.SpinalSimMiscTester.SpinalSimMiscTesterCounter).withWave.doManagedSim(dut => {
      dut.clockDomain.forkStimulus(10)

      var counterModel = 0
      Suspendable.repeat(100) {
        dut.io.enable.randomize()
        dut.clockDomain.waitActiveEdge(); sleep(0)
        if (dut.io.enable.toBoolean) {
          counterModel = (counterModel + 1) & 0xFF
        }
        if(dut.io.value.toInt != counterModel){
          simFailure("miaou")
        }
      }
    })
  }

  test("testCompInWhen"){
    SimConfig.compile(new Component{
      val src = in Bool()
      val sel = in Bool()
      val dst = out Bool()

      dst := False
      when(sel){
        dst := BufferCC(src, False)
      }
    }).doSim{dut =>
      dut.clockDomain.forkStimulus(10)

      var modelA, modelB = false
      Suspendable.repeat(100){
        dut.src.randomize()
        dut.sel.randomize()
        dut.clockDomain.waitSampling()
        assert(dut.dst.toBoolean == (modelB && dut.sel.toBoolean))
        modelB = modelA
        modelA = dut.src.toBoolean
      }
    }
  }
}
