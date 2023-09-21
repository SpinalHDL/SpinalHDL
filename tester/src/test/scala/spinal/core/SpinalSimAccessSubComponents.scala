package spinal.core

import sim._
import spinal.sim._

import spinal.tester.{SpinalAnyFunSuite, SpinalSimTester}

object SpinalSimAccessSubComponents {
  class SubSub extends Component {
    val io = new Bundle {
      val a,b = in UInt(8 bits)
      val result = out UInt(8 bits)
    }

    io.result := io.a ^ io.b
    val miaou = (io.a & io.b)

    val miaouVec = Vec(UInt(8 bits), 4)
    miaouVec := Vec(io.a,io.b,io.result,miaou)
  }

  class Sub extends Component {
    val io = new Bundle {
      val a,b = in UInt(8 bits)
      val result = out UInt(8 bits)
    }
    val subSubInst = new SubSub
    subSubInst.io.a <> io.a
    subSubInst.io.b <> io.b
    subSubInst.io.result <> io.result
  }

  class Dut extends Component {
    val io = new Bundle {
      val a,b = in UInt(8 bits)
      val result = out UInt(8 bits)
    }

    val subInst = new Sub
    subInst.io.a <> io.a
    subInst.io.b <> io.b
    subInst.io.result <> io.result
  }

}

class SpinalSimAccessSubComponents extends SpinalAnyFunSuite {
  SpinalSimTester { env =>
    import env._

    var compiled: SimCompiled[SpinalSimAccessSubComponents.Dut] = null
    test(prefix + "compile") {
      compiled = SimConfig.compile {
        val dut = new SpinalSimAccessSubComponents.Dut
        dut.subInst.subSubInst.miaouVec.simPublic()
        dut.subInst.subSubInst.io.a.simPublic()
        dut.subInst.subSubInst.miaou.simPublic()
        dut.subInst.subSubInst.miaou.pull()
        dut.subInst.subSubInst.io.pull()
        dut.subInst.subSubInst.miaouVec.pull()
        dut.subInst.subSubInst.miaouVec.pull()
        dut.subInst.subSubInst.miaouVec.pull()
        dut.subInst.subSubInst.miaouVec.pull()
        dut
      }
    }

    test(prefix + "simulate") {
      compiled.doSim { dut =>
        for (repeat <- 0 until 1000) {
          dut.io.a.randomize()
          dut.io.b.randomize()
          sleep(1)
          assert(dut.io.result.toInt == (dut.io.a.toInt ^ dut.io.b.toInt))
          assert(dut.subInst.subSubInst.miaou.toInt == (dut.io.a.toInt & dut.io.b.toInt))
          assert(dut.subInst.subSubInst.io.a.toInt == (dut.io.a.toInt))
          assert(dut.subInst.subSubInst.miaouVec(0).toInt == (dut.io.a.toInt))
          assert(dut.subInst.subSubInst.miaouVec(1).toInt == (dut.io.b.toInt))
          assert(dut.subInst.subSubInst.miaouVec(2).toInt == (dut.io.a.toInt ^ dut.io.b.toInt))
          assert(dut.subInst.subSubInst.miaouVec(3).toInt == (dut.io.a.toInt & dut.io.b.toInt))
        }
      }
    }
  }
}
