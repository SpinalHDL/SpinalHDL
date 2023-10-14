package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.pcie._
import spinal.lib._
import spinal.tester.code.TestTop
import spinal.lib.bus.pcie.sim.PcieCompleterMasterAgent
import spinal.lib.bus.misc.SizeMapping
import spinal.tester.PlayWithBug
import scala.util.Random

object TestTopLevel {
  class PcieSlaveFactoryDut extends Component {
    val rxReqConfig = TlpConfig(64/32, useData = true, useStrb = true)

    val io = new Bundle {
      val rxReq = slave(Stream Fragment(Tlp(rxReqConfig)))
      val txCpl = master(Stream Fragment(Tlp(rxReqConfig)))
    }
    val factory = PcieSlaveFactory(io.rxReq, io.txCpl)

    val kk = factory.createReadAndWrite(UInt(64 bits), 0x10000)
  }

  class Dummy extends Component {
    val io = new Bundle {
      val i1 = in UInt(16 bits)
      val o1 = out UInt(16 bits)
      val en = in(Bool)
    }

    val iReg = Reg(io.i1) init 0
    io.o1 := iReg
    when(io.en) {
      iReg := io.i1
    }
  }
}

class SpinalSimPcieSlaveFactoryTester extends SpinalAnyFunSuite {
  test("PcieSlaveFactory") {
    val compiled = SimConfig.allOptimisation.withFstWave.compile(rtl = new TestTopLevel.PcieSlaveFactoryDut)
    compiled.doSim("PcieSlaveFactory") { dut =>
      val cd = dut.clockDomain
      val agent = new PcieCompleterMasterAgent(dut.io.rxReq, dut.io.txCpl, cd) {

        override def mappingAllocate(mapping: SizeMapping): Boolean = {true}

        override def mappingFree(mapping: SizeMapping): Unit = {}
      }
      agent.reset()
      agent.allowGen = false
      cd.forkStimulus(10)
      cd.waitSampling(10)
      val transWr = agent.CompleterRequest(0x10000>>2, 2, 0xF, 0xF)
      while(!agent.applyWrite(transWr)) {cd.waitSampling()}

      val transRd = agent.CompleterRequest(0x10000>>2, 2, 0xF, 0xF)
      while(!agent.applyRead(transRd)) {cd.waitSampling()}
      cd.waitSampling(1000)
    }
  }
}

object SimAsynchronousExample {
  class Dut extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }
    io.result := io.a + io.b - io.c
  }

  def main(args: Array[String]): Unit = {
  }
}
