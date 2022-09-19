package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._
import spinal.lib.bus.amba4.axi._
import spinal.lib.HistoryModifyable

class FormalAxi4DownsizerTester extends SpinalFormalFunSuite {
  def writeTester(inConfig: Axi4Config, outConfig: Axi4Config) {
    FormalConfig
      .withBMC(10)
      // .withProve(10)
      .withCover(10)
      .withOutWireReduce
      .doVerify(new Component {
        val dut = FormalDut(new Axi4WriteOnlyDownsizer(inConfig, outConfig))
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val inData = anyconst(Bits(inConfig.dataWidth bits))

        val input = slave(Axi4WriteOnly(inConfig))
        dut.io.input << input
        val inHist = new HistoryModifyable(cloneOf(input.aw.size), 4)
        inHist.init()
        inHist.io.input.valid := input.w.fire & input.w.data === inData
        inHist.io.input.payload := 0

        val output = master(Axi4WriteOnly(outConfig))
        dut.io.output >> output
        val outHist = new HistoryModifyable(Bits(outConfig.dataWidth bits), 2)
        outHist.init()
        outHist.io.input.valid := output.w.fire
        outHist.io.input.payload := output.w.data

        val highRange = outConfig.dataWidth until 2 * outConfig.dataWidth
        val lowRange = 0 until outConfig.dataWidth
        val d1 = inData(lowRange)
        val d2 = inData(highRange)
        assume(d1 =/= d2)
        assume(input.w.data(highRange) =/= d1)
        assume(input.w.data(lowRange) =/= d2)
        when(input.w.data(lowRange) === d1) { assume(input.w.data(highRange) === d2) }
        when(input.w.data(highRange) === d2) { assume(input.w.data(lowRange) === d1) }

        val maxStall = 16
        val inputChecker = input.formalContext(4, 4)
        inputChecker.withSlaveAsserts(maxStall)
        inputChecker.withSlaveAssumes(maxStall)
        val outputChecker = output.formalContext(4, 4)
        outputChecker.withMasterAsserts(maxStall)
        outputChecker.withMasterAssumes(maxStall)

        when(inHist.io.input.valid) {
          val inputSelected = inputChecker.hist.io.outStreams(inputChecker.wId)
          when(inputChecker.wExist & inputSelected.payload.axDone) {
            inHist.io.input.payload := inputSelected.size
          }.otherwise { inHist.io.input.payload := input.aw.size }
        }

        val (inValid, inId) = inHist.io.outStreams.sFindFirst(_.valid)
        val inSelected = inHist.io.outStreams(inId)
        when(inValid & output.w.fire) {
          val outSelected = outputChecker.hist.io.outStreams(outputChecker.wId)
          when(output.w.fire) {
            when(outputChecker.wExist & inSelected.payload === 3) {
              when(output.w.data === d2) {
                val (d1Exist, d1Id) = outHist.io.outStreams.sFindFirst(x => x.valid & x.payload === d1)
                assert(d1Exist)
                assert(d1Id === 0)
                inSelected.ready := True
              }
            }
          }.elsewhen(inSelected.payload < 3) {
            assert(output.w.data === d1 | output.w.data === d2)
          }
        }

        outputChecker.withCovers()
        inputChecker.withCovers()
      })
  }

  def readTester(inConfig: Axi4Config, outConfig: Axi4Config) {
    FormalConfig
      .withBMC(10)
      // .withProve(10)
      .withCover(10)
      .withOutWireReduce
      .doVerify(new Component {
        val dut = FormalDut(new Axi4ReadOnlyDownsizer(inConfig, outConfig))
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val input = slave(Axi4ReadOnly(inConfig))
        dut.io.input << input

        val output = master(Axi4ReadOnly(outConfig))
        dut.io.output >> output

        val maxStall = 16
        val inputChecker = input.formalContext(4)
        inputChecker.withSlaveAsserts(maxStall)
        inputChecker.withSlaveAssumes(maxStall)
        val outputChecker = output.formalContext(4)
        outputChecker.withMasterAsserts(maxStall)
        outputChecker.withMasterAssumes(maxStall)

        outputChecker.withCovers()
        inputChecker.withCovers()
      })
  }

  val inConfig = Axi4Config(20, 64, 4, useBurst = false, useId = false)
  val outConfig = Axi4Config(20, 32, 4, useBurst = false, useId = false)
  test("64_32_write") {
    writeTester(inConfig, outConfig)
  }
  test("64_32_read") {
    readTester(inConfig, outConfig)
  }
}
