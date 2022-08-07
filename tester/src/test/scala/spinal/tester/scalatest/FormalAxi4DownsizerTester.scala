package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._
import spinal.lib.bus.amba4.axi._

object Util {
  def size2Ratio(size: UInt): UInt = {
    val out = cloneOf(size)
    when(size === 3) { out := U(1) }
      .otherwise { out := U(0) }
    out
  }

  def size2Outsize(size: UInt): UInt = {
    val out = cloneOf(size)
    when(size > 2) { out := U(2) }
      .otherwise { out := size }
    out
  }
}

class FormalAxi4DownsizerTester extends SpinalFormalFunSuite {
  def writeTester(inConfig: Axi4Config, outConfig: Axi4Config) {
    FormalConfig
      .withBMC(10)
      // .withProve(10)
      .withCover(10)
      .withDebug
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
        val inputChecker = input.formalContext(3, 4)
        inputChecker.withSlaveAsserts(maxStall)
        inputChecker.withSlaveAssumes(maxStall)
        val outputChecker = output.formalContext(6, 4)
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

        assume(!inputChecker.hist.io.willOverflow)
        assert(!outputChecker.hist.io.willOverflow)

        outputChecker.withCovers()
        inputChecker.withCovers()
      })
  }

  def readTester(inConfig: Axi4Config, outConfig: Axi4Config) {
    FormalConfig
      .withBMC(10)
      .withProve(10)
      .withCover(10)
      .withDebug
      .doVerify(new Component {
        val dut = FormalDut(new Axi4ReadOnlyDownsizer(inConfig, outConfig))
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val input = slave(Axi4ReadOnly(inConfig))
        dut.io.input << input

        val output = master(Axi4ReadOnly(outConfig))
        dut.io.output >> output

        val maxStall = 16
        val inputChecker = input.formalContext(3)
        inputChecker.withSlaveAsserts(maxStall)
        inputChecker.withSlaveAssumes(maxStall)
        val outputChecker = output.formalContext(5)
        outputChecker.withMasterAsserts(maxStall)
        outputChecker.withMasterAssumes(maxStall)

        outputChecker.hist.io.outStreams(outputChecker.rmId).ready := False
        when(inputChecker.rmExist) {
          assert(outputChecker.rmExist)
          val rmOut = outputChecker.hist.io.outStreams(outputChecker.rmId)
          rmOut.ready := True
          val rmIn = inputChecker.hist.io.outStreams(inputChecker.rmId)
          assert(rmOut.len === rmIn.len)
          assert(rmOut.size === Util.size2Outsize(rmIn.size))

          when(rmIn.size > 2) {
            val preRm = outputChecker.hist.io.outStreams(outputChecker.rmId - 1)
            assert(preRm.valid & preRm.axDone & preRm.seenLast)
            preRm.ready := True
            assert(preRm.len === rmIn.len)
            assert(preRm.size === Util.size2Outsize(rmIn.size))
          }
        }

        when(outputChecker.rExist) {
          assert(inputChecker.rExist)

        }

//        inputChecker.hist.io.outStreams.splitAt()

        assert(!inputChecker.hist.io.willOverflow)
        assert(!outputChecker.hist.io.willOverflow)

        val rInput = inputChecker.hist.io.outStreams(inputChecker.rId)
        val size = CombInit(input.ar.size.getZero)
        when(inputChecker.rExist) {
          size := rInput.size
        }

        val countWaitingInputs = inputChecker.hist.io.outStreams.sCount(x => x.valid && !x.seenLast && x.axDone)
        assert(countWaitingInputs <= 2)
        val countWaitingOutputs = outputChecker.hist.io.outStreams.sCount(x => x.valid && !x.seenLast && x.axDone)
        assert(countWaitingOutputs <= 4)

        val dataHist = History(output.r.data, 2, output.r.fire, init = output.r.data.getZero)
        val d1 = anyconst(Bits(outConfig.dataWidth bits))
        val d2 = anyconst(Bits(outConfig.dataWidth bits))
        assume(d1 =/= 0 & d1 =/= d2)
        assume(d2 =/= 0)

        val dataCheckSizeLess3 = (size < 3 & output.r.fire & output.r.data === d1)
        val dataCheckSize3 = (size === 3 & input.r.fire & input.r.data === (d2 ## d1))
        when(dataCheckSizeLess3) {
          val highRange = outConfig.dataWidth until 2 * outConfig.dataWidth
          val lowRange = 0 until outConfig.dataWidth
          assert(input.r.data(highRange) === d1 | input.r.data(lowRange) === d1)
        }.elsewhen(dataCheckSize3) {
          assert(dataHist(0) === d2 & dataHist(1) === d1)
        }
        cover(dataCheckSizeLess3)
        cover(dataCheckSize3)

        val (cmdExist, cmdId) = inputChecker.hist.io.outStreams.sFindFirst(x => x.valid & x.axDone)
        val cmdInput = inputChecker.hist.io.outStreams(cmdId)
        val waitExist = cmdExist & inputChecker.rExist & cmdId =/= inputChecker.rId
        val waitId = CombInit(cmdId)

        val cmdCounter = dut.generator.cmdExtender.counter
        val cmdChecker = cmdCounter.withAsserts()
        val ratio = Util.size2Ratio(cmdInput.size)
        when(cmdChecker.started) {
          assert(cmdExist & (cmdCounter.expected === ratio))
        }
        when(inputChecker.rExist) {
          assert(dut.dataOutCounter.io.working | dut.dataCounter.io.working)
        }

        val lenCounter = dut.dataOutCounter.counter
        val lenChecker = lenCounter.withAsserts()
        when(lenChecker.started) {
          assert(cmdExist & (lenCounter.expected === cmdInput.len))
//          when(lenCounter.counter.value > 0) {
//            assert(rInput.count + 1 === lenCounter.counter.value)
//          }
//            .elsewhen(ratioChecker.started) {
          //            assert(rInput.count === lenCounter.counter.value)
          //          }
        }

        val ratioCounter = dut.dataCounter.counter
        val ratioChecker = ratioCounter.withAsserts()
        when(ratioCounter.io.working) {
          val ratio = Util.size2Ratio(rInput.size)
          assert(inputChecker.rExist & (ratioCounter.expected === ratio))
        }

        when(lenChecker.startedReg) {
          assert(dut.countStream.payload === dut.countOutStream.payload)
        }
        when(dut.dataOutCounter.io.working & dut.countOutStream.ratio > 0) { assert(dut.countOutStream.size === 2) }
        when(dut.dataCounter.io.working & dut.countStream.ratio > 0) { assert(dut.countStream.size === 2) }

//        val (cmdOutExist, cmdOutId) = outputChecker.hist.io.outStreams.sFindFirst(x => x.valid & x.axDone)
//        val cmdOutput = outputChecker.hist.io.outStreams(cmdOutId)

        val selected = inputChecker.hist.io.outStreams(inputChecker.rmId)
        cover(inputChecker.rmExist)
        cover(inputChecker.rmExist && selected.size === 3)
        cover(inputChecker.rmExist && selected.size === 3 && selected.len === 1)
        outputChecker.withCovers()
        inputChecker.withCovers()
      })
  }
  val inConfig = Axi4Config(20, 64, 4, useBurst = false, useId = false, useLock = false)
  val outConfig = Axi4Config(20, 32, 4, useBurst = false, useId = false, useLock = false)

  // test("64_32_write") {
  //   writeTester(inConfig, outConfig)
  // }
  test("64_32_read") {
    readTester(inConfig, outConfig)
  }
}
