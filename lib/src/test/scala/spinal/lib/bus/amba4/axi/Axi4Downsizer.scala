package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

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

  def getMostNOnesMsb(that: Bits, N: UInt): Bits = {
    val width = that.getWidth
    val index = (1 until width)
    val datas = index.map(x => that(x until width).resizeLeft(width))

    val slices = index.map(x => that(x until width).resize(width bits))
    val conds = slices.map(CountOne(_) >= N)
    val out = cloneOf(that)
    when(N === 0) {
      out := B(0)
    }.elsewhen(conds.asBits =/= 0) {
      out := PriorityMux(conds.zip(datas).reverse)
    }.otherwise {
      out := that
    }
    out
  }

  def constraintByMask(
      enable: Bool,
      mask: Bits,
      streams: Vec[Stream[FormalAxi4Record]],
      target: FormalAxi4Record
  ) = new Area {
    val expected = U(0, 3 bits)
    when(enable) { expected := Util.size2Ratio(target.size) + 1 }
    val outMask = getMostNOnesMsb(mask, expected)

    when(enable & (outMask =/= 0)) {
      val firstOutOh = OHMasking.last(outMask)
      val firstOut = OHMux(firstOutOh, streams)
      assert(firstOut.size === size2Outsize(target.size))
      assert(firstOut.len === target.len)
      val secondOutOh = OHMasking.first(outMask)
      when(expected === 2 & (firstOutOh =/= secondOutOh)) {
        val secondOut = OHMux(secondOutOh, streams)
        assert(secondOut.size === size2Outsize(target.size))
        assert(secondOut.len === target.len)
      }
    }
  }
}

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
        val inputChecker = input.formalContext(3, 4)
        inputChecker.formalAssertsSlave(maxStall)
        inputChecker.formalAssumesSlave(maxStall)
        val outputChecker = output.formalContext(6, 4)
        outputChecker.formalAssertsMaster(maxStall)
        outputChecker.formalAssumesMaster(maxStall)

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

        outputChecker.formalCovers()
        inputChecker.formalCovers()
      })
  }

  def readTester(inConfig: Axi4Config, outConfig: Axi4Config) {
    FormalConfig
      .withBMC(10)
      .withProve(10)
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
        val inputChecker = input.formalContext(3)
        inputChecker.formalAssertsSlave()
        inputChecker.formalAssumesSlave(2)
        val outputChecker = output.formalContext(5)
        outputChecker.formalAssertsMaster(maxStall)
        outputChecker.formalAssumesMaster(maxStall)

        val countWaitingInputs = inputChecker.hist.io.outStreams.sCount(x => x.valid && !x.seenLast && x.axDone)
        assert(countWaitingInputs <= 2)

        val rInput = inputChecker.hist.io.outStreams(inputChecker.rId)
        val rOutput = outputChecker.hist.io.outStreams(outputChecker.rId)
        val rOutCount = outputChecker.hist.io.outStreams.sCount(x => x.valid & x.axDone & !x.seenLast)
        val rOutsize = Util.size2Outsize(rInput.size)
        val rRatio = Util.size2Ratio(rInput.size)

        val rmInput = inputChecker.hist.io.outStreams(inputChecker.rmId)
        val rmOutput = outputChecker.hist.io.outStreams(outputChecker.rmId)
        val rmRatio = Util.size2Ratio(rmInput.size)

        val (doneExist, doneId) = inputChecker.hist.io.outStreams.sFindFirst(x => x.valid & x.axDone)
        val waitExist = doneExist & inputChecker.rExist & doneId =/= inputChecker.rId
        val waitId = CombInit(doneId)
        val waitInput = inputChecker.hist.io.outStreams(doneId)
        val waitOutsize = Util.size2Outsize(waitInput.size)
        val waitRatio = Util.size2Ratio(waitInput.size)

        val cmdCounter = dut.generator.cmdExtender.counter
        val lenCounter = dut.dataOutCounter.counter
        val ratioCounter = dut.dataCounter.counter
        val dutChecker = dut.formalAsserts()

        val transferred = (rInput.count << rRatio) + ratioCounter.io.value

        assert(inputChecker.rExist === (lenCounter.working | ratioCounter.working))
        assert(inputChecker.rExist === (outputChecker.rExist | cmdCounter.working))

        // constraints on input history.
        when(waitExist) {
          assert(countWaitingInputs === 2)
        }.otherwise {
          assert(countWaitingInputs < 2)
        }

        // sync on input and output history.
        val validOutMask = outputChecker.hist.io.outStreams.map(x => x.valid).asBits()
        val rmOutLogic = Util.constraintByMask(
          inputChecker.rmExist,
          validOutMask,
          outputChecker.hist.io.outStreams,
          rmInput
        )
        val rmOutExpect = rmOutLogic.expected
        val rmOutMask = rmOutLogic.outMask

        val rOutLogic = Util.constraintByMask(
          inputChecker.rExist,
          validOutMask & ~rmOutMask,
          outputChecker.hist.io.outStreams,
          rInput
        )
        val rOutExpect = rOutLogic.expected
        val rOutMask = rOutLogic.outMask

        val waitOutLogic = Util.constraintByMask(
          waitExist,
          validOutMask & ~rmOutMask & ~rOutMask,
          outputChecker.hist.io.outStreams,
          waitInput
        )
        val waitOutExpect = waitOutLogic.expected
        val waitOutMask = waitOutLogic.outMask

        when(outputChecker.rExist) {
          assert((rOutMask.asUInt & (U(1) << outputChecker.rId).resized) =/= 0)
        }
        when(outputChecker.rmExist) {
          when(inputChecker.rmExist) {
            assert(OHMasking.last(rmOutMask).asUInt === (U(1) << outputChecker.rmId))
          }.otherwise {
            assert(OHMasking.last(rOutMask).asUInt === (U(1) << outputChecker.rmId))
          }
        }

        // constraints on dut internal streams.
        when(inputChecker.rExist) {
          when(waitExist) {
            assert(dut.countOutStream.len === waitInput.len)
            assert(dut.countOutStream.size === waitOutsize)
            assert(dut.countOutStream.ratio === waitRatio)
          }.otherwise {
            assert(dut.countOutStream.len === rInput.len)
            assert(dut.countOutStream.size === rOutsize)
            assert(dut.countOutStream.ratio === rRatio)
          }
        }

        // constraints on dut internal counter working status.
        when(inputChecker.rExist) {
          when(waitExist) {
            assert(cmdCounter.expected === waitRatio)
            assert(dut.generator.size === waitOutsize)

            assert(lenCounter.expected === waitInput.len)
            assert(lenCounter.working & ratioCounter.working)
          }.otherwise {
            when(cmdCounter.working) {
              assert(cmdCounter.expected === rRatio)
              assert(dut.generator.size === rOutsize)
            }
            assert(lenCounter.expected === rInput.len)
          }

          when(ratioCounter.working) {
            assert(ratioCounter.expected === rRatio)
            assert(dut.countStream.size === rOutsize)
          }
        }.otherwise {
          assert(!lenCounter.working & !ratioCounter.working)
        }

        // check items to be removed.
        rmOutput.ready := False
        val rmOutCount = outputChecker.hist.io.outStreams.sCount(x => x.valid && x.seenLast && x.axDone)
        when(inputChecker.rmExist) {
          assert(outputChecker.rmExist)
          when(inputChecker.rExist) { assert(rInput.count === 0) }
          when(outputChecker.rExist) { assert(rOutput.count === 0) }
          assert(rmOutCount === rmRatio + 1)
          rmOutput.ready := True

          when(rmOutCount > 1) {
            val preRm = OHMux(OHMasking.first(rmOutMask), outputChecker.hist.io.outStreams)
            assert(preRm.valid & preRm.axDone & preRm.seenLast)
            preRm.ready := True
          }
        }

        // check len counter value.
        when(inputChecker.rExist) {
          when(waitExist) {
            assert(lenCounter.io.value === 0)
          }.otherwise {
            when(lenCounter.working) {
              when(lenCounter.io.value > 0) { assert(lenCounter.io.value === rInput.count + 1) }
              when(ratioCounter.working) { assert(lenCounter.io.value > 0) }
                .otherwise { assert(lenCounter.io.value === 0) }
            }
          }
        }

        // check processing input counter value.
        when(inputChecker.rExist) {
          when(outputChecker.rExist) {
            when(lenCounter.working) {
              when(rInput.count < rInput.len) {
                assert(rInput.count + 1 === lenCounter.io.value)
              }
            }.otherwise {
              assert(rInput.count === rInput.len)
            }
          }.otherwise {
            assert(rInput.count === 0)
          }
        }

        // check output items vs cmd counter.
        when(inputChecker.rExist) {
          val rOutMax = CombInit(rOutCount.getZero)
          val waitOutMax = CombInit(rOutCount.getZero)
          when(waitExist) {
            rOutMax := rOutExpect
            when(!cmdCounter.working) { waitOutMax := waitOutExpect }
          }.otherwise {
            when(!cmdCounter.working) { rOutMax := rOutExpect }
          }
          assert(rOutCount + rmOutCount === cmdCounter.io.value + rOutMax + waitOutMax + rmOutExpect)
        }

        // constraints on processing output's count.
        when(outputChecker.rExist) {
          when(outputChecker.rmExist & !inputChecker.rmExist) {
            assert(transferred === rOutput.count + rInput.len + 1)
          }.otherwise {
            assert(transferred === rOutput.count)
          }
        }

        // check items processing with max size.
        when(inputChecker.rExist & !cmdCounter.working & rInput.size === 3) {
          when(!outputChecker.rmExist) {
            val preRm = OHMux(OHMasking.first(rOutMask), outputChecker.hist.io.outStreams)
            assert(preRm.valid & preRm.axDone & !preRm.seenLast)
          }.elsewhen(!inputChecker.rmExist) {
            val postRm = OHMux(OHMasking.last(rOutMask), outputChecker.hist.io.outStreams)
            assert(postRm.valid & postRm.axDone & postRm.seenLast)
          }
        }

        // align lastLast signal for all cases.
        when(lenCounter.working) {
          when(waitExist) { assert(dut.lastLast) }
            .otherwise { assert(!dut.lastLast) }
        }.elsewhen(ratioCounter.io.working) {
          assert(dut.lastLast)
        }.otherwise {
          assert(!dut.lastLast)
        }

        assert(!inputChecker.hist.io.willOverflow)
        assert(!outputChecker.hist.io.willOverflow)

        val size = CombInit(input.ar.size.getZero)
        when(inputChecker.rExist) {
          size := rInput.size
        }

        val dataHist = History(output.r.data, 2, output.r.fire, init = output.r.data.getZero)
        val d1 = anyconst(Bits(outConfig.dataWidth bits))
        val d2 = anyconst(Bits(outConfig.dataWidth bits))
        assume(d1 =/= 0 & d1 =/= d2)
        assume(d2 =/= 0)

        val dataCheckSizeLess3 = (size < 3 & output.r.fire & output.r.data === d1)
        val dataCheckSize3 = (size === 3 & input.r.fire & input.r.data === (d2 ## d1))
        val highRange = outConfig.dataWidth until 2 * outConfig.dataWidth
        val lowRange = 0 until outConfig.dataWidth
        when(dataCheckSizeLess3) {
          assert(input.r.data(highRange) === d1 | input.r.data(lowRange) === d1)
        }.elsewhen(dataCheckSize3) {
          assert(dataHist(0) === d2 & dataHist(1) === d1)
        }
        cover(dataCheckSizeLess3)
        cover(dataCheckSize3)

        // constraints on data reg for multiple out transaction.
        when(inputChecker.rExist & rInput.size === 3 & ratioCounter.working) {
          assert(dut.offset === ratioCounter.io.value << 2)
          when(transferred > 0 || rOutput.count > 0) {
            assert(dut.dataReg(U(outConfig.dataWidth) - (dut.offset << 3), outConfig.dataWidth bits) === dataHist(1))
          }
        }

        val selected = inputChecker.hist.io.outStreams(inputChecker.rmId)
        cover(inputChecker.rmExist)
        cover(inputChecker.rmExist && selected.size === 3)
        cover(inputChecker.rmExist && selected.size === 3 && selected.len === 1)
        outputChecker.formalCovers()
        inputChecker.formalCovers()
      })
  }
  val inConfig = Axi4Config(20, 64, 4, useBurst = false, useId = false, useLock = false)
  val outConfig = Axi4Config(20, 32, 4, useBurst = false, useId = false, useLock = false)

  test("64_32_write") {
    writeTester(inConfig, outConfig)
  }

  test("64_32_read") {
    readTester(inConfig, outConfig)
  }
}
