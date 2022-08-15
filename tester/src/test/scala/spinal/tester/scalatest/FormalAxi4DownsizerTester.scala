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
      .withOutWireReduce
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
        inputChecker.withSlaveAsserts()
        inputChecker.withSlaveAssumes(2)
        val outputChecker = output.formalContext(5)
        outputChecker.withMasterAsserts(maxStall)
        outputChecker.withMasterAssumes(maxStall)

        val countWaitingInputs = inputChecker.hist.io.outStreams.sCount(x => x.valid && !x.seenLast && x.axDone)
        assert(countWaitingInputs <= 2)
        val countWaitingOutputs = outputChecker.hist.io.outStreams.sCount(x => x.valid && !x.seenLast && x.axDone)
        assert(countWaitingOutputs <= 4)

        val rInput = inputChecker.hist.io.outStreams(inputChecker.rId)
        val rOutput = outputChecker.hist.io.outStreams(outputChecker.rId)
        val rOutCount = outputChecker.hist.io.outStreams.sCount(x => x.valid & x.axDone & !x.seenLast)
        val rOutsize = Util.size2Outsize(rInput.size)
        val rRatio = Util.size2Ratio(rInput.size)

        val rmInput = inputChecker.hist.io.outStreams(inputChecker.rmId)
        val rmOutput = outputChecker.hist.io.outStreams(outputChecker.rmId)
        val rmOutsize = Util.size2Outsize(rmInput.size)
        val rmRatio = Util.size2Ratio(rmInput.size)

        val (doneExist, doneId) = inputChecker.hist.io.outStreams.sFindFirst(x => x.valid & x.axDone)
        val waitExist = doneExist & inputChecker.rExist & doneId =/= inputChecker.rId
        val waitId = CombInit(doneId)
        val waitInput = inputChecker.hist.io.outStreams(doneId)
        val waitOutsize = Util.size2Outsize(waitInput.size)
        val waitRatio = Util.size2Ratio(waitInput.size)

        val cmdCounter = dut.generator.cmdExtender.counter
        val cmdChecker = cmdCounter.withAsserts()
        val lenCounter = dut.dataOutCounter.counter
        val lenChecker = lenCounter.withAsserts()
        val ratioCounter = dut.dataCounter.counter
        val ratioChecker = ratioCounter.withAsserts()

        val transferred = (rInput.count << rRatio) + ratioCounter.io.value

        assert(inputChecker.rExist === (lenCounter.working | ratioCounter.working))
        assert(inputChecker.rExist === (outputChecker.rExist | output.ar.valid))

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

        when(waitExist) {
          assert(dut.countOutStream.len === waitInput.len)
          assert(dut.countOutStream.size === waitOutsize)
          assert(dut.countOutStream.ratio === waitRatio)

          assert(ratioCounter.expected === rRatio)
          assert(lenCounter.expected === waitInput.len)
          assert(lenCounter.working & lenCounter.io.value === 0)
          assert(lenCounter.working & ratioCounter.working)
        }.elsewhen(inputChecker.rExist) {
          assert(dut.countOutStream.len === rInput.len)
          assert(dut.countOutStream.size === rOutsize)
          assert(dut.countOutStream.ratio === rRatio)
          when(lenCounter.working) {
            when(lenCounter.io.value > 0) { assert(rInput.count + 1 === lenCounter.io.value) }
              .otherwise { assert(rInput.count === 0) }
            when(ratioCounter.working) { assert(lenCounter.io.value > 0) }
              .otherwise { assert(lenCounter.io.value === 0) }
          }
        }.otherwise {
          assert(!lenCounter.working & !ratioCounter.working)
        }

        rmOutput.ready := False
        val rmOutCount = outputChecker.hist.io.outStreams.sCount(x => x.valid && x.seenLast && x.axDone)
        when(inputChecker.rmExist) {
          assert(outputChecker.rmExist)
          when(inputChecker.rExist) { assert(rInput.count === 0) }
          when(outputChecker.rExist) { assert(rOutput.count === 0) }
          rmOutput.ready := True
          assert(rmOutCount === rmRatio + 1)

          when(rmOutCount > 1) {
            val preRm = OHMux(OHMasking.first(rmOutMask), outputChecker.hist.io.outStreams)
            assert(preRm.valid & preRm.axDone & preRm.seenLast)
            preRm.ready := True
          }
        }

        val preRExist = CombInit(False)
        val postRExist = CombInit(False)
        val rOutMax = CombInit(rOutCount.getZero)
        val waitOutMax = CombInit(rOutCount.getZero)
        val rmOutMax = CombInit(rOutCount.getZero)
        when(inputChecker.rExist) {
          when(!output.ar.valid) {
            when(rInput.size === 3) {
              when(!outputChecker.rmExist) {
                preRExist := True
              }.otherwise {
                postRExist := True
              }
            }

            when(lenCounter.working) {
              when(lenChecker.startedReg) { assert(lenCounter.io.value === rInput.count + 1) }
            }.otherwise {
              assert(rInput.count === rInput.len)
            }
          }.elsewhen(waitExist) {
            assert(cmdCounter.expected === waitRatio)
            assert(dut.generator.size === waitOutsize)

            val preRmCount = CombInit(rRatio.getZero)
            when(inputChecker.rmExist) {
              preRmCount := rmRatio + 1
            }
            assert(rOutCount + rmOutCount === cmdCounter.io.value + rRatio + preRmCount + 1)
          }.otherwise {
            assert(cmdCounter.expected === rRatio)
            assert(dut.generator.size === rOutsize)
            assert(rOutCount === cmdCounter.io.value)
            when(outputChecker.rExist) { assert(transferred === rOutput.count) }
          }

          when(inputChecker.rmExist) { rmOutMax := rmRatio + 1 }

          when(waitExist) {
            rOutMax := rRatio + 1
            when(!cmdCounter.working) { waitOutMax := waitRatio + 1 }
            assert(waitInput.len === lenCounter.expected)
          }.otherwise {
            when(!cmdCounter.working) { rOutMax := rRatio + 1 }
            assert(rInput.len === lenCounter.expected)
          }
          assert(rOutCount + rmOutCount === cmdCounter.io.value + rOutMax + waitOutMax + rmOutMax)

          when(outputChecker.rExist) {
            when(outputChecker.rmExist & !inputChecker.rmExist) {
              assert(transferred === rOutput.count + rInput.len + 1)
            }.otherwise {
              assert(transferred === rOutput.count)
            }

            when(lenCounter.working) {
              when(rInput.count < rInput.len) {
                assert(lenCounter.io.value === rInput.count + 1)
              }
            }.otherwise {
              assert(rInput.count === rInput.len)
            }

            assert((rOutMask.asUInt & (U(1) << outputChecker.rId).resized) =/= 0)
          }
        }.otherwise {
          assert(countWaitingInputs === 0) // duplicated
        }

        when(preRExist) {
          val preRm = OHMux(OHMasking.first(rOutMask), outputChecker.hist.io.outStreams)
          assert(preRm.valid & preRm.axDone & !preRm.seenLast)
          assert(preRm.len === rInput.len)
          assert(preRm.size === rOutsize)
          assert(transferred === rOutput.count)
        }

        when(postRExist & !inputChecker.rmExist) {
          val postRm = OHMux(OHMasking.last(rOutMask), outputChecker.hist.io.outStreams)
          assert(postRm.valid & postRm.axDone & postRm.seenLast)
          assert(postRm.len === rInput.len)
          assert(postRm.size === rOutsize)

          assert(transferred === rOutput.count + rInput.len + 1)
        }

        when(outputChecker.rExist) {
          assert(rOutput.len === rInput.len)
          assert(rOutput.size === rOutsize)
          when(rInput.size < 3) { assert(rOutput.count === rInput.count) }
        }

        when(inputChecker.rExist) {
          when(!outputChecker.rExist) { assert(rInput.count === 0) }
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
//        when(size === 3 & past(dut.dataReg(highRange) === d1)) { assert( dataHist(1) === d1) }
        cover(dataCheckSizeLess3)
        cover(dataCheckSize3)

        when(inputChecker.rExist & !waitExist) {
          assert(lenCounter.expected === rInput.len)
        }

        when(ratioCounter.io.working) {
          val ratio = rRatio
          assert(inputChecker.rExist & (ratioCounter.expected === ratio))
        }

        when(lenChecker.startedReg) {
          assert(dut.countStream.payload === dut.countOutStream.payload)
        }
        when(lenCounter.working & dut.countOutStream.ratio > 0) { assert(dut.countOutStream.size === 2) }
        when(ratioCounter.working & dut.countStream.ratio > 0) { assert(dut.countStream.size === 2) }
        when(ratioCounter.working) { assert(dut.countStream.size === rOutsize) }
        when(lenCounter.io.working) {
          assert(dut.countOutStream.size === dut.cmdStream.size)
          assert(dut.countOutStream.len === dut.cmdStream.len)
          assert(dut.countOutStream.ratio === cmdCounter.expected)
          when(waitExist) { assert(dut.lastLast) }
            .otherwise { assert(!dut.lastLast) }
        }.elsewhen(ratioCounter.io.working) {
          assert(dut.lastLast)
        }.otherwise {
          assert(!dut.lastLast)
        }
        when(inputChecker.rExist & rInput.size === 3 & ratioCounter.working) {
          assert(dut.offset === ratioCounter.io.value << 2)
          when(transferred > 0 || rOutput.count > 0) {
            assert(dut.dataReg(U(outConfig.dataWidth) - (dut.offset << 3), outConfig.dataWidth bits) === dataHist(1))
          }
        }
        val addrWidth = dut.countOutStream.size + dut.countOutStream.ratio
        val addrMask = (U(1) << addrWidth - 1)
        when(lenCounter.working & addrWidth === 3) { assert((dut.countOutStream.start & addrMask.resized) === 0) }

        val cmdAddress = dut.generator.address
        val cmdBoundAddress =
          cmdAddress + (((dut.generator.cmdExtendedStream.len +^ 1) << (dut.generator.size + cmdCounter.expected)) - 1).resized
        when(cmdCounter.working & cmdCounter.io.value === 0) {
          assert(
            cmdAddress(12, inConfig.addressWidth - 12 bits) === cmdBoundAddress(12, inConfig.addressWidth - 12 bits)
          )
        }

        when(dut.io.output.r.fire) { assert(ratioCounter.io.working) }

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
