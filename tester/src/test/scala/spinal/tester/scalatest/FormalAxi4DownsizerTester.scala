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
    when(N === 0){
      out := B(0)
    }.elsewhen(conds.asBits =/= 0) {
      out := PriorityMux(conds.zip(datas).reverse)
    }.otherwise {
      out := that
    }
    out
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

        val rmInput = inputChecker.hist.io.outStreams(inputChecker.rmId)
        val rmOutput = outputChecker.hist.io.outStreams(outputChecker.rmId)

        val (cmdExist, cmdId) = inputChecker.hist.io.outStreams.sFindFirst(x => x.valid & x.axDone)
        val cmdInput = inputChecker.hist.io.outStreams(cmdId)
        val waitExist = cmdExist & inputChecker.rExist & cmdId =/= inputChecker.rId
        val waitId = CombInit(cmdId)
        val waitInput = inputChecker.hist.io.outStreams(cmdId)

        val (waitFirstOutExist, waitOutId) = outputChecker.hist.io.outStreams.sFindFirst(x => x.valid & x.axDone & !x.seenLast)
        val waitOutExist = waitFirstOutExist & (waitOutId =/= outputChecker.rId)

        val (undoneExist, undoneId) = inputChecker.hist.findFirst(x => x.valid & !x.axDone)
        val undoneInput = inputChecker.hist.io.outStreams(undoneId)
        val undoneInCount = inputChecker.hist.io.outStreams.sCount(x => x.valid & !x.axDone)

        val (undoneOutExist, undoneOutId) = outputChecker.hist.findFirst(x => x.valid & !x.axDone)
        val undoneOutput = outputChecker.hist.io.outStreams(undoneOutId)
        val undoneOutCount = outputChecker.hist.io.outStreams.sCount(x => x.valid & !x.axDone)

        val cmdCounter = dut.generator.cmdExtender.counter
        val cmdChecker = cmdCounter.withAsserts()
        val lenCounter = dut.dataOutCounter.counter
        val lenChecker = lenCounter.withAsserts()
        val ratioCounter = dut.dataCounter.counter
        val ratioChecker = ratioCounter.withAsserts()

        val ratio = Util.size2Ratio(cmdInput.size)
        val waitItemCount = CombInit(ratio.getZero)
        val rItemCount = CombInit(ratio.getZero)
//        val rDoneItemCount = CombInit(ratio.getZero)

        val transferred = (rInput.count << Util.size2Ratio(rInput.size)) + ratioCounter.io.value

        val validOutMask = outputChecker.hist.io.outStreams.map(x => x.valid).asBits()

        val rmOutExpect = U(0, 3 bits)
        when(inputChecker.rmExist) { rmOutExpect := Util.size2Ratio(rmInput.size) + 1}
        val rmOutMask = Util.getMostNOnesMsb(validOutMask, rmOutExpect)

        when(inputChecker.rmExist){
          assert(rmOutMask =/= 0)
          val firstOutOh = OHMasking.last(rmOutMask)
          val firstOut = OHMux(firstOutOh, outputChecker.hist.io.outStreams)
          assert(firstOut.size === Util.size2Outsize(rmInput.size))
          assert(firstOut.len === rmInput.len)
          when(rmOutExpect === 2){
            val secondOut = OHMux(OHMasking.first(rmOutMask), outputChecker.hist.io.outStreams)
            assert(secondOut.size === Util.size2Outsize(rmInput.size))
            assert(secondOut.len === rmInput.len)
          }
        }
        
        val rOutExpect = U(0, 3 bits)
        when(inputChecker.rExist) { rOutExpect := Util.size2Ratio(rInput.size) + 1}
        val rOutMask = Util.getMostNOnesMsb(validOutMask & ~rmOutMask, rOutExpect)

        when(inputChecker.rExist & (rOutMask =/= 0)){
          val firstOutOh = OHMasking.last(rOutMask)
          val firstOut = OHMux(firstOutOh, outputChecker.hist.io.outStreams)
          assert(firstOut.size === Util.size2Outsize(rInput.size))
          assert(firstOut.len === rInput.len)
          val secondOutOh = OHMasking.first(rOutMask)
          when(rOutExpect === 2 & (firstOutOh =/= secondOutOh)){
            val secondOut = OHMux(secondOutOh, outputChecker.hist.io.outStreams)
            assert(secondOut.size === Util.size2Outsize(rInput.size))
            assert(secondOut.len === rInput.len)
          }
        }
        
        val waitOutExpect = U(0, 3 bits)
        when(waitExist) { waitOutExpect := Util.size2Ratio(waitInput.size) + 1}
        val waitOutMask = Util.getMostNOnesMsb(validOutMask & ~rmOutMask & ~rOutMask, waitOutExpect)

        when(waitExist & (waitOutMask =/= 0)){
          val firstOutOh = OHMasking.last(waitOutMask)
          val firstOut = OHMux(firstOutOh, outputChecker.hist.io.outStreams)
          assert(firstOut.size === Util.size2Outsize(waitInput.size))
          assert(firstOut.len === waitInput.len)
          val secondOutOh = OHMasking.first(waitOutMask)
          when(waitOutExpect === 2 & (firstOutOh =/= secondOutOh)){
            val secondOut = OHMux(secondOutOh, outputChecker.hist.io.outStreams)
            assert(secondOut.size === Util.size2Outsize(waitInput.size))
            assert(secondOut.len === waitInput.len)
          }
        }

        assert(undoneInCount <= 1)
        when(undoneExist) { assert(input.ar.valid & input.ar.len === undoneInput.len & input.ar.size === undoneInput.size) }
        assert(undoneOutCount <= 1)
        when(undoneOutExist) { assert(output.ar.valid & output.ar.len === undoneOutput.len & output.ar.size === undoneOutput.size) }

//        when(!waitExist & !inputChecker.rExist) { assert(!undoneExist) }
        // when(waitExist) { waitItemCount := Util.size2Ratio(waitInput.size) + 1 }
//        when(cmdChecker.startedReg) { rDoneItemCount := 1 }
        // when(inputChecker.rExist) { rItemCount := Util.size2Ratio(rInput.size) + 1}
        // assert(countWaitingOutputs <= waitItemCount + rItemCount)

        // when(outputChecker.rExist && rInput.size === 3 && undoneOutExist) {
        //   when(waitOutExist) {
        //     assert(undoneOutId + 1 === waitOutId)
        //   }.otherwise{
        //     assert(undoneOutId + 1 === outputChecker.rId)
        //   }
        // }

        when(waitExist) {
          assert(waitInput.len === dut.countOutStream.len)
          assert(dut.countOutStream.size === Util.size2Outsize(waitInput.size))
          assert(dut.countOutStream.ratio === Util.size2Ratio(waitInput.size))

          assert(ratioCounter.expected === Util.size2Ratio(rInput.size))
          assert(lenCounter.expected === waitInput.len)
          assert(lenCounter.working & lenCounter.io.value === 0)

        }.elsewhen(inputChecker.rExist) {
          assert(rInput.len === dut.countOutStream.len)
          assert(dut.countOutStream.size === Util.size2Outsize(rInput.size))
          assert(dut.countOutStream.ratio === Util.size2Ratio(rInput.size))
          when(lenCounter.working){
            when(lenCounter.io.value > 0) { assert(rInput.count + 1 === lenCounter.io.value) }
              .otherwise { assert(rInput.count === 0) }
            when(ratioCounter.working) { assert(lenCounter.io.value > 0)}
              .otherwise { assert(lenCounter.io.value === 0) }
          }
        }.otherwise {
          assert(!lenCounter.working & !ratioCounter.working)
        }
        assert(undoneOutCount <= 1)

        when(waitExist) {
          assert(countWaitingInputs === 2)
          assert(lenCounter.working & ratioCounter.working)
          when(undoneExist) { assert(waitId === undoneId + 1) }
        }.otherwise {
          assert(countWaitingInputs < 2)
//          assert(undoneOutCount === 0)
        }

        assert(inputChecker.rExist === lenCounter.working | ratioCounter.working)

        rmOutput.ready := False
        val rmOutCount = outputChecker.hist.io.outStreams.sCount(x => x.valid && x.seenLast && x.axDone)
        when(inputChecker.rmExist) {
          assert(outputChecker.rmExist)
          when(inputChecker.rExist) { assert(rInput.count === 0) }
          when(outputChecker.rExist) { assert(rOutput.count === 0) }
          rmOutput.ready := True
          assert(rmOutput.len === rmInput.len)
          assert(rmOutput.size === Util.size2Outsize(rmInput.size))
          assert(rmOutCount === Util.size2Ratio(rmInput.size) + 1)

          when(rmOutCount > 1) {
            val preRm = OHMux(OHMasking.first(rmOutMask), outputChecker.hist.io.outStreams)
            assert(preRm.valid & preRm.axDone & preRm.seenLast)
            preRm.ready := True
            assert(preRm.len === rmInput.len)
            assert(preRm.size === Util.size2Outsize(rmInput.size))
            assert(rmInput.size === 3)
          }
        }.elsewhen(outputChecker.rmExist) {
          assert(rOutput.len === rmOutput.len)
          assert(rOutput.size === rmOutput.size)
          assert(rmOutput.size === 2)
          assert(rInput.size === 3)
//          val inTrans = (rInput.count) << Util.size2Ratio(rInput.size)
//          val outTrans = rOutput.count + rmOutput.count
//          assert(outTrans === inTrans + ratioCounter.counter.value)
          when(!waitExist) { assert(countWaitingOutputs === 1) }
        }

        assert(inputChecker.rExist === (outputChecker.rExist | output.ar.valid))

        val preRExist = CombInit(False)
        val postRExist = CombInit(False)
        val rOutMax = CombInit(rOutCount.getZero)
        val waitOutMax = CombInit(rOutCount.getZero)
        val rmOutMax = CombInit(rOutCount.getZero)
        when(inputChecker.rExist) {
          when(!output.ar.valid) {
            when(rInput.size === 3){
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
            assert(cmdCounter.expected === Util.size2Ratio(waitInput.size))
            assert(dut.generator.size === Util.size2Outsize(waitInput.size))

            val preRmCount = CombInit(ratio.getZero)
            when(inputChecker.rmExist) {
              preRmCount := Util.size2Ratio(rmInput.size) + 1
            }
            assert(rOutCount + rmOutCount === cmdCounter.io.value + Util.size2Ratio(rInput.size) + preRmCount + 1)
//            assert(cmdCounter.io.value === 0)
          }.otherwise {
            assert(cmdCounter.expected === Util.size2Ratio(rInput.size))
            assert(dut.generator.size === Util.size2Outsize(rInput.size))
            assert(rOutCount === cmdCounter.io.value)
            when(outputChecker.rExist) { assert(transferred === rOutput.count) }
          }

          when(inputChecker.rmExist) { rmOutMax := Util.size2Ratio(rmInput.size) + 1 }

          when(waitExist) {
            rOutMax := Util.size2Ratio(rInput.size) + 1
            when(!cmdCounter.working) { waitOutMax := Util.size2Ratio(waitInput.size) + 1 }
            assert(waitInput.len === lenCounter.expected)
          }.otherwise {
            when(!cmdCounter.working) { rOutMax := Util.size2Ratio(rInput.size) + 1 }
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
              // when(lenChecker.startedReg) { assert(lenCounter.io.value === rInput.count + 1) }
              when(rInput.count < rInput.len) {
                assert(lenCounter.io.value === rInput.count + 1)
              }
            }.otherwise {
              assert(rInput.count === rInput.len)
            }
          }
        }.otherwise {
          assert(countWaitingInputs === 0) // duplicated
        }

        when(preRExist) {
          val preRm = OHMux(OHMasking.first(rOutMask), outputChecker.hist.io.outStreams)
          assert(preRm.valid & preRm.axDone & !preRm.seenLast)
          assert(preRm.len === rInput.len)
          assert(preRm.size === Util.size2Outsize(rInput.size))
          assert(transferred === rOutput.count)
        }

        when(postRExist & !inputChecker.rmExist) {
          val postRm = OHMux(OHMasking.last(rOutMask), outputChecker.hist.io.outStreams)
          assert(postRm.valid & postRm.axDone & postRm.seenLast)
          assert(postRm.len === rInput.len)
          assert(postRm.size === Util.size2Outsize(rInput.size))

          assert(transferred === rOutput.count + rInput.len + 1)
        }

        when(outputChecker.rExist) {
          assert(rOutput.len === rInput.len)
          assert(rOutput.size === Util.size2Outsize(rInput.size))
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

        when(cmdCounter.io.working) { assert(cmdExist) }
        when(cmdChecker.started) {
          assert(cmdExist & (cmdCounter.expected === ratio))
        }

        when(lenChecker.started) {
//        when(lenCounter.io.working){
          assert(cmdExist & (lenCounter.expected === cmdInput.len))
//          when(lenCounter.counter.value > 0) {
//            assert(rInput.count + 1 === lenCounter.counter.value)
//          }
//            .elsewhen(ratioChecker.started) {
          //            assert(rInput.count === lenCounter.counter.value)
          //          }
        }
        when(inputChecker.rExist & !waitExist) {
          assert(lenCounter.expected === rInput.len)
        }

        when(ratioCounter.io.working) {
          val ratio = Util.size2Ratio(rInput.size)
          assert(inputChecker.rExist & (ratioCounter.expected === ratio))
        }

        when(lenChecker.startedReg) {
          assert(dut.countStream.payload === dut.countOutStream.payload)
        }
        when(lenCounter.working & dut.countOutStream.ratio > 0) { assert(dut.countOutStream.size === 2) }
        when(ratioCounter.working & dut.countStream.ratio > 0) { assert(dut.countStream.size === 2) }
        when(ratioCounter.working) { assert(dut.countStream.size === Util.size2Outsize(rInput.size)) }
        when(lenCounter.io.working) {
          assert(dut.countOutStream.size === dut.cmdStream.size)
          assert(dut.countOutStream.len === dut.cmdStream.len)
          assert(dut.countOutStream.ratio === cmdCounter.expected)
          when(waitExist) { assert(dut.lastLast) }
          .otherwise { assert(!dut.lastLast) }
        }.elsewhen(ratioCounter.io.working) {
          assert(dut.lastLast)
        }.otherwise{
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
        val cmdBoundAddress = cmdAddress + (((dut.generator.cmdExtendedStream.len +^ 1) << (dut.generator.size + cmdCounter.expected)) - 1).resized
        when(cmdCounter.working & cmdCounter.io.value === 0) { assert(cmdAddress(12, inConfig.addressWidth-12 bits) === cmdBoundAddress(12, inConfig.addressWidth-12 bits) ) }

        when(dut.io.output.r.fire) { assert(ratioCounter.io.working) }

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
