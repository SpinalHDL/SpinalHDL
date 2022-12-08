package spinal.lib

import spinal.core._
import spinal.core.formal._
import spinal.lib.formal._

import spinal.tester.{SpinalTesterCocotbBase, SpinalTesterGhdlBase}

object StreamTester{
  case class BundleA(aaa : Int) extends Bundle{
    val a = UInt(8 bit)
    val b = Bool()
  }
}

import StreamTester._

class StreamTester extends Component {
  val io = new Bundle {
    val slave0 = slave Stream new BundleA(8)
    val master0 = master Stream new BundleA(8)
    val fifo0_occupancy = out UInt()
  }

  val fifo0 = new StreamFifo(new BundleA(8),16)
  fifo0.io.push << io.slave0
  fifo0.io.pop >/-> io.master0
  io.fifo0_occupancy := fifo0.io.occupancy

  assert(3 == LatencyAnalysis(io.slave0.a,io.master0.a))
  assert(2 == LatencyAnalysis(io.master0.ready,io.slave0.ready))



  val forkInput = slave Stream(Bits(8 bits))
  val forkOutputs = Vec(master Stream(Bits(8 bits)),3)
  (forkOutputs , StreamFork(forkInput,3)).zipped.foreach(_ << _)

  val dispatcherInOrderInput = slave Stream(Bits(8 bits))
  val dispatcherInOrderOutputs = Vec(master Stream(Bits(8 bits)),3)
  (dispatcherInOrderOutputs , StreamDispatcherSequencial(dispatcherInOrderInput,3)).zipped.foreach(_ << _)

  val streamFlowArbiterInputStream = slave Stream(Bits(8 bits))
  val streamFlowArbiterInputFlow = slave Flow(Bits(8 bits))
  val streamFlowArbiterOutput = master Flow(Bits(8 bits))
  streamFlowArbiterOutput << StreamFlowArbiter(streamFlowArbiterInputStream,streamFlowArbiterInputFlow)

  val arbiterInOrderInputs =  Vec(slave Stream(Bits(8 bits)),3)
  val arbiterInOrderOutput =  master Stream(Bits(8 bits))
  arbiterInOrderOutput << StreamArbiterFactory.sequentialOrder.on(arbiterInOrderInputs)

  val arbiterLowIdPortFirstInputs =  Vec(slave Stream(Bits(8 bits)),3)
  val arbiterLowIdPortFirstOutput =  master Stream(Bits(8 bits))
  arbiterLowIdPortFirstOutput << StreamArbiterFactory.lowerFirst.on(arbiterLowIdPortFirstInputs)

  val arbiterRoundRobinInputs =  Vec(slave Stream(Bits(8 bits)),3)
  val arbiterRoundRobinOutput =  master Stream(Bits(8 bits))
  arbiterRoundRobinOutput << StreamArbiterFactory.roundRobin.on(arbiterRoundRobinInputs)


  val arbiterLowIdPortFirstNoLockInputs =  Vec(slave Stream(Bits(8 bits)),3)
  val arbiterLowIdPortFirstNoLockOutput =  master Stream(Bits(8 bits))
  arbiterLowIdPortFirstNoLockOutput << StreamArbiterFactory.lowerFirst.noLock.on(arbiterLowIdPortFirstNoLockInputs)

  val arbiterLowIdPortFirstFragmentLockInputs =  Vec(slave Stream(Fragment(Bits(8 bits))),3)
  val arbiterLowIdPortFirstFragmentLockOutput =  master Stream(Fragment(Bits(8 bits)))
  arbiterLowIdPortFirstFragmentLockOutput << StreamArbiterFactory.lowerFirst.fragmentLock.on(arbiterLowIdPortFirstFragmentLockInputs)


  //  val muxSelect = in UInt(2 bits)
//  val muxInputs = Vec(slave Stream(Bits(8 bits)),3)
//  val muxOutput = master Stream(Bits(8 bits))
//  muxOutput << StreamMux(muxSelect,muxInputs)

//  val joinInputs = Vec(slave Stream(Bits(8 bits)),3)
//  val joinOutput = master.Event
//  joinOutput << StreamJoin(joinInputs)
}



class StreamTesterGhdlBoot extends SpinalTesterGhdlBase {
  override def getName: String = "StreamTester"
  override def createToplevel: Component = new StreamTester
}

class StreamTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "StreamTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/StreamTester"
  override def createToplevel: Component = new StreamTester
  override def noVhdl = true
}

object StreamTester2 {
  case class BundleA() extends Bundle{
    val a = UInt(8 bit)
    val b = Bool()
  }

  class StreamTester2 extends Component{
    val fifoA = new StreamFifo(BundleA(),16)
    val fifoAPush = slave(cloneOf(fifoA.io.push))
    val fifoAPop = master(cloneOf(fifoA.io.pop))
    val fifoAOccupancy = out(cloneOf(fifoA.io.occupancy))
    fifoA.io.push << fifoAPush
    fifoA.io.pop >> fifoAPop
    fifoA.io.occupancy <> fifoAOccupancy
    assert(2 == LatencyAnalysis(fifoAPush.a,fifoAPop.a))
    assert(1 == LatencyAnalysis(fifoAPop.ready,fifoAPush.ready))


    val fifoB = new StreamFifoLowLatency(BundleA(),16)
    val fifoBPush = slave(cloneOf(fifoB.io.push))
    val fifoBPop = master(cloneOf(fifoB.io.pop))
    val fifoBOccupancy = out(cloneOf(fifoB.io.occupancy))
    fifoB.io.push << fifoBPush
    fifoB.io.pop >> fifoBPop
    fifoB.io.occupancy <> fifoBOccupancy
    assert(0 == LatencyAnalysis(fifoBPush.a,fifoBPop.a))
    assert(1 == LatencyAnalysis(fifoBPop.ready,fifoBPush.ready))
  }
}

class StreamTester2CocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "StreamTester2"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/StreamTester2"
  override def createToplevel: Component = new StreamTester2.StreamTester2
  override def backendConfig(config: SpinalConfig): SpinalConfig = config
}

class FormalArbiterTester extends SpinalFormalFunSuite {
  def usualVerify[T <: Data](
      inputs: Vec[Stream[T]],
      output: Stream[T],
      portCount: Int,
      select: UInt,
      selectOH: Bits,
      reset: Bool,
      withLock: Boolean,
      locked: Bool
  ) = new Area {
    output.formalCovers(2)
    when(reset || past(reset)) {
      for (i <- 0 until portCount) {
        assume(inputs(i).valid === False)
      }
      assert(select === 0)
      if (withLock) assert(!locked)
    }

    for (i <- 0 until portCount) {
      inputs(i).formalAssumesSlave()
    }

    assert(select < portCount)
    assert(select === OHToUInt(selectOH))
    assert(selectOH === OHMasking.first(selectOH))
    assert(output.fire === inputs(select).fire)
    assert(output.payload === inputs(select).payload)
  }

  def prepareContext(withLock: Boolean) = new Area {
    val portCount = 5
    val select = UInt(log2Up(portCount) bit)
    val selectOH = Bits(portCount bit)
    val dataType = Bits(8 bits)
    val locked = Bool()

    val reset = ClockDomain.current.isResetActive
    val notReset = !(reset || past(reset))
    cover(notReset)
    assumeInitial(reset)

    val inputs = Vec(slave(Stream(dataType)), portCount)
    val output = master(Stream(dataType))
    val usual = usualVerify(inputs, output, portCount, select, selectOH, reset, withLock, locked)

    val inputsValid = Vec(inputs.map(_.valid))
    val selStableCond = past(output.isStall) && notReset
    val selectUnLockCond = !locked || past(output.fire)
  }

  def prepareContextFragment(withLock: Boolean) = new Area {
    val portCount = 5
    val select = UInt(log2Up(portCount) bit)
    val selectOH = Bits(portCount bit)
    val dataType = Fragment(Bits(8 bits))
    val locked = Bool()

    val reset = ClockDomain.current.isResetActive
    val notReset = !(reset || past(reset))
    cover(notReset)
    assumeInitial(reset)

    val inputs = Vec(slave(Stream(dataType)), portCount)
    val output = master(Stream(dataType))
    val usual = usualVerify(inputs, output, portCount, select, selectOH, reset, withLock, locked)

    val inputsValid = Vec(inputs.map(_.valid))
    val selStableCond = !past((inputsValid.asBits === 0) || output.last || reset)
    val selectUnLockCond = !locked || past(output.last && output.fire)
  }

  def sequentialOrderVerify(select: UInt, portCount: Int) = new Area {
    val d1 = anyconst(UInt(log2Up(portCount) bit))
    assume(d1 < portCount)
    val d2 = UInt(log2Up(portCount) bit)
    d2 := (d1 + 1) % portCount

    val cntSeqCheck = changed(select) && (select === d2)
    cover(cntSeqCheck)
    when(cntSeqCheck) {
      assert(past(select) === d1)
    }
  }

  def lowFirstVerify(
      selectOH: Bits,
      inputsvalid: Vec[Bool],
      unLockCond: Bool
  ) = new Area {
    val inputsLowerFirst = OHMasking.first(inputsvalid).asBits
    cover(unLockCond)
    when(unLockCond) {
      assert(selectOH === inputsLowerFirst)
    }
  }

  def roundRobinVerify(
      selectOH: Bits,
      inputsValid: Vec[Bool],
      portCount: Int,
      maskLocked: Vec[Bool],
      unLockCond: Bool
  ) = new Area {
    val requests = inputsValid.asBits.asUInt
    val uGranted = selectOH.asUInt
    val doubleRequests = requests @@ requests
    val doubleGrant = doubleRequests & ~(doubleRequests - uGranted)
    val masked = doubleGrant(portCount, portCount bits) | doubleGrant(0, portCount bits)
    val inputsRoundRobinOH = masked.asBits

    cover(unLockCond)
    when(unLockCond) {
      assert(selectOH === inputsRoundRobinOH)
    }
    // used for Prove
    assert(maskLocked === OHMasking.first(maskLocked))
  }

  def selStableVerify(selectOH: Bits, selStableCond: Bool, withLock: Boolean) = new Area {
    if (withLock) {
      cover(selStableCond)
      when(selStableCond) {
        assert(stable(selectOH))
      }
    }
  }

  def withAssertsVerify[T <: Data](outisStall: Bool, verifyCond: Vec[Bool], output: Stream[T], withLock: Boolean) =
    new Area {
      if (withLock) {
        val stallStableSel = outisStall && stable(verifyCond)
        cover(stallStableSel)
        when(stallStableSel) {
          output.formalAssertsMaster()
        }
      }
    }

  test("Arbiter-sequentialOrder-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val withLock = true
        val context = prepareContext(withLock)
        val dut = FormalDut(
          new StreamArbiter(context.dataType, context.portCount)(
            StreamArbiter.Arbitration.sequentialOrder,
            StreamArbiter.Lock.none
          )
        )

        context.select := dut.io.chosen
        context.selectOH := dut.io.chosenOH
        context.locked := dut.locked
        context.output << dut.io.output
        for (i <- 0 until context.portCount) {
          context.inputs(i) >> dut.io.inputs(i)
        }

        val withAssert = withAssertsVerify(context.output.isStall, context.inputsValid, context.output, withLock)
        val selStable = selStableVerify(context.selectOH, context.selStableCond, withLock)
        val sequentialOrder = sequentialOrderVerify(context.select, context.portCount)
      })
  }

  test("Arbiter-lowerfirst-none-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val withLock = false
        val context = prepareContext(withLock)
        val dut = FormalDut(
          new StreamArbiter(context.dataType, context.portCount)(
            StreamArbiter.Arbitration.lowerFirst,
            StreamArbiter.Lock.none
          )
        )

        context.select := dut.io.chosen
        context.selectOH := dut.io.chosenOH
        context.locked := dut.locked
        context.output << dut.io.output
        for (i <- 0 until context.portCount) {
          context.inputs(i) >> dut.io.inputs(i)
        }

        val withAssert = withAssertsVerify(context.output.isStall, context.inputsValid, context.output, withLock)
        val selStable = selStableVerify(context.selectOH, context.selStableCond, withLock)
        val lowFirst = lowFirstVerify(context.selectOH, context.inputsValid, context.selectUnLockCond)
      })
  }

  test("Arbiter-lowerfirst-transactionLock-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {

        val withLock = true
        val context = prepareContext(withLock)
        val dut = FormalDut(
          new StreamArbiter(context.dataType, context.portCount)(
            StreamArbiter.Arbitration.lowerFirst,
            StreamArbiter.Lock.transactionLock
          )
        )

        context.select := dut.io.chosen
        context.selectOH := dut.io.chosenOH
        context.locked := dut.locked
        context.output << dut.io.output
        for (i <- 0 until context.portCount) {
          context.inputs(i) >> dut.io.inputs(i)
        }
        // used for Prove
        when(context.notReset) {
          assert(past(context.output.isStall) === dut.locked)
        }

        val withAssert = withAssertsVerify(context.output.isStall, context.inputsValid, context.output, withLock)
        val selStable = selStableVerify(context.selectOH, context.selStableCond, withLock)
        val lowFirst = lowFirstVerify(context.selectOH, context.inputsValid, context.selectUnLockCond)
      })
  }

  test("Arbiter-lowerfirst-fragment-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val withLock = true
        val context = prepareContextFragment(withLock)
        val dut = FormalDut(
          new StreamArbiter(context.dataType, context.portCount)(
            StreamArbiter.Arbitration.lowerFirst,
            StreamArbiter.Lock.fragmentLock
          )
        )

        context.select := dut.io.chosen
        context.selectOH := dut.io.chosenOH
        context.locked := dut.locked
        context.output << dut.io.output
        for (i <- 0 until context.portCount) {
          context.inputs(i) >> dut.io.inputs(i)
        }

        val withAssert = withAssertsVerify(context.output.isStall, context.inputsValid, context.output, withLock)
        val selStable = selStableVerify(context.selectOH, context.selStableCond, withLock)
        val lowFirst = lowFirstVerify(context.selectOH, context.inputsValid, context.selectUnLockCond)
      })
  }

  test("Arbiter-roundrobin-none-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val withLock = false
        val context = prepareContext(withLock)
        val dut = FormalDut(
          new StreamArbiter(context.dataType, context.portCount)(
            StreamArbiter.Arbitration.roundRobin,
            StreamArbiter.Lock.none
          )
        )

        context.select := dut.io.chosen
        context.selectOH := dut.io.chosenOH
        context.locked := dut.locked
        context.output << dut.io.output
        for (i <- 0 until context.portCount) {
          context.inputs(i) >> dut.io.inputs(i)
        }

        val roundRobin = roundRobinVerify(
          context.selectOH,
          context.inputsValid,
          context.portCount,
          dut.maskLocked,
          context.selectUnLockCond
        )
        val withAssert = withAssertsVerify(context.output.isStall, roundRobin.masked.asBools, context.output, withLock)
        val selStable = selStableVerify(context.selectOH, context.selStableCond, withLock)
      })
  }

  test("Arbiter-roundrobin-transactionLock-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      .withDebug
      .doVerify(new Component {
        val withLock = true
        val context = prepareContext(withLock)
        val dut = FormalDut(
          new StreamArbiter(context.dataType, context.portCount)(
            StreamArbiter.Arbitration.roundRobin,
            StreamArbiter.Lock.transactionLock
          )
        )

        context.select := dut.io.chosen
        context.selectOH := dut.io.chosenOH
        context.locked := dut.locked
        context.output << dut.io.output
        for (i <- 0 until context.portCount) {
          context.inputs(i) >> dut.io.inputs(i)
        }
        // used for Prove
        when(context.notReset) {
          assert(past(context.output.isStall) === dut.locked)
        }
        val roundRobin = roundRobinVerify(
          context.selectOH,
          context.inputsValid,
          context.portCount,
          dut.maskLocked,
          context.selectUnLockCond
        )
        val withAssert = withAssertsVerify(context.output.isStall, roundRobin.masked.asBools, context.output, withLock)
        val selStable = selStableVerify(context.selectOH, context.selStableCond, withLock)
      })
  }

  test("Arbiter-roundrobin-fragment-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val withLock = true
        val context = prepareContextFragment(withLock)
        val dut = FormalDut(
          new StreamArbiter(context.dataType, context.portCount)(
            StreamArbiter.Arbitration.roundRobin,
            StreamArbiter.Lock.fragmentLock
          )
        )

        context.select := dut.io.chosen
        context.selectOH := dut.io.chosenOH
        context.locked := dut.locked
        context.output << dut.io.output
        for (i <- 0 until context.portCount) {
          context.inputs(i) >> dut.io.inputs(i)
        }

        val roundRobin = roundRobinVerify(
          context.selectOH,
          context.inputsValid,
          context.portCount,
          dut.maskLocked,
          context.selectUnLockCond
        )
        val withAssert = withAssertsVerify(context.output.isStall, roundRobin.masked.asBools, context.output, withLock)
        val selStable = selStableVerify(context.selectOH, context.selStableCond, withLock)
      })
  }
}

class FormalDeMuxTester extends SpinalFormalFunSuite {
  def formaldemux(selWithCtrl: Boolean = false) = {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val portCount = 5
        val dataType = Bits(8 bits)
        val dut = FormalDut(new StreamDemux(dataType, portCount))

        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val demuxSelect = anyseq(UInt(log2Up(portCount) bit))
        val demuxInput = slave(Stream(dataType))
        val demuxOutputs = Vec(master(Stream(dataType)), portCount)

        dut.io.select := demuxSelect
        demuxInput >> dut.io.input

        when(reset || past(reset)) {
          assume(demuxInput.valid === False)
        }

        val selStableCond = if (selWithCtrl) past(demuxOutputs(demuxSelect).isStall) else null
        if (selWithCtrl) {
          cover(selStableCond)
          when(selStableCond) {
            assume(stable(demuxSelect))
          }
        }

        assumeInitial(demuxSelect < portCount)
        cover(demuxInput.fire)
        demuxInput.formalAssumesSlave()
        demuxInput.formalCovers(5)

        val inputFireStableSelChanged = past(demuxInput.fire) && demuxInput.fire && changed(demuxSelect)
        cover(inputFireStableSelChanged)

        for (i <- 0 until portCount) {
          demuxOutputs(i) << dut.io.outputs(i)
          demuxOutputs(i).formalAssertsMaster()
          demuxOutputs(i).formalCovers(5)
        }

        for (i <- 0 until portCount) {
          assert(demuxOutputs(i).payload === demuxInput.payload)
          when(i =/= demuxSelect) {
            assert(demuxOutputs(i).valid === False)
          }
        }
        when(demuxSelect < portCount) {
          assert(demuxOutputs(demuxSelect) === demuxInput)
        }
      })
  }
  test("demux_sel_with_control") {
    formaldemux(true)
  }
  test("demux_sel_without_control") {
    shouldFail(formaldemux(false))
  }

  test("demux_with_selector") {
    FormalConfig
      .withProve(20)
      .withCover(20)
      .doVerify(new Component {
        val portCount = 5
        val dataType = Bits(8 bits)
        val dut = FormalDut(new StreamDemux(dataType, portCount))
        val selector = dut.io.createSelector()

        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        val demuxSelector = slave(Stream(UInt(log2Up(portCount) bit)))
        demuxSelector >> selector
        val demuxInput = slave(Stream(dataType))
        demuxInput >> dut.io.input
        val demuxOutputs = Vec(master(Stream(dataType)), portCount)
        for (i <- 0 until portCount) {
          demuxOutputs(i) << dut.io.outputs(i)
        }

        when(reset || past(reset)) {
          assume(demuxInput.valid === False)
          assume(demuxSelector.valid === False)
        }

        assumeInitial(demuxSelector.payload < portCount)
        demuxSelector.formalAssumesSlave()
        demuxInput.formalAssumesSlave()
        demuxInput.formalCovers(5)

        val inputFireStableSelChanged = past(demuxInput.fire) && demuxInput.fire && changed(dut.io.select)
        cover(inputFireStableSelChanged)

        for (i <- 0 until portCount) {
          demuxOutputs(i).formalAssertsMaster()
          demuxOutputs(i).formalCovers(5)
        }

        for (i <- 0 until portCount) {
          when(i =/= dut.io.select) {
            assert(demuxOutputs(i).valid === False)
          }
        }
        when(dut.io.select < portCount) {
          assert(demuxOutputs(dut.io.select) === demuxInput)
        }
      })
  }
}

class FormalDispatcherSequencialTester extends SpinalFormalFunSuite {
  test("DispatcherSequencial-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(40)
      // .withDebug
      .doVerify(new Component {
        val portCount = 5
        val dataType = Bits(8 bits)
        val reset = ClockDomain.current.isResetActive
        val dut = FormalDut(new StreamDispatcherSequencial(dataType, portCount))

        assumeInitial(reset)

        val muxInput = slave(Stream(dataType))
        val muxOutputs = Vec(master(Stream(dataType)), portCount)

        muxInput >> dut.io.input
        for (i <- 0 until portCount) {
          muxOutputs(i) << dut.io.outputs(i)
        }

        when(reset || past(reset)) {
          assume(muxInput.valid === False)
        }

        cover(muxInput.fire)

        muxInput.formalAssumesSlave()
        muxInput.formalCovers(3)

        for (i <- 0 until portCount) {
          cover(dut.counter.value === i)
          muxOutputs(i).formalAssertsMaster()
          muxOutputs(i).formalCovers()
        }

        for (i <- 0 until portCount) {
          when(dut.counter.value =/= i) {
            assert(muxOutputs(i).valid === False)
          } otherwise {
            assert(muxOutputs(i) === muxInput)
          }
        }

        val d1 = anyconst(UInt(log2Up(portCount) bit))
        assume(d1 < portCount)
        val d2 = UInt(log2Up(portCount) bit)
        d2 := (d1 + 1) % portCount

        val cntSeqCheck = changed(dut.counter.value) && (dut.counter.value === d2)
        cover(cntSeqCheck)
        when(cntSeqCheck) {
          assert(past(dut.counter.value) === d1)
        }
      })
  }
}

class FormalFifoCCTester extends SpinalFormalFunSuite {
  def formalContext(pushPeriod: Int, popPeriod: Int, seperateReset: Boolean = false) = new Area {
    val back2backCycles = 2
    val fifoDepth = 4

    val pushClock = ClockDomain.current
    val reset = pushClock.isResetActive
    val popClock = ClockDomain.external("pop")
    val popReset = popClock.isResetActive

    val inValue = in(UInt(3 bits))
    val inValid = in(Bool())
    val outReady = in(Bool())
    val gclk = GlobalClock()

    assumeInitial(reset)
    assumeInitial(popReset)

    gclk.assumeClockTiming(pushClock, pushPeriod)
    gclk.assumeResetReleaseSync(pushClock)
    if (!seperateReset) {
      gclk.keepBoolLeastCycles(reset, popPeriod)
    }

    gclk.assumeClockTiming(popClock, popPeriod)
    if (seperateReset) {
      gclk.assumeResetReleaseSync(popClock)
      gclk.alignAsyncResetStart(pushClock, popClock)
    }

    val dut = FormalDut(new StreamFifoCC(cloneOf(inValue), fifoDepth, pushClock, popClock, !seperateReset))
    gclk.assumeIOSync2Clock(pushClock, dut.io.push.valid)
    gclk.assumeIOSync2Clock(pushClock, dut.io.push.payload)
    gclk.assumeIOSync2Clock(popClock, dut.io.pop.ready)

    dut.io.push.payload := inValue
    dut.io.push.valid := inValid
    dut.io.pop.ready := outReady

    // assume no valid while reset and one clock later.
    when(reset || past(reset)) {
      assume(inValid === False)
    }

    dut.formalAsserts(gclk.domain)

    val pushArea = new ClockingArea(pushClock) {
      dut.io.push.formalAssumesSlave()
      dut.io.push.formalCovers()
    }

    // back to back transaction cover test.
    val popCheckDomain = if (seperateReset) popClock else popClock.copy(reset = reset)
    val popArea = new ClockingArea(popCheckDomain) {
      dut.io.pop.formalCovers(back2backCycles)
      dut.io.pop.formalAssertsMaster()
    }
  }

  def testMain(pushPeriod: Int, popPeriod: Int, seperateReset: Boolean = false) = {
    val proveCycles = 8
    val coverCycles = 8
    val maxPeriod = Math.max(pushPeriod, popPeriod)

    FormalConfig
      .withProve(maxPeriod * proveCycles)
      .withCover(maxPeriod * coverCycles)
      .withAsync
      .doVerify(new Component {
        val context = formalContext(pushPeriod, popPeriod, seperateReset)
      })
  }

  test("fifo-verify fast pop") {
    testMain(5, 3)
  }

  test("fifo-verify fast push") {
    testMain(3, 5)
  }

//   test("fifo-verify ultra fast pop") {
//     testMain(11, 2)
//   }

//   test("fifo-verify ultra fast push") {
//     testMain(2, 11)
//   }

  test("fifo-verify fast pop reset seperately") {
    testMain(5, 3, true)
  }

  test("fifo-verify fast push reset seperately") {
    testMain(3, 5, true)
  }

//   test("fifo-verify ultra fast pop reset seperately") {
//     testMain(11, 2, true)
//   }

//   test("fifo-verify ultra fast push reset seperately") {
//     testMain(2, 11, true)
//   }

  def testNoLoss(pushPeriod: Int, popPeriod: Int, seperateReset: Boolean = false) = {
    val proveCycles = 8
    val coverCycles = 8
    val maxPeriod = Math.max(pushPeriod, popPeriod)

    FormalConfig
      .withCover(maxPeriod * coverCycles)
      .withAsync
      .withDebug
      .doVerify(new Component {
        val context = formalContext(pushPeriod, popPeriod, seperateReset)

        val d1 = anyconst(cloneOf(context.inValue))
        val d1_in = RegInit(False)
        when(context.inValue === d1 & context.dut.io.push.fire) {
          d1_in := True
        }
        when(d1_in) { assume(context.inValue =/= d1) }

        val popCheckArea = new ClockingArea(context.popCheckDomain) {
          val d1_inCC = BufferCC(d1_in)
          val cond = d1_inCC & context.dut.io.pop.fire
          val hist = History(context.dut.io.pop.payload, context.fifoDepth, cond)
          val counter = Counter(context.fifoDepth, inc = cond)
          when(!d1_inCC) { counter.clear() }
          cover(counter.willOverflow & !hist.sContains(d1))
        }

        val globalCheckArea = new ClockingArea(context.popCheckDomain) {
          def checkValidData(cond: UInt => Bool): Seq[Bool] = context.dut.rework {
            val maps = for (i <- 0 until context.fifoDepth) yield {
              val index = context.dut.popCC.popPtr + i
              val out = False
              when(i < context.dut.pushCC.pushPtr - context.dut.popCC.popPtr) {
                out := cond(context.dut.ram(index.resized))
              }
              out
            }
            maps
          }
          val fits = checkValidData(_ === d1.pull())
          val containD1 = fits.reduce(_ || _)
          when(!d1_in) { assume(!containD1) }
        }
      })
  }

  test("noloss fast pop") {
    shouldFail(testNoLoss(5, 3))
  }

  test("noloss fast push") {
    shouldFail(testNoLoss(3, 5))
  }

  // test("noloss ultra fast pop") {
  //   shouldFail(testNoLoss(11, 2))
  // }

  // test("noloss ultra fast push") {
  //   shouldFail(testNoLoss(2, 11))
  // }

  test("noloss fast pop reset seperately") {
    shouldFail(testNoLoss(5, 3, true))
  }

  test("noloss fast push reset seperately") {
    shouldFail(testNoLoss(3, 5, true))
  }

  // test("noloss ultra fast pop reset seperately") {
  //   shouldFail(testNoLoss(11, 2, true))
  // }

  // test("noloss ultra fast push reset seperately") {
  //   shouldFail(testNoLoss(2, 11, true))
  // }

}

class FormalFifoTester extends SpinalFormalFunSuite {
  test("fifo-verify all") {
    val initialCycles = 2
    val inOutDelay = 2
    val coverCycles = 10
    FormalConfig
      .withBMC(10)
      .withProve(10)
      .withCover(coverCycles)
      // .withDebug
      .doVerify(new Component {
        val depth = 4
        val dut = FormalDut(new StreamFifo(UInt(7 bits), depth))
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val inValue = anyseq(UInt(7 bits))
        val inValid = anyseq(Bool())
        val outReady = anyseq(Bool())
        dut.io.push.payload := inValue
        dut.io.push.valid := inValid
        dut.io.pop.ready := outReady

        // assume no valid while reset and one clock later.
        when(reset || past(reset)) {
          assume(inValid === False)
        }

        dut.io.push.formalAssumesSlave()
        dut.io.pop.formalAssertsMaster()

        dut.io.push.formalCovers()
        // back to back transaction cover test.
        dut.io.pop.formalCovers(coverCycles - initialCycles - inOutDelay - 1)

        val d1 = anyconst(UInt(7 bits))
        val d2 = anyconst(UInt(7 bits))

        val (d1_in, d2_in) = dut.io.push.formalAssumesOrder(d1, d2)
        val (d1_out, d2_out) = dut.io.pop.formalAssertsOrder(d1, d2)

        when(!d1_in) { assume(!dut.formalContains(d1)) }
        when(d1_in && !d1_out) { assert(dut.formalCount(d1) === 1) }

        when(!d2_in) { assume(!dut.formalContains(d2)) }
        when(d2_in && !d2_out) { assert(dut.formalCount(d2) === 1) }

        when(d1_in && d2_in && !d1_out) { assert(!d2_out) }

        def getCompId(x: UInt): UInt = {
          val id = OHToUInt(dut.formalCheck(_ === x.pull()).asBits)
          val extId = id +^ depth
          val compId = CombInit(extId)
          when(id >= dut.logic.popPtr) {
            compId := id.resized
          }
          compId
        }
        when(d1_in && d2_in && !d1_out && !d2_out) {
          assert(getCompId(d1) < getCompId(d2))
        }
      })
  }
}

class FormalForkTester extends SpinalFormalFunSuite {
  def formalfork (synchronous: Boolean = false, back2BackCheck: Boolean = false ) = {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val portCount = 2
        val dataType = Bits(8 bits)
        val dut = FormalDut(new StreamFork(dataType, portCount, synchronous))

        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val input = slave(Stream(dataType))
        val outputs = Vec(master(Stream(dataType)), portCount)

        input >> dut.io.input
        for (i <- 0 until portCount) {
          outputs(i) << dut.io.outputs(i)
        }

        when(reset || past(reset)) {
          assume(input.valid === False)
        }

        cover(input.fire)

        input.formalAssumesSlave()
        if(back2BackCheck) input.formalCovers(3)

        for (i <- 0 until portCount) {
          assert(outputs(i).payload === input.payload)
          outputs(i).formalAssertsMaster()
          if(back2BackCheck) outputs(i).formalCovers(3)          
        }
        
        val fired = if(!synchronous)Vec(RegInit(True), portCount) else null // store fire status, 
        val outputsFireAsync = if(!synchronous){fired(0) =/= fired(1)} else null
        val out0FireFirst = if(!synchronous){fired(1) && !fired(0)} else null
        val out1FireFirst = if(!synchronous){fired(0) && !fired(1)} else null
        val out0FireLast = if(!synchronous){~(input.fire ^ outputs(0).fire)} else null
        val out1FireLast = if(!synchronous){~(input.fire ^ outputs(1).fire)} else null        

        if(synchronous){
          for (i <- 0 until portCount) {
            assert(outputs(i).fire === input.fire)
            
          }
        }
        else{ 
          for(i <- 0 until portCount){
            cover(input.ready)
            when(input.ready){
              fired(i) := True                            
            }
            cover(!input.ready && outputs(i).fire)
            when(!input.ready && outputs(i).fire){
              fired(i) := False              
            }

            assert(fired(i) === dut.logic.linkEnable(i))
          }          
          
          cover(out0FireFirst && !outputs(1).fire)
          cover(out1FireFirst && !outputs(0).fire)

          when(outputsFireAsync) {
            assert(!past(input.fire))
          }

          when(out0FireFirst) {
            assert(out1FireLast) 
            assert(!outputs(0).valid)
          }

          when(out1FireFirst) {
            assert(out0FireLast) 
            assert(!outputs(1).valid)
          }
        }       
      })
  }

  test("fork-verify sync") {
    formalfork(true,false)
  }

  test("fork-verify sync back2Back fail") {
    shouldFail(formalfork(true,true))
  }
  
  test("fork-verify async") {
    formalfork(false,true)
  }
}

class Join[T1 <: Data, T2 <: Data](dataType_0: T1, dataType_1: T2) extends Component {
  val io = new Bundle {
    val inputs_0 = slave(Stream (dataType_0))
    val inputs_1 = slave(Stream (dataType_1))
    val output = master(Stream(TupleBundle2(
        dataType_0,
        dataType_1
    )))
  }
  val logic = StreamJoin(io.inputs_0,io.inputs_1)
  io.output << logic
}

class FormalJoinTester extends SpinalFormalFunSuite {
  test("StreamJoinTester-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      
      // .withDebug
      .doVerify(new Component {
        val portCount = 5
        val dataType_0 = Bits(8 bits)
        val dataType_1 = Bits(16 bits)
        val dut = FormalDut(new Join(dataType_0, dataType_1))
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val inputs_0 = slave Stream (dataType_0)
        val inputs_1 = slave Stream (dataType_1)
        val output = master(
          Stream(
            TupleBundle2(
              dataType_0,
              dataType_1
            )
          )
        )

        inputs_0 >> dut.io.inputs_0
        inputs_1 >> dut.io.inputs_1
        output << dut.io.output

        when(reset || past(reset)) {
          assume(inputs_0.valid === False)
          assume(inputs_1.valid === False)
        }

        inputs_0.formalAssumesSlave()
        inputs_1.formalAssumesSlave()
        output.formalAssertsMaster()
        output.formalCovers(3)

        assert(inputs_0.fire === inputs_1.fire)
        assert(output.fire === inputs_0.fire)
        assert(output.payload._1 === inputs_0.payload)
        assert(output.payload._2 === inputs_1.payload)
      })
  }
}

class FormalMuxTester extends SpinalFormalFunSuite {
  def formalmux(selWithCtrl: Boolean = false) = {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val portCount = 5
        val dataType = Bits(8 bits)
        val dut = FormalDut(new StreamMux(dataType, portCount))

        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val muxSelect = anyseq(UInt(log2Up(portCount) bit))
        val muxInputs = Vec(slave(Stream(dataType)), portCount)
        val muxOutput = master(Stream(dataType))

        dut.io.select := muxSelect
        muxOutput << dut.io.output

        assumeInitial(muxSelect < portCount)
        val selStableCond = if (selWithCtrl) past(muxOutput.isStall) else null

        when(reset || past(reset)) {
          for (i <- 0 until portCount) {
            assume(muxInputs(i).valid === False)
          }
        }

        if (selWithCtrl) {
          cover(selStableCond)
          when(selStableCond) {
            assume(stable(muxSelect))
          }
        }
        muxOutput.formalAssertsMaster()
        muxOutput.formalCovers(5)

        for (i <- 0 until portCount) {
          muxInputs(i) >> dut.io.inputs(i)
          muxInputs(i).formalAssumesSlave()
        }

        cover(muxOutput.fire)

        for (i <- 0 until portCount) {
          cover(dut.io.select === i)
          muxInputs(i).formalAssumesSlave()
        }

        when(muxSelect < portCount) {
          assert(muxOutput === muxInputs(muxSelect))
        }
      })
  }
  test("mux_sel_with_control") {
    formalmux(true)
  }
  test("mux_sel_without_control") {
    shouldFail(formalmux(false))
  }

  test("mux_with_selector") {
    FormalConfig
      .withProve(20)
      .withCover(20)
      .doVerify(new Component {
        val portCount = 5
        val dataType = Bits(8 bits)
        val dut = FormalDut(new StreamMux(dataType, portCount))
        val selector = dut.io.createSelector()

        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        val muxSelector = slave(cloneOf(selector))
        muxSelector >> selector
        val muxInputs = Vec(slave(Stream(dataType)), portCount)
        for (i <- 0 until portCount) {
          muxInputs(i) >> dut.io.inputs(i)
        }
        val muxOutput = master(Stream(dataType))
        muxOutput << dut.io.output

        assumeInitial(muxSelector.payload < portCount)
        muxSelector.formalAssumesSlave()

        when(reset || past(reset)) {
          for (i <- 0 until portCount) {
            assume(muxInputs(i).valid === False)
          }
          assume(muxSelector.valid === False)
        }

        muxOutput.formalAssertsMaster()
        muxOutput.formalCovers(5)
        cover(dut.io.select =/= muxSelector.payload)
        cover(changed(muxSelector.payload) & stable(muxSelector.valid) & muxSelector.valid)
        val readyBits = muxInputs.map(_.ready).asBits()
        assert((readyBits === 0) || (CountOne(readyBits) === 1))

        for (i <- 0 until portCount) {
          cover(dut.io.select === i)
          muxInputs(i).formalAssumesSlave()
        }

        when(dut.io.select < portCount) {
          assert(muxOutput === muxInputs(dut.io.select))
        }
      })

  }
}
