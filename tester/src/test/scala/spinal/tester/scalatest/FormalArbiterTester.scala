package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

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

    val cntSeqCheck = pastValidAfterReset && changed(select) && (select === d2)
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
