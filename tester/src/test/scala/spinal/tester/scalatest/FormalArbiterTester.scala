package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

class FormalArbiterTester extends SpinalFormalFunSuite {
  test("Arbiter-sequentialOrder-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val portCount = 5
        val dataType = Bits(8 bits)

        val dut = FormalDut(
          new StreamArbiter(dataType, portCount)(StreamArbiter.Arbitration.sequentialOrder, StreamArbiter.Lock.none)
        )
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val select = UInt(log2Up(portCount) bit)
        val selectOH = Bits(portCount bit)
        val inputs = Vec(slave(Stream(dataType)), portCount)
        val output = master(Stream(dataType))

        when(reset || past(reset)) {
          for (i <- 0 until portCount) {
            assume(inputs(i).valid === False)
          }
        }

        select := dut.io.chosen
        selectOH := dut.io.chosenOH
        output << dut.io.output

        for (i <- 0 until portCount) {
          inputs(i) >> dut.io.inputs(i)
          inputs(i).formalAssumesSlave()
        }
        output.formalCovers(2)
        output.formalAssertsMaster()

        assert(select < portCount)
        assert(select === OHToUInt(selectOH))
        assert(output.payload === inputs(select).payload)

        val d1 = anyconst(UInt(log2Up(portCount) bit))
        assume(d1 < portCount)
        val d2 = UInt(log2Up(portCount) bit)
        d2 := (d1 + 1) % portCount

        val cntSeqCheck = changed(select) && (select === d2)
        cover(cntSeqCheck)
        when(cntSeqCheck) {
          assert(past(select) === d1)
        }
      })
  }

  test("Arbiter-lowerfirst-none-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val portCount = 5
        val dataType = Bits(8 bits)

        val dut = FormalDut(
          new StreamArbiter(dataType, portCount)(StreamArbiter.Arbitration.lowerFirst, StreamArbiter.Lock.none)
        )
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val select = UInt(log2Up(portCount) bit)
        val selectOH = Bits(portCount bit)
        val inputs = Vec(slave(Stream(dataType)), portCount)
        val output = master(Stream(dataType))

        select := dut.io.chosen
        selectOH := dut.io.chosenOH
        output << dut.io.output

        when(reset || past(reset)) {
          for (i <- 0 until portCount) {
            assume(inputs(i).valid === False)
          }
        }

        for (i <- 0 until portCount) {
          inputs(i) >> dut.io.inputs(i)
          inputs(i).formalAssumesSlave()
        }
        output.formalCovers(2)

        assert(select < portCount)
        assert(select === OHToUInt(selectOH))
        assert(output.payload === inputs(select).payload)

        val allInputsValidOH = OHMasking.first(Vec(inputs.map(_.valid))).asBits
        assert(selectOH === allInputsValidOH)
        val stallStableSel = output.isStall && stable(allInputsValidOH)
        cover(stallStableSel)
        when(stallStableSel) {
          output.formalAssertsMaster()
        }
      })
  }

  test("Arbiter-lowerfirst-transactionLock-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val portCount = 5
        val dataType = Bits(8 bits)

        val dut = FormalDut(
          new StreamArbiter(dataType, portCount)(
            StreamArbiter.Arbitration.lowerFirst,
            StreamArbiter.Lock.transactionLock
          )
        )
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val select = UInt(log2Up(portCount) bit)
        val selectOH = Bits(portCount bit)
        val inputs = Vec(slave(Stream(dataType)), portCount)
        val output = master(Stream(dataType))

        select := dut.io.chosen
        selectOH := dut.io.chosenOH
        output << dut.io.output

        when(reset || past(reset)) {
          for (i <- 0 until portCount) {
            assume(inputs(i).valid === False)
            assert(!dut.locked)
          }
        }

        val notReset = !(reset || past(reset))
        cover(notReset)
        when(notReset) {
          assert(past(output.isStall) === dut.locked)
        }

        for (i <- 0 until portCount) {
          inputs(i) >> dut.io.inputs(i)
          inputs(i).formalAssumesSlave()
        }
        output.formalCovers(2)
        output.formalAssertsMaster()

        assert(select < portCount)
        assert(select === OHToUInt(selectOH))
        assert(output.payload === inputs(select).payload)
        assert(output.valid === inputs(select).valid)

        val shouldStableSelCond = past(output.isStall) && notReset
        cover(shouldStableSelCond)
        when(shouldStableSelCond) {
          assert(stable(select))
        }

        val allInputsValidOH = OHMasking.first(Vec(inputs.map(_.valid))).asBits
        cover(changed(select))
        when(changed(select)) {
          assert(selectOH === allInputsValidOH)
        }
      })
  }

  test("Arbiter-lowerfirst-fragment-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val portCount = 5
        val dataType = Fragment(Bits(8 bits))

        val dut = FormalDut(
          new StreamArbiter(dataType, portCount)(StreamArbiter.Arbitration.lowerFirst, StreamArbiter.Lock.fragmentLock)
        )
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val select = UInt(log2Up(portCount) bit)
        val selectOH = Bits(portCount bit)
        val inputs = Vec(slave(Stream(dataType)), portCount)
        val output = master(Stream(dataType))

        select := dut.io.chosen
        selectOH := dut.io.chosenOH
        output << dut.io.output

        when(reset || past(reset)) {
          for (i <- 0 until portCount) {
            assume(inputs(i).valid === False)
          }
          assert(!dut.locked)
          assert(select === 0)
        }

        for (i <- 0 until portCount) {
          inputs(i) >> dut.io.inputs(i)
          inputs(i).formalAssumesSlave()
        }
        output.formalCovers(2)
        output.formalAssertsMaster()

        assert(select < portCount)
        assert(select === OHToUInt(selectOH))
        assert(output.payload === inputs(select).payload)

        // if not locked or fragment last fired, select will be lowest inputs.valid
        val allInputsValidOH = OHMasking.first(Vec(inputs.map(_.valid))).asBits
        val selectRefresh = !dut.locked || past(output.last && output.fire)
        cover(selectRefresh)
        when(selectRefresh) {
          assert(selectOH === allInputsValidOH)
        }

        // chosenOH should be a OneHot value
        assert(selectOH === OHMasking.first(selectOH))

        val shouldStableSelCond = !past((allInputsValidOH === 0) || output.last || reset)
        cover(shouldStableSelCond)
        when(shouldStableSelCond) {
          assert(stable(select))
        }
      })
  }

  test("Arbiter-roundrobin-none-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val portCount = 5
        val dataType = Bits(8 bits)
        val dut = FormalDut(
          new StreamArbiter(dataType, portCount)(StreamArbiter.Arbitration.roundRobin, StreamArbiter.Lock.none)
        )
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val select = UInt(log2Up(portCount) bit)
        val selectOH = Bits(portCount bit)
        val inputs = Vec(slave(Stream(dataType)), portCount)
        val output = master(Stream(dataType))

        select := dut.io.chosen
        selectOH := dut.io.chosenOH
        output << dut.io.output

        when(reset || past(reset)) {
          for (i <- 0 until portCount) {
            assume(inputs(i).valid === False)
          }
        }

        for (i <- 0 until portCount) {
          inputs(i) >> dut.io.inputs(i)
          inputs(i).formalAssumesSlave()
        }
        output.formalCovers(2)

        assert(select < portCount)
        assert(select === OHToUInt(selectOH))
        assert(output.payload === inputs(select).payload)
        assert(output.fire === inputs(select).fire)

        val requests = Vec(inputs.map(_.valid)).asBits.asUInt
        val uGranted = dut.io.chosenOH.asUInt
        val doubleRequests = requests @@ requests
        val doubleGrant = doubleRequests & ~(doubleRequests - uGranted)
        val masked = doubleGrant(portCount, portCount bits) | doubleGrant(0, portCount bits)
        val allInputsValidOH = masked.asBits

        assert(selectOH === allInputsValidOH)
        assert(dut.maskLocked === OHMasking.first(dut.maskLocked))
        assert(selectOH === OHMasking.first(selectOH))

        val stallStableSel = output.isStall && stable(allInputsValidOH)
        cover(stallStableSel)
        when(stallStableSel) {
          output.formalAssertsMaster()
        }
      })
  }

  test("Arbiter-roundrobin-transactionLock-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      .withDebug
      .doVerify(new Component {
        val portCount = 5
        val dataType = Bits(8 bits)

        val dut = FormalDut(
          new StreamArbiter(dataType, portCount)(
            StreamArbiter.Arbitration.roundRobin,
            StreamArbiter.Lock.transactionLock
          )
        )
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val select = UInt(log2Up(portCount) bit)
        val selectOH = Bits(portCount bit)
        val inputs = Vec(slave(Stream(dataType)), portCount)
        val output = master(Stream(dataType))

        select := dut.io.chosen
        selectOH := dut.io.chosenOH
        output << dut.io.output

        when(reset || past(reset)) {
          for (i <- 0 until portCount) {
            assume(inputs(i).valid === False)
            assert(!dut.locked)
          }
        }

        val notReset = !(reset || past(reset))
        cover(notReset)
        when(notReset) {
          assert(past(output.isStall) === dut.locked)
        }

        for (i <- 0 until portCount) {
          inputs(i) >> dut.io.inputs(i)
          inputs(i).formalAssumesSlave()
        }
        output.formalCovers(2)
        output.formalAssertsMaster()

        assert(select < portCount)
        assert(select === OHToUInt(selectOH))
        assert(output.payload === inputs(select).payload)
        assert(output.fire === inputs(select).fire)

        val shouldStableSelCond = past(output.isStall) && notReset
        cover(shouldStableSelCond)
        when(shouldStableSelCond) {
          assert(stable(select))
        }

        val requests = Vec(inputs.map(_.valid)).asBits.asUInt
        val uGranted = dut.io.chosenOH.asUInt
        val doubleRequests = requests @@ requests
        val doubleGrant = doubleRequests & ~(doubleRequests - uGranted)
        val masked = doubleGrant(portCount, portCount bits) | doubleGrant(0, portCount bits)
        val allInputsValidOH = masked.asBits

        val selectRefresh = !dut.locked || past(output.fire)
        cover(selectRefresh)
        when(selectRefresh) {
          assert(selectOH === allInputsValidOH)
        }
        assert(dut.maskLocked === OHMasking.first(dut.maskLocked))
        assert(selectOH === OHMasking.first(selectOH))
      })
  }

  test("Arbiter-roundrobin-fragment-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val portCount = 5
        val dataType = Fragment(Bits(8 bits))
        val dut = FormalDut(
          new StreamArbiter(dataType, portCount)(StreamArbiter.Arbitration.roundRobin, StreamArbiter.Lock.fragmentLock)
        )
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val select = UInt(log2Up(portCount) bit)
        val selectOH = Bits(portCount bit)
        val inputs = Vec(slave(Stream(dataType)), portCount)
        val output = master(Stream(dataType))

        select := dut.io.chosen
        selectOH := dut.io.chosenOH
        output << dut.io.output

        when(reset || past(reset)) {
          for (i <- 0 until portCount) {
            assume(inputs(i).valid === False)
          }
          assert(!dut.locked)
          assert(select === 0)
        }

        for (i <- 0 until portCount) {
          inputs(i) >> dut.io.inputs(i)
          inputs(i).formalAssumesSlave()
        }
        output.formalCovers(2)
        output.formalAssertsMaster()

        assert(select === OHToUInt(selectOH))
        assert(dut.io.chosen < portCount)
        assert(output.payload === inputs(select).payload)
        assert(output.fire === inputs(select).fire)

        // if not locked or fragment last fired, select will be most priority inputs.valid
        val requests = Vec(inputs.map(_.valid)).asBits.asUInt
        val uGranted = dut.io.chosenOH.asUInt
        val doubleRequests = requests @@ requests
        val doubleGrant = doubleRequests & ~(doubleRequests - uGranted)
        val masked = doubleGrant(portCount, portCount bits) | doubleGrant(0, portCount bits)
        val allInputsValidOH = masked.asBits

        val selectRefresh = !dut.locked || past(output.last && output.fire)
        cover(selectRefresh)
        when(selectRefresh) {
          assert(selectOH === allInputsValidOH)
        }
        assert(dut.maskLocked === OHMasking.first(dut.maskLocked))
        assert(selectOH === OHMasking.first(selectOH))

        val shouldStableSelCond = !past((allInputsValidOH === 0) || output.last || reset)
        cover(shouldStableSelCond)
        when(shouldStableSelCond) {
          assert(stable(select))
        }
      })
  }
}
