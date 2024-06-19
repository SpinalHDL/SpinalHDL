package spinal.tester.code

import spinal.core._
import spinal.core.formal._
import spinal.tester._

/** Test the functional equivalence of the two formal backends:
  * - SymbiYosys: generates SystemVerilog with SVA statements
  * - GHDL:       generates VHDL with PSL statements
  *
  * The functionality is slowly tested by building ontop of previous tested
  * functionality tests. If multiple tests fail, the "higher" one is likely the
  * culprit.
  *
  * Tested API:
  *  - [x] past
  *  - [ ] pastValid
  *  - [ ] pastValidAfterReset
  *  - [x] rose
  *  - [x] fell
  *  - [x] changed
  *  - [x] stable
  *  - [x] initstate
  *  - [x] anyseq / anyconst / allseq / allconst
  *  - [x] assumeInitial
  *  - [x] assume
  *  - [x] cover
  *  - [x] assert
  *  - [ ] QOL show backtrace
  *  - [x] assume/assert inside conditional scope
  *  - [x] assert during reset
  *  - [x] assert outside of reset
  *  - [x] multiple clock domains
  */
class FormalApiTest extends SpinalFormalFunSuite {

  val symbiYosys = SpinalFormalBackendSel.SYMBIYOSYS
  val ghdl = SpinalFormalBackendSel.SYMBIYOSYS_GHDL

  /** Test the generation and verification of assert statement.
    *
    * @param backend formal backend to use, i.e. use SymbiYosys or GHDL
    * @param doFail  deliberately trigger the assert
    */
  def runAssertTest(backend: SpinalFormalBackendSel, doFail: Boolean = false): Unit = {
    val config = FormalConfig.copy(_backend = backend).withBMC(2)

    config.doVerify(new Component {
      setDefinitionName("AssertTest")
      setFormalTester()

      val a = Bool()
      if (!doFail) { a := True }
      assert(a === True)
    })
  }

  test("FormalApiTest.assert.symbiyosys") {
    runAssertTest(symbiYosys)
  }
  test("FormalApiTest.assert.symbiyosys.fail") {
    shouldFailWithOutput("Assert failed in AssertTest")(
      runAssertTest(symbiYosys, doFail = true)
    )
  }
  test("FormalApiTest.assert.ghdl") {
    runAssertTest(ghdl)
  }
  test("FormalApiTest.assert.ghdl.fail") {
    shouldFailWithOutput("Assert failed in AssertTest")(
      runAssertTest(ghdl, doFail = true)
    )
  }

  /** Test the generation and verification of assume statement on IO signals.
    *
    * @note Assume statements only make sense for inputs or unconstrained
    *       internal (anyconst/anyseq) signals. This is tested later.
    *
    * @param backend formal backend to use, i.e. use SymbiYosys or GHDL
    * @param doFail  dont to the assume, following assert will trigger
    */
  def runAssumeIoTest(backend: SpinalFormalBackendSel, doFail: Boolean = false): Unit = {
    val config = FormalConfig.copy(_backend = backend).withBMC(2)

    config.doVerify(new Component {
      setDefinitionName("AssumeIoTest")
      setFormalTester()

      val io = new Bundle {
        val a = in port Bool()
      }
      if (!doFail) { assume(io.a === True) }
      assert(io.a === True)
    })
  }

  test("FormalApiTest.assume.io.symbiyosys") {
    runAssumeIoTest(symbiYosys)
  }
  test("FormalApiTest.assume.io.symbiyosys.fail") {
    shouldFailWithOutput("Assert failed in AssumeIoTest")(
      runAssumeIoTest(symbiYosys, doFail = true)
    )
  }
  test("FormalApiTest.assume.io.ghdl") {
    runAssumeIoTest(ghdl)
  }
  test("FormalApiTest.assume.io.ghdl.fail") {
    shouldFailWithOutput("Assert failed in AssumeIoTest")(
      runAssumeIoTest(ghdl, doFail = true)
    )
  }

  /** Test the generation and verification of anyconst, allconst, anyseq and
    * allseq unconstrained assignments.
    *
    * This test works by first assigning anyconst (or the others) to a boolean
    * signal. The formal solver state space could assume values True or False
    * for this signal now. We then assume the signal to be True. This forces the
    * signal to be True and the following assert passes. In the failing test,
    * this assume is not done. Which triggers the assert as now the state
    * signal = False is stil reachable.
    *
    * Different behavious of anyconst, allconst, anyseq and allseq are DUT
    * specific and not something we have to test.
    *
    * @param func           reference to anyconst, allconst, anyseq or allseq
    * @param backend formal backend to use, i.e. use SymbiYosys or GHDL
    * @param doFail  dont assume a value, following assert will trigger
    */
  def runUnconstrainedTest(func: (Data) => Data, backend: SpinalFormalBackendSel, doFail: Boolean = false): Unit = {
    // Allconst and allseq require option stdt as otherwise yosys generates err:
    //   "Forall-exists problems are only supported in -stbv or -stdt mode."
    // With the stdt flag the default solver yices then fails with error:
    //   "syntax error: declare-datatype is not a command"
    // so we use any other solver that supports forall, here Z3.
    val config = FormalConfig
      .copy(_backend = backend)
      .withBMC(2)
      .withEngies(Seq(SmtBmc(stdt = true, nopresat = true, solver = SmtBmcSolver.Z3)))

    config.doVerify(new Component {
      setDefinitionName("UnconstrainedTest")
      setFormalTester()

      val a = Bool()
      func(a)
      if (!doFail) { assume(a === True) }
      assert(a === True)
    })
  }

  for (
    (func, name) <- Seq[((Data) => Data, String)](
      (anyconst, "anyconst"),
      (allconst, "allconst"),
      (anyseq, "anyseq"),
      (allseq, "allseq")
    )
  ) {
    test(s"FormalApiTest.${name}.symbiyosys") {
      runUnconstrainedTest(func, symbiYosys)
    }
    test(s"FormalApiTest.${name}.symbiyosys.fail") {
      shouldFailWithOutput("Assert failed in UnconstrainedTest")(
        runUnconstrainedTest(func, symbiYosys, doFail = true)
      )
    }
    test(s"FormalApiTest.${name}.ghdl") {
      runUnconstrainedTest(func, ghdl)
    }
    test(s"FormalApiTest.${name}.ghdl.fail") {
      shouldFailWithOutput("Assert failed in UnconstrainedTest")(
        runUnconstrainedTest(func, ghdl, doFail = true)
      )
    }
  }

  /** Test the generation and verification of assume statement on internal
    * unconstrained signals.
    *
    * @param backend formal backend to use, i.e. use SymbiYosys or GHDL
    * @param doFail  dont to the assume, following assert will trigger
    */
  def runAssumeInternalTest(backend: SpinalFormalBackendSel, doFail: Boolean = false): Unit = {
    val config = FormalConfig.copy(_backend = backend).withBMC(2)

    config.doVerify(new Component {
      setDefinitionName("AssumeInternalTest")
      setFormalTester()

      val a = Bool()
      // SymbiYosys (without GHDL) would technically pass the test without
      // marking the internal signal as unconstrained. This is not a behaviour
      // that is intented though.
      anyconst(a)
      if (!doFail) { assume(a === True) }
      assert(a === True)
    })
  }

  test("FormalApiTest.assume.internal.symbiyosys") {
    runAssumeInternalTest(symbiYosys)
  }
  test("FormalApiTest.assume.internal.symbiyosys.fail") {
    shouldFailWithOutput("Assert failed in AssumeInternalTest")(
      runAssumeInternalTest(symbiYosys, doFail = true)
    )
  }
  test("FormalApiTest.assume.internal.ghdl") {
    runAssumeInternalTest(ghdl)
  }
  test("FormalApiTest.assume.internal.ghdl.fail") {
    shouldFailWithOutput("Assert failed in AssumeInternalTest")(
      runAssumeInternalTest(ghdl, doFail = true)
    )
  }

  /** Test the generation and verification of assumeInitial statement.
    *
    * This test works (in the passing case) by assigning anyconst to a boolean
    * signal which is then initially (in the first clock) assumed to be True.
    * The solver is forced to choose True for the value and as the unconstrained
    * assignment is of type anyconst the value cannot change from clock to
    * clock. A subsequent assert tests that the boolean is always True.
    *
    * In the failing case, the unconstrained assignment of anyconst is changed
    * to anyseq. So the solver is only for the first cycle forced to choose True
    * and subsequent cycles the initial assume does not apply anymore.
    *
    * Weirdly, SymbiYosy (without GHDL) requires three clock steps to trigger the
    * assert of the failing test. GHDL ony requires two.
    *
    * @param backend formal backend to use, i.e. use SymbiYosys or GHDL
    * @param doFail  false = use anyconst; true = use anyseq
    */
  def runAssumeInitialTest(backend: SpinalFormalBackendSel, doFail: Boolean = false): Unit = {
    val config = FormalConfig.copy(_backend = backend).withBMC(3)

    config.doVerify(new Component {
      setDefinitionName("AssumeInitialTest")
      setFormalTester()

      val a = Bool()
      if (!doFail) {
        anyconst(a)
      } else {
        anyseq(a)
      }
      assumeInitial(a === True)
      assert(a === True)
    })
  }

  test("FormalApiTest.assumeInitial.symbiyosys") {
    runAssumeInitialTest(symbiYosys)
  }
  test("FormalApiTest.assumeInitial.symbiyosys.fail") {
    shouldFailWithOutput("Assert failed in AssumeInitialTest")(
      runAssumeInitialTest(symbiYosys, doFail = true)
    )
  }
  test("FormalApiTest.assumeInitial.ghdl") {
    runAssumeInitialTest(ghdl)
  }
  test("FormalApiTest.assumeInitial.ghdl.fail") {
    shouldFailWithOutput("Assert failed in AssumeInitialTest")(
      runAssumeInitialTest(ghdl, doFail = true)
    )
  }

  /** Test the generation and verification of cover statement.
    *
    * @param backend formal backend to use, i.e. use SymbiYosys or GHDL
    * @param doFail  force cover statement to be non-reachable
    */
  def runCoverTest(backend: SpinalFormalBackendSel, doFail: Boolean = false): Unit = {
    val config = FormalConfig.copy(_backend = backend).withCover(3)

    config.doVerify(new Component {
      setDefinitionName("CoverTest")
      setFormalTester()

      val a = Bool()
      if (!doFail) {
        anyconst(a)
      } else {
        a := False
      }
      cover(a === True)
    })
  }

  test("FormalApiTest.cover.symbiyosys") {
    runCoverTest(symbiYosys)
  }
  test("FormalApiTest.cover.symbiyosys.fail") {
    shouldFailWithOutput("Unreached cover statement")(
      runCoverTest(symbiYosys, doFail = true)
    )
  }
  test("FormalApiTest.cover.ghdl") {
    runCoverTest(ghdl)
  }
  test("FormalApiTest.cover.ghdl.fail") {
    shouldFailWithOutput("Unreached cover statement")(
      runCoverTest(ghdl, doFail = true)
    )
  }

  /** Test the generation and verification of assert statement with appropriate
    * (a)synchronous reset condition.
    *
    * This test works by asserting on false, which should immediately trigger an
    * assert. But as the assert is inactive during reset, we assume (in the
    * passing case) to be in constant reset. The assert may never trigger.
    *
    * For the failing case, the reset is only held for the initial cycle. The
    * following cycle the assert must trigger.
    *
    * @param backend formal backend to use, i.e. use SymbiYosys or GHDL
    * @param doFail  false = constant reset; true = initial reset
    */
  def runAssertUnderResetTest(backend: SpinalFormalBackendSel, doFail: Boolean = false): Unit = {
    val config = FormalConfig.copy(_backend = backend).withBMC(3)

    config.doVerify(new Component {
      setDefinitionName("AssertUnderResetTest")
      setFormalTester()

      if (!doFail) {
        assume(clockDomain.isResetActive)
      } else {
        assumeInitial(clockDomain.isResetActive)
      }
      assert(False)
    })
  }

  test("FormalApiTest.assertUnderReset.symbiyosys") {
    runAssertUnderResetTest(symbiYosys)
  }
  test("FormalApiTest.assertUnderReset.symbiyosys.fail") {
    shouldFailWithOutput("Checking assertions in step 1..")( // must not fail in first clock
      runAssertUnderResetTest(symbiYosys, doFail = true)
    )
  }
  test("FormalApiTest.assertUnderReset.ghdl") { // fails
    runAssertUnderResetTest(ghdl)
  }
  test("FormalApiTest.assertUnderReset.ghdl.fail") {
    shouldFailWithOutput("Checking assertions in step 1..")( // must not fail in first clock
      runAssertUnderResetTest(ghdl, doFail = true)
    )
  }

  /** Test the generation and verification of assert statement outside the
    * usual (a)synchronous reset condition.
    *
    * The test works by always (even during reset) asserting on false, which
    * should obviously immediately trigger. It then assumes the reset to be
    * always active. If implementation disables the assert during resets it will
    * not raise exception. Hence an implementation that raises an exception is
    * passing.
    *
    * @note Test has no failing case, raising an exception (assert triggered) is
    *       passing the test.
    *
    * @param backend formal backend to use, i.e. use SymbiYosys or GHDL
    */
  def runAssertWithoutResetTest(backend: SpinalFormalBackendSel): Unit = {
    val config = FormalConfig.copy(_backend = backend).withBMC(3)

    config.doVerify(new Component {
      setDefinitionName("AssertWithoutResetTest")
      setFormalTester()

      assume(clockDomain.isResetActive)
      clockDomain.withoutReset() {
        assert(False)
      }
    })
  }

  test("FormalApiTest.assertWithoutReset.symbiyosys.fail") {
    shouldFailWithOutput("Assert failed in AssertWithoutResetTest") {
      runAssertWithoutResetTest(symbiYosys)
    }
  }
  test("FormalApiTest.assertWithoutReset.ghdl.fail") {
    shouldFailWithOutput("Assert failed in AssertWithoutResetTest") {
      runAssertWithoutResetTest(ghdl)
    }
  }

  /** Test the generation and verification of assert statement inside
    * conditional scope.
    *
    * This test works by wrapping an always failing assert into a when
    * conditional scope. In passing test, the when condition is always false,
    * hence the failing assert should not trigger. The condition of the failing
    * case is true and the assert should trigger.
    *
    * @param backend formal backend to use, i.e. use SymbiYosys or GHDL
    * @param doFail  false = condition is always false; true = always true
    */
  def runAssertWithinCondTest(backend: SpinalFormalBackendSel, doFail: Boolean = false): Unit = {
    val config = FormalConfig.copy(_backend = backend).withBMC(3)

    config.doVerify(new Component {
      setDefinitionName("AssertWithinCondTest")
      setFormalTester()

      when(Bool(doFail)) {
        when(True) {
          assert(False)
        } otherwise {
          assert(False)
        }
      }

      switch(Bool(doFail)) {
        is(True) {
          assert(False)
        }
      }
    })
  }

  test("FormalApiTest.assertWithinCond.symbiyosys") {
    runAssertWithinCondTest(symbiYosys)
  }
  test("FormalApiTest.assertWithinCond.symbiyosys.fail") {
    shouldFailWithOutput("Assert failed in AssertWithinCondTest") {
      runAssertWithinCondTest(symbiYosys, doFail = true)
    }
  }
  test("FormalApiTest.assertWithinCond.ghdl") {
    runAssertWithinCondTest(ghdl)
  }
  test("FormalApiTest.assertWithinCond.ghdl.fail") {
    shouldFailWithOutput("Assert failed in AssertWithinCondTest") {
      runAssertWithinCondTest(ghdl, doFail = true)
    }
  }

  /** Test the generation and verification of statements within multiple clock
    * domains.
    *
    * @param backend formal backend to use, i.e. use SymbiYosys or GHDL
    * @param doFail  false = assert shall not trigger; true = will trigger
    */
  def runWithinClockDomainTest(backend: SpinalFormalBackendSel, doFail: Boolean = false): Unit = {
    val config = FormalConfig.copy(_backend = backend).withBMC(3)

    config.doVerify(new Component {
      setDefinitionName("WithinClockDomain")
      setFormalTester()

      val domainA = ClockDomain.internal("a")
      val domainB = ClockDomain.internal("b")

      val areaA = new ClockingArea(domainA) {
        assert(Bool(!doFail))
      }
      val areaB = new ClockingArea(domainB) {
        assert(Bool(!doFail))
      }
    })
  }

  test("FormalApiTest.withinClockDomain.symbiyosys") {
    runWithinClockDomainTest(symbiYosys)
  }
  test("FormalApiTest.withinClockDomain.symbiyosys.fail") {
    shouldFailWithOutput("Assert failed in WithinClockDomain") {
      runWithinClockDomainTest(symbiYosys, doFail = true)
    }
  }
  test("FormalApiTest.withinClockDomain.ghdl") {
    runWithinClockDomainTest(ghdl)
  }
  test("FormalApiTest.withinClockDomain.ghdl.fail") {
    shouldFailWithOutput("Assert failed in WithinClockDomain") {
      runWithinClockDomainTest(ghdl, doFail = true)
    }
  }

  /** Test the generation and verification of rose statements.
    *
    * @param backend formal backend to use, i.e. use SymbiYosys or GHDL
    * @param doFail  false = assume a value change; true = dont change value
    */
  def runRoseTest(backend: SpinalFormalBackendSel, doFail: Boolean = false): Unit = {
    val config = FormalConfig
      .copy(_backend = backend)
      .withBMC(3)
      .withEngies(Seq(SmtBmc(nopresat = true)))

    config.doVerify(new Component {
      setDefinitionName("RoseTest")
      setFormalTester()

      val a = Bool()
      anyseq(a)
      assumeInitial(!a)
      if (!doFail) { assume(a) }

      assert(formalRose(a))
    })
  }

  test("FormalApiTest.rose.symbiyosys") {
    runRoseTest(symbiYosys)
  }
  test("FormalApiTest.rose.symbiyosys.fail") {
    shouldFailWithOutput("Assert failed in RoseTest") {
      runRoseTest(symbiYosys, doFail = true)
    }
  }
  test("FormalApiTest.rose.ghdl") {
    runRoseTest(ghdl)
  }
  test("FormalApiTest.rose.ghdl.fail") {
    shouldFailWithOutput("Assert failed in RoseTest") {
      runRoseTest(ghdl, doFail = true)
    }
  }

  /** Test the generation and verification of fell statements.
    *
    * @param backend formal backend to use, i.e. use SymbiYosys or GHDL
    * @param doFail  false = assume a value change; true = dont change value
    */
  def runFellTest(backend: SpinalFormalBackendSel, doFail: Boolean = false): Unit = {
    val config = FormalConfig
      .copy(_backend = backend)
      .withBMC(3)
      .withEngies(Seq(SmtBmc(nopresat = true)))

    config.doVerify(new Component {
      setDefinitionName("FellTest")
      setFormalTester()

      val a = Bool()
      anyseq(a)
      assumeInitial(a)
      if (!doFail) { assume(!a) }

      assert(formalFell(a))
    })
  }

  test("FormalApiTest.fell.symbiyosys") {
    runFellTest(symbiYosys)
  }
  test("FormalApiTest.fell.symbiyosys.fail") {
    shouldFailWithOutput("Assert failed in FellTest") {
      runFellTest(symbiYosys, doFail = true)
    }
  }
  test("FormalApiTest.fell.ghdl") {
    runFellTest(ghdl)
  }
  test("FormalApiTest.fell.ghdl.fail") {
    shouldFailWithOutput("Assert failed in FellTest") {
      runFellTest(ghdl, doFail = true)
    }
  }

  /** Test the generation and verification of changed statements.
    *
    * @param backend formal backend to use, i.e. use SymbiYosys or GHDL
    * @param doFail  false = assume a value change; true = dont change value
    */
  def runChangedTest(backend: SpinalFormalBackendSel, doFail: Boolean = false): Unit = {
    val config = FormalConfig
      .copy(_backend = backend)
      .withBMC(3)
      .withEngies(Seq(SmtBmc(nopresat = true)))

    config.doVerify(new Component {
      setDefinitionName("ChangedTest")
      setFormalTester()

      val a = Bool()
      anyseq(a)
      assumeInitial(a)
      if (!doFail) { assume(!a) }

      assert(formalChanged(a))
    })
  }

  test("FormalApiTest.changed.symbiyosys") {
    runChangedTest(symbiYosys)
  }
  test("FormalApiTest.changed.symbiyosys.fail") {
    shouldFailWithOutput("Assert failed in ChangedTest") {
      runChangedTest(symbiYosys, doFail = true)
    }
  }
  test("FormalApiTest.changed.ghdl") {
    runChangedTest(ghdl)
  }
  test("FormalApiTest.changed.ghdl.fail") {
    shouldFailWithOutput("Assert failed in ChangedTest") {
      runChangedTest(ghdl, doFail = true)
    }
  }

  /** Test the generation and verification of stable statements.
    *
    * @param backend formal backend to use, i.e. use SymbiYosys or GHDL
    * @param doFail  false = keep constant value; true = change value
    */
  def runStableTest(backend: SpinalFormalBackendSel, doFail: Boolean = false): Unit = {
    val config = FormalConfig.copy(_backend = backend).withBMC(3)

    config.doVerify(new Component {
      setDefinitionName("StableTest")
      setFormalTester()

      val a = Bool()
      if (!doFail) {
        anyconst(a)
      } else {
        anyseq(a)
      }

      assumeInitial(clockDomain.isResetActive)
      assert(formalStable(a))
    })
  }

  test("FormalApiTest.stable.symbiyosys") {
    runStableTest(symbiYosys)
  }
  test("FormalApiTest.stable.symbiyosys.fail") {
    shouldFailWithOutput("Assert failed in StableTest") {
      runStableTest(symbiYosys, doFail = true)
    }
  }
  test("FormalApiTest.stable.ghdl") {
    runStableTest(ghdl)
  }
  test("FormalApiTest.stable.ghdl.fail") {
    shouldFailWithOutput("Assert failed in StableTest") {
      runStableTest(ghdl, doFail = true)
    }
  }

  /** Test the generation and verification of inistate statements.
    *
    * @param backend formal backend to use, i.e. use SymbiYosys or GHDL
    * @param doFail  false = keep constant value; true = change value
    */
  def runInitStateTest(backend: SpinalFormalBackendSel, doFail: Boolean = false): Unit = {
    val config = FormalConfig.copy(_backend = backend).withBMC(3)

    config.doVerify(new Component {
      setDefinitionName("InitStateTest")
      setFormalTester()

      clockDomain.withBootReset() {
        val a = Reg(Bool()) init (False)
        a := Bool(!doFail)

        when(initstate()) {
          assert(!a)
        } otherwise {
          assert(a)
        }
      }
    })
  }

  test("FormalApiTest.initstate.symbiyosys") {
    runInitStateTest(symbiYosys)
  }
  test("FormalApiTest.initstate.symbiyosys.fail") {
    shouldFailWithOutput("Assert failed in InitStateTest") {
      runInitStateTest(symbiYosys, doFail = true)
    }
  }
  test("FormalApiTest.initstate.ghdl") {
    runInitStateTest(ghdl)
  }
  test("FormalApiTest.initstate.ghdl.fail") {
    shouldFailWithOutput("Assert failed in InitStateTest") {
      runInitStateTest(ghdl, doFail = true)
    }
  }

  /** Test the generation and verification of past statements.
    *
    * @param backend formal backend to use, i.e. use SymbiYosys or GHDL
    * @param doFail  false = keep constant value; true = change value
    */
  def runPastTest(backend: SpinalFormalBackendSel, doFail: Boolean = false): Unit = {
    val config = FormalConfig.copy(_backend = backend).withBMC(3)

    config.doVerify(new Component {
      setDefinitionName("PastTest")
      setFormalTester()

      clockDomain.withBootReset() {
        val a = Reg(Bool()) init (False)
        if (!doFail) {
          a := !a
        }
        when(!initstate()) {
          assert(!a === formalPast(a, 1))
        }
      }
    })
  }

  test("FormalApiTest.past.symbiyosys") {
    runPastTest(symbiYosys)
  }
  test("FormalApiTest.past.symbiyosys.fail") {
    shouldFailWithOutput("Assert failed in PastTest") {
      runPastTest(symbiYosys, doFail = true)
    }
  }
  test("FormalApiTest.past.ghdl") {
    runPastTest(ghdl)
  }
  test("FormalApiTest.past.ghdl.fail") {
    shouldFailWithOutput("Assert failed in PastTest") {
      runPastTest(ghdl, doFail = true)
    }
  }
}
