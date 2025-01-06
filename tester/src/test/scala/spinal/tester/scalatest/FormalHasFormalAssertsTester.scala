package spinal.tester.scalatest

import spinal.core._
import spinal.lib._
import spinal.core.formal.{FormalConfig, FormalDut, HasFormalAsserts, anyseq}
import spinal.tester.SpinalFormalFunSuite

import scala.language.postfixOps

class ChildComponent extends Component with HasFormalAsserts {
  val io = new Bundle {
    val input = in(Bits(32 bits))
    val never_true = out(Bool())
  }

  io.never_true := io.input === 0xdeadbeefL

  override lazy val formalValidInputs = io.input =/= 0xdeadbeefL

  override def formalAsserts()(implicit useAssumes: Boolean) = new Composite(this, "formalAsserts") {
    assertOrAssume(io.never_true === False)
  }
}

class FormalChildComponent extends Component {
  val dut = FormalDut(new ChildComponent())
  assumeInitial(ClockDomain.current.isResetActive)

  dut.formalAssumeInputs()

  anyseq(dut.io.input)
}

class ParentComponent extends Component with HasFormalAsserts {
  val io = new Bundle {
    val input = in(Bits(32 bits))
    val never_true = out(Bool())
  }

  val child = new ChildComponent()
  child.io <> io

  override lazy val formalValidInputs = child.formalValidInputs

  /**
   * Set o formal assertions required for testing and validating the component completely.
   *
   * @param useAssumes indicates that we want to assume all the predicates are always true; which informs inductive
   *                   provers which states are acceptable.
   * @return An area (typically a composite) which may contain signals useful for collectign during a simulation
   */
  override def formalAsserts()(implicit useAssumes: Boolean) = new Composite(this, "formalAsserts") {
    child.formalAssertInputs()
    child.formalAssumes()
  }
}

class FormalParentComponent(assumeInputs : Boolean = true) extends Component {
  val dut = FormalDut(new ParentComponent())
  assumeInitial(ClockDomain.current.isResetActive)

  if(assumeInputs) {
    dut.formalAssumeInputs()
  }

  anyseq(dut.io.input)
}

class FormalHasFormalTester extends SpinalFormalFunSuite {
  test("Check child") {
    FormalConfig.withDebug.withBMC(15).doVerify(new FormalChildComponent())
  }
  test("Check Parent") {
    FormalConfig.withDebug.withBMC(15).doVerify(new FormalParentComponent())
  }

  // Shows that if we don't assume proper inputs into the parent, that the assumptions in the child class are correctly
  // predicated on the inputs being correct
  test("Check Parent - no input assumes") {
    shouldFail(
      FormalConfig.withDebug.withBMC(15).doVerify(new FormalParentComponent(false))
    )
  }
}