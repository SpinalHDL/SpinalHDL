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

  override def formalChecks()(implicit useAssumes: Boolean): Unit =
    assertOrAssume(io.never_true === False)
}

class FormalChildComponent extends Component {
  val dut = FormalDut(new ChildComponent())
  assumeInitial(ClockDomain.current.isResetActive)

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
}

class FormalParentComponent(assumeInputs : Boolean = true) extends Component {
  withAutoPull()
  setFormalTester()

  val dut = new ParentComponent()
  assumeInitial(ClockDomain.current.isResetActive)

  dut.formalAsserts()
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