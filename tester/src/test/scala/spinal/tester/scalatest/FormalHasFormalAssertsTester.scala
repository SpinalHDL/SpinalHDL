package spinal.tester.scalatest

import spinal.core._
import spinal.lib._
import spinal.core.formal.{FormalConfig, FormalDut, anyseq}
import spinal.lib.formal.{ComponentWithFormalAsserts, FormalMasterSlave, HasFormalAsserts}
import spinal.tester.SpinalFormalFunSuite

import scala.language.postfixOps

case class TestBundle() extends Bundle with IMasterSlave with FormalMasterSlave {
  val input = Bits(32 bits)
  val never_true = Bool()

  override def asMaster(): Unit = {
    out(input)
    in(never_true)
  }

  override def formalIsProducerValid(): Bool = input =/= 0xdeadbeefL
  override def formalIsConsumerValid() = never_true === False
}

class ChildComponent extends ComponentWithFormalAsserts {
  val io = new Bundle {
    val contract = slave(TestBundle())
  }

  io.contract.never_true := io.contract.input === 0xdeadbeefL
}

class FormalChildComponent extends Component {
  val dut = FormalDut(new ChildComponent())
  assumeInitial(ClockDomain.current.isResetActive)

  dut.anyseq_inputs()
}

class ParentComponent extends Component with HasFormalAsserts {
  val io = new Bundle {
    val contract = slave(TestBundle())
  }

  val child = new ChildComponent()
  child.io <> io

  override lazy val formalValidInputs = child.formalValidInputs
}

class FormalParentComponent(assumeInputs : Boolean = true) extends Component {
  val dut = FormalDut(new ParentComponent())
  assumeInitial(ClockDomain.current.isResetActive)

  dut.formalAsserts()
  if(assumeInputs) {
    dut.formalAssumeInputs()
  }

  anyseq(dut.io.contract.input)
}

class FormalHasFormalTester extends SpinalFormalFunSuite {
  test("Check child") {
    FormalConfig.withBMC(15).doVerify(new FormalChildComponent())
  }
  test("Check Parent") {
    FormalConfig.withBMC(15).doVerify(new FormalParentComponent())
  }

  // Shows that if we don't assume proper inputs into the parent, that the assumptions in the child class are correctly
  // predicated on the inputs being correct
  test("Check Parent - no input assumes") {
    shouldFail(
      FormalConfig.withBMC(15).doVerify(new FormalParentComponent(false))
    )
  }
}