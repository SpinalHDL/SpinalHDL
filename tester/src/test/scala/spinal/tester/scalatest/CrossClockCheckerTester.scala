package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._


class BBA(val cd : ClockDomain) extends BlackBox{
  val i = in(Bool)
}

class BBB(val cd : ClockDomain) extends BlackBox{
  val o = out(Bool)
}

class CrossClockCheckerTesterA extends Component{
  val clkA = ClockDomain.external("clkA")
  val clkB = ClockDomain.external("clkB")

  val reg = clkA(RegNext(in Bool))

  val bb = new BBA(clkB)
  bb.i := reg
}


class CrossClockCheckerTesterB extends Component{
  val clkA = ClockDomain.external("clkA")
  val clkB = ClockDomain.external("clkB")

  val reg = in Bool()

  val bb = new BBA(clkB)
  bb.i := reg
}

class CrossClockCheckerTesterC extends Component{
  val clkA = ClockDomain.external("clkA")
  val clkB = ClockDomain.external("clkB")

  val reg = out Bool()

  val bb = new BBB(clkB)
  bb.o <> reg
}


class CrossClockCheckerTester extends FunSuite{
  import CheckTester._

  test("a") {
    generationShouldPass(new CrossClockCheckerTesterA)
  }

  test("b") {
    generationShouldFaild({
      val c = new CrossClockCheckerTesterA
      c.bb.i.addTag(ClockDomainTag(c.clkB))
      c
    })
  }

  test("c") {
    generationShouldFaild({
      val c = new CrossClockCheckerTesterB
      c.reg.addTag(ClockDomainTag(c.clkA))
      c.bb.i.addTag(ClockDomainTag(c.clkB))
      c
    })
  }

  test("d") {
    generationShouldFaild({
      val c = new CrossClockCheckerTesterC
      c.reg.addTag(ClockDomainTag(c.clkA))
      c.bb.o.addTag(ClockDomainTag(c.clkB))
      c
    })
  }
}
