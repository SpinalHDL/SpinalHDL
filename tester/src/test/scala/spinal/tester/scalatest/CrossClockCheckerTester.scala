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


class SyncronousCheckerTesterA extends Component{
  val clk,reset = in Bool()
  val input = in UInt(8 bits)
  val output = out UInt(8 bits)

  val cd = ClockDomain(clk, reset)
  val logic = cd on new Area{
    val a = RegNext(input)
    val b = RegNext(a)
    output := b
  }
}

class SyncronousCheckerTesterB extends Component{
  val clk1,reset = in Bool()
  val input = in UInt(8 bits)
  val output = out UInt(8 bits)


  val clk2 = CombInit(clk1)

  val cd1 = ClockDomain(clk1, reset)
  val cd2 = ClockDomain(clk2, reset)

  val logic1 = cd1 on new Area{
    val a = RegNext(input)
    val b = RegNext(a)
    output := b
  }

  val logic2 = cd2 on new Area{
    val a = out(RegNext(logic1.a))

  }
}

class SyncronousCheckerTesterC(v : Int) extends Component{
  val clk1, clk2, reset = in Bool()
  val input = in UInt(8 bits)
  val output = out UInt(8 bits)

  val clk1Buf = List.fill(3)(Bool).foldLeft(clk1){(i,o) => o := i; o}
  val clk2Buf = List.fill(3)(Bool).foldLeft(clk2){(i,o) => o := i; o}

  val cd1 = ClockDomain(clk1Buf, reset)
  val cd2 = ClockDomain(clk2Buf, reset)
  v match{
    case 0 =>
    case 1 => cd1.setSyncWith(cd2)
    case 2 => cd2.setSyncWith(cd1)
    case 3 => ClockDomain(clk1).setSyncWith(cd2)
    case 4 => ClockDomain(clk2).setSyncWith(cd1)
    case 5 => ClockDomain(clk1).setSyncWith(ClockDomain(clk2))
  }

  val logic1 = cd1 on new Area{
    val a = RegNext(input)
    val b = RegNext(a)
    output := b
  }

  val logic2 = cd2 on new Area{
    val a = out(RegNext(logic1.a))
  }
}


class SyncronousCheckerTesterD(v : Int) extends Component{
  val clk1,reset = in Bool()
  val input = in UInt(8 bits)
  val output = out UInt(8 bits)


  val clk2 = clk1 && input === 0

  val cd1 = ClockDomain(clk1, reset)
  val cd2 = ClockDomain(clk2, reset)

  v match {
    case 0 =>
    case 1 => cd1.setSyncWith(cd2)
    case 2 => Clock.syncDrive(source = clk1, sink = clk2)
  }

  val logic1 = cd1 on new Area{
    val a = RegNext(input)
    val b = RegNext(a)
    output := b
  }

  val logic2 = cd2 on new Area{
    val a = out(RegNext(logic1.a))
  }
}

class SyncronousCheckerTester extends FunSuite{
  import CheckTester._

  test("a") { generationShouldPass(new SyncronousCheckerTesterA) }
  test("b") { generationShouldPass(new SyncronousCheckerTesterB) }
  test("c0") { generationShouldFaild(new SyncronousCheckerTesterC(0)) }
  test("c1") { generationShouldPass(new SyncronousCheckerTesterC(1)) }
  test("c2") { generationShouldPass(new SyncronousCheckerTesterC(2)) }
  test("c3") { generationShouldPass(new SyncronousCheckerTesterC(3)) }
  test("c4") { generationShouldPass(new SyncronousCheckerTesterC(4)) }
  test("c5") { generationShouldPass(new SyncronousCheckerTesterC(5)) }
  test("d0") { generationShouldFaild(new SyncronousCheckerTesterD(0)) }
  test("d1") { generationShouldPass(new SyncronousCheckerTesterD(1)) }
  test("d2") { generationShouldPass(new SyncronousCheckerTesterD(2)) }
//  test("d3") { generationShouldFaild(new SyncronousCheckerTesterD(3)) }
}
