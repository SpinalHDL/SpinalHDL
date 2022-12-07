package spinal.tester

import spinal.core._

import org.scalatest.funsuite.AnyFunSuite

case class BBA(cd: ClockDomain) extends BlackBox {
  val i = in port Bool()
}

case class BBB(cd: ClockDomain) extends BlackBox {
  val o = out port Bool()
}

case class CrossClockA() extends Component {
  val clkA = ClockDomain.external("clkA")
  val clkB = ClockDomain.external("clkB")

  val reg = clkA(RegNext(in port Bool()))

  val bb = BBA(clkB)
  bb.i := reg
}

case class CrossClockB() extends Component {
  val clkA = ClockDomain.external("clkA")
  val clkB = ClockDomain.external("clkB")

  val reg = in port Bool()

  val bb = BBA(clkB)
  bb.i := reg
}

case class CrossClockC() extends Component {
  val clkA = ClockDomain.external("clkA")
  val clkB = ClockDomain.external("clkB")

  val reg = out port Bool()

  val bb = BBB(clkB)
  bb.o <> reg
}

class CrossClockCheckerTester extends AnyFunSuite {
  test("a") {
    GenerationShould pass CrossClockA()
  }

  test("b") {
    GenerationShould fail {
      val c = CrossClockA()
      c.bb.i.addTag(ClockDomainTag(c.clkB))
      c
    }
  }

  test("c") {
    GenerationShould fail {
      val c = CrossClockB()
      c.reg.addTag(ClockDomainTag(c.clkA))
      c.bb.i.addTag(ClockDomainTag(c.clkB))
      c
    }
  }

  test("d") {
    GenerationShould fail {
      val c = CrossClockC()
      c.reg.addTag(ClockDomainTag(c.clkA))
      c.bb.o.addTag(ClockDomainTag(c.clkB))
      c
    }
  }
}

case class SyncronousA() extends Component {
  val ck, rst = in  port Bool()
  val input   = in  port UInt(8 bits)
  val output  = out port UInt(8 bits)

  val cd = ClockDomain(ck, rst)
  val logic = cd on new Area {
    val a = RegNext(input)
    val b = RegNext(a)
    output := b
  }
}

case class SyncronousB() extends Component {
  val ck1, rst = in  port Bool()
  val input    = in  port UInt(8 bits)
  val output   = out port UInt(8 bits)

  val ck2 = CombInit(ck1)

  val cd1 = ClockDomain(ck1, rst)
  val cd2 = ClockDomain(ck2, rst)

  val logic1 = cd1 on new Area {
    val a = RegNext(input)
    val b = RegNext(a)
    output := b
  }

  val logic2 = cd2 on new Area {
    val a = out port RegNext(logic1.a)
  }
}

case class SyncronousC(v: Int) extends Component {
  val ck1, ck2, rst = in  port Bool()
  val input         = in  port UInt(8 bits)
  val output        = out port UInt(8 bits)

  val clk1Buf = List.fill(3)(Bool()).foldLeft(ck1) { (i, o) => o := i; o }
  val clk2Buf = List.fill(3)(Bool()).foldLeft(ck2) { (i, o) => o := i; o }

  val cd1 = ClockDomain(clk1Buf, rst)
  val cd2 = ClockDomain(clk2Buf, rst)
  v match {
    case 0 =>
    case 1 => cd1.setSyncWith(cd2)
    case 2 => cd2.setSyncWith(cd1)
    case 3 => ClockDomain(ck1).setSyncWith(cd2)
    case 4 => ClockDomain(ck2).setSyncWith(cd1)
    case 5 => ClockDomain(ck1).setSyncWith(ClockDomain(ck2))
  }

  val logic1 = cd1 on new Area {
    val a = RegNext(input)
    val b = RegNext(a)
    output := b
  }

  val logic2 = cd2 on new Area {
    val a = out port RegNext(logic1.a)
  }
}

case class SyncronousCheckerTesterD(v: Int) extends Component {
  val ck1, rst = in  port Bool()
  val input    = in  port UInt(8 bits)
  val output   = out port UInt(8 bits)

  val ck2 = ck1 && input === 0

  val cd1 = ClockDomain(ck1, rst)
  val cd2 = ClockDomain(ck2, rst)

  v match {
    case 0 =>
    case 1 => cd1.setSyncWith(cd2)
    case 2 => Clock.syncDrive(source = ck1, sink = ck2)
  }

  val logic1 = cd1 on new Area {
    val a = RegNext(input)
    val b = RegNext(a)
    output := b
  }

  val logic2 = cd2 on new Area {
    val a = out port RegNext(logic1.a)
  }
}

class SyncronousCheckerTester extends AnyFunSuite {
  test("a") { GenerationShould pass SyncronousA() }
  test("b") { GenerationShould pass SyncronousB() }
  test("c0") { GenerationShould fail SyncronousC(0) }
  test("c1") { GenerationShould pass SyncronousC(1) }
  test("c2") { GenerationShould pass SyncronousC(2) }
  test("c3") { GenerationShould pass SyncronousC(3) }
  test("c4") { GenerationShould pass SyncronousC(4) }
  test("c5") { GenerationShould pass SyncronousC(5) }
  test("d0") { GenerationShould fail SyncronousCheckerTesterD(0) }
  test("d1") { GenerationShould pass SyncronousCheckerTesterD(1) }
  test("d2") { GenerationShould pass SyncronousCheckerTesterD(2) }
//  test("d3") { GenerationShould fail (SyncronousCheckerTesterD(3)) }
}
