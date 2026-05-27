package spinal.lib

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

/** Behavior tests for every counter encoding, direction, and boundary-policy
  * permutation. Each test builds its own Component so failures are isolated;
  * scenario dimensions are iterated in Scala loops and registered as individual
  * test cases.
  */
class CounterTester extends AnyFunSuite {

  private def waitOutOfReset(dut: Component): Unit = dut.clockDomain.waitSampling(10)

  private val policies = Seq(BoundaryPolicy.Wrap, BoundaryPolicy.Saturate, BoundaryPolicy.Freeze)

  // Drive `sig` high for `n` cycles, then low, then advance one more cycle so
  // the final value has propagated through the RegNext.
  private def pulse(cd: ClockDomain, sig: Bool, n: Int = 1): Unit = {
    sig #= true
    cd.waitSampling(n)
    sig #= false
    cd.waitSampling()
  }

  // Sample `read` on each of the next `n` clock edges.
  private def collect[T](cd: ClockDomain, n: Int)(read: => T): Seq[T] =
    (0 until n).map(_ => { cd.waitSampling(); read })

  // Assert that elaborating the component body throws.
  private def expectElaborationFailure(body: => Component): Unit = {
    intercept[Exception] { SimConfig.compile(body) }
    ()
  }

  private class BidirDut(upper: BoundaryPolicy, lower: BoundaryPolicy, end: Int) extends Component {
    val w = log2Up(end + 1)
    val io = new Bundle {
      val inc     = in  Bool()
      val dec     = in  Bool()
      val clr     = in  Bool()
      val loadIt  = in  Bool()
      val loadVal = in  UInt(w bits)
      val v       = out UInt(w bits)
      val frozen  = out Bool()
      val satHi   = out Bool()
      val satLo   = out Bool()
    }
    val c = new Counter(0, end, upper = upper, lower = lower, direction = CounterDirection.Both)
    when(io.inc)    { c.increment() }
    when(io.dec)    { c.decrement() }
    when(io.clr)    { c.clear() }
    when(io.loadIt) { c.load(io.loadVal) }
    io.v      := c.value
    io.frozen := c.frozen
    io.satHi  := c.saturatedHigh
    io.satLo  := c.saturatedLow
  }

  private def withBidirDut(upper: BoundaryPolicy, lower: BoundaryPolicy, end: Int = 15)
                          (body: BidirDut => Unit): Unit = {
    SimConfig.compile(new BidirDut(upper, lower, end)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc     #= false
      dut.io.dec     #= false
      dut.io.clr     #= false
      dut.io.loadIt  #= false
      dut.io.loadVal #= 0
      waitOutOfReset(dut)
      body(dut)
    }
  }

  private class UpDut(upper: BoundaryPolicy, end: Int) extends Component {
    val w = log2Up(end + 1)
    val io = new Bundle {
      val inc     = in  Bool()
      val clr     = in  Bool()
      val loadIt  = in  Bool()
      val loadVal = in  UInt(w bits)
      val v       = out UInt(w bits)
      val frozen  = out Bool()
      val satHi   = out Bool()
    }
    val c = new Counter(0, end, upper = upper)
    when(io.inc)    { c.increment() }
    when(io.clr)    { c.clear() }
    when(io.loadIt) { c.load(io.loadVal) }
    io.v      := c.value
    io.frozen := c.frozen
    io.satHi  := c.saturatedHigh
  }

  private def withUpDut(upper: BoundaryPolicy = BoundaryPolicy.Wrap, end: Int = 15)
                       (body: UpDut => Unit): Unit = {
    SimConfig.compile(new UpDut(upper, end)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc     #= false
      dut.io.clr     #= false
      dut.io.loadIt  #= false
      dut.io.loadVal #= 0
      waitOutOfReset(dut)
      body(dut)
    }
  }

  private class DownDut(lower: BoundaryPolicy, end: Int) extends Component {
    val w = log2Up(end + 1)
    val io = new Bundle {
      val dec    = in  Bool()
      val clr    = in  Bool()
      val v      = out UInt(w bits)
      val frozen = out Bool()
      val satLo  = out Bool()
    }
    val c = new Counter(0, end, lower = lower, direction = CounterDirection.Down)
    when(io.dec) { c.decrement() }
    when(io.clr) { c.clear() }
    io.v      := c.value
    io.frozen := c.frozen
    io.satLo  := c.saturatedLow
  }

  private def withDownDut(lower: BoundaryPolicy = BoundaryPolicy.Wrap, end: Int = 15)
                         (body: DownDut => Unit): Unit = {
    SimConfig.compile(new DownDut(lower, end)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.dec #= false
      dut.io.clr #= false
      waitOutOfReset(dut)
      body(dut)
    }
  }

  for (stateCount <- Seq(4, 7, 8, 10, 16)) {
    test(s"Counter[Up] stateCount=$stateCount Wrap: full cycle traversal") {
      SimConfig.compile(new Component {
        val io = new Bundle {
          val inc   = in  Bool()
          val value = out UInt(log2Up(stateCount) bits)
        }
        val c = Counter(stateCount)
        when(io.inc) { c.increment() }
        io.value := c.value
      }).doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.io.inc #= false
        waitOutOfReset(dut)
        assert(dut.io.value.toInt == 0, "init at 0")
        dut.io.inc #= true
        dut.clockDomain.waitSampling()   // burn: inc change takes effect next edge
        for (i <- 1 to stateCount * 3) {
          dut.clockDomain.waitSampling()
          val expected = i % stateCount
          assert(dut.io.value.toInt == expected,
            s"stateCount=$stateCount cycle $i: expected $expected got ${dut.io.value.toInt}")
        }
      }
    }
  }

  test("Counter[Up] Wrap cycles correctly at stateCount boundary") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc   = in  Bool()
        val value = out UInt(4 bits)
      }
      val c = Counter(16)
      when(io.inc) { c.increment() }
      io.value := c.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= false
      waitOutOfReset(dut)
      assert(dut.io.value.toInt == 0, "init")
      dut.io.inc #= true
      dut.clockDomain.waitSampling()
      for (expected <- Seq(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0,1,2)) {
        dut.clockDomain.waitSampling()
        assert(dut.io.value.toInt == expected, s"expected $expected got ${dut.io.value.toInt}")
      }
    }
  }

  test("Counter[Up] Saturate pins at end") {
    withUpDut(BoundaryPolicy.Saturate) { dut =>
      dut.io.inc #= true
      dut.clockDomain.waitSampling(40)
      assert(dut.io.v.toInt == 15)
      assert(dut.io.satHi.toBoolean)
    }
  }

  test("Counter[Up] Freeze latches and clear releases") {
    withUpDut(BoundaryPolicy.Freeze) { dut =>
      dut.io.inc #= true
      dut.clockDomain.waitSampling(40)
      assert(dut.io.v.toInt == 15)
      assert(dut.io.frozen.toBoolean)
      dut.clockDomain.waitSampling(10)
      assert(dut.io.v.toInt == 15, "should hold at 15 while frozen")
      dut.io.inc #= false
      pulse(dut.clockDomain, dut.io.clr)
      assert(dut.io.v.toInt == 0)
      assert(!dut.io.frozen.toBoolean)
    }
  }

  test("Counter[Up] Freeze: load escapes the latch") {
    withUpDut(BoundaryPolicy.Freeze) { dut =>
      dut.io.inc #= true
      dut.clockDomain.waitSampling(30)
      assert(dut.io.frozen.toBoolean)
      dut.io.inc #= false
      dut.io.loadVal #= 7
      pulse(dut.clockDomain, dut.io.loadIt)
      assert(dut.io.v.toInt == 7)
      assert(!dut.io.frozen.toBoolean)
    }
  }

  test("Counter[Up] with non-zero start/end: [5, 12] wraps correctly") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc = in  Bool()
        val v   = out UInt(4 bits)
      }
      val c = new Counter(5, 12)
      when(io.inc) { c.increment() }
      io.v := c.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= false
      waitOutOfReset(dut)
      assert(dut.io.v.toInt == 5, "init at start=5")
      dut.io.inc #= true
      dut.clockDomain.waitSampling()
      val seen = collect(dut.clockDomain, 20) { dut.io.v.toInt }
      val expected = Seq(6,7,8,9,10,11,12,5,6,7,8,9,10,11,12,5,6,7,8,9)
      assert(seen == expected, s"got $seen expected $expected")
    }
  }

  test("Counter[Down] Wrap starts at end and wraps at 0") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val dec = in  Bool()
        val v   = out UInt(4 bits)
      }
      val c = Counter.down(16)
      when(io.dec) { c.decrement() }
      io.v := c.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.dec #= false
      waitOutOfReset(dut)
      assert(dut.io.v.toInt == 15, "init at end=15")
      dut.io.dec #= true
      dut.clockDomain.waitSampling()
      val seen = collect(dut.clockDomain, 18) { dut.io.v.toInt }
      assert(seen == Seq(14,13,12,11,10,9,8,7,6,5,4,3,2,1,0,15,14,13))
    }
  }

  test("Counter[Down] Saturate pins at start=0") {
    withDownDut(BoundaryPolicy.Saturate) { dut =>
      dut.io.dec #= true
      dut.clockDomain.waitSampling(40)
      assert(dut.io.v.toInt == 0)
      assert(dut.io.satLo.toBoolean)
    }
  }

  test("Counter[Down] Freeze latches and clear returns to end") {
    withDownDut(BoundaryPolicy.Freeze) { dut =>
      dut.io.dec #= true
      dut.clockDomain.waitSampling(40)
      assert(dut.io.v.toInt == 0)
      assert(dut.io.frozen.toBoolean)
      dut.io.dec #= false
      pulse(dut.clockDomain, dut.io.clr)
      assert(dut.io.v.toInt == 15, "clear returns to end")
      assert(!dut.io.frozen.toBoolean)
    }
  }

  for (upper <- policies; lower <- policies) {
    test(s"Counter[Both] upper=$upper lower=$lower: basic advance in both directions") {
      withBidirDut(upper, lower) { dut =>
        assert(dut.io.v.toInt == 0, "init at 0")
        pulse(dut.clockDomain, dut.io.inc, 3)
        assert(dut.io.v.toInt == 3, s"after 3 inc, got ${dut.io.v.toInt}")
        pulse(dut.clockDomain, dut.io.dec, 2)
        assert(dut.io.v.toInt == 1, s"after 2 dec, got ${dut.io.v.toInt}")
      }
    }
  }

  for (upper <- policies; lower <- policies) {
    test(s"Counter[Both] upper=$upper lower=$lower: upper boundary behavior") {
      withBidirDut(upper, lower) { dut =>
        dut.io.inc #= true
        dut.clockDomain.waitSampling(30)
        val v = dut.io.v.toInt
        upper match {
          case BoundaryPolicy.Wrap     => assert(v >= 0 && v < 16)
          case BoundaryPolicy.Saturate => assert(v == 15, s"Saturate should pin at 15, got $v")
          case BoundaryPolicy.Freeze   => assert(v == 15, s"Freeze should hold at 15, got $v")
        }
        if (upper == BoundaryPolicy.Freeze) {
          dut.io.inc #= false
          pulse(dut.clockDomain, dut.io.clr)
          assert(dut.io.v.toInt == 0, "clear should return to start")
        }
      }
    }
  }

  for (upper <- policies; lower <- policies) {
    test(s"Counter[Both] upper=$upper lower=$lower: lower boundary behavior") {
      withBidirDut(upper, lower) { dut =>
        dut.io.dec #= true
        dut.clockDomain.waitSampling(30)
        val v = dut.io.v.toInt
        lower match {
          case BoundaryPolicy.Wrap     => assert(v >= 0 && v < 16)
          case BoundaryPolicy.Saturate => assert(v == 0, s"Saturate should pin at 0, got $v")
          case BoundaryPolicy.Freeze   => assert(v == 0, s"Freeze should hold at 0, got $v")
        }
        if (lower == BoundaryPolicy.Freeze) {
          dut.io.dec #= false
          pulse(dut.clockDomain, dut.io.clr)
          assert(dut.io.v.toInt == 0, "clear should return to start")
        }
      }
    }
  }

  test("Counter[Both] Wrap both: full forward then backward traversal") {
    withBidirDut(BoundaryPolicy.Wrap, BoundaryPolicy.Wrap) { dut =>
      dut.io.inc #= true
      dut.clockDomain.waitSampling()
      val forward = collect(dut.clockDomain, 16) { dut.io.v.toInt }
      assert(forward == (1 to 15).toList ++ List(0))
      dut.io.inc #= false
      dut.io.dec #= true
      dut.clockDomain.waitSampling(2)   // handoff cycle
      val backward = collect(dut.clockDomain, 16) { dut.io.v.toInt }
      assert(backward == (15 to 0 by -1).toList)
    }
  }

  test("Counter[Both] Wrap both: simultaneous inc+dec holds value") {
    withBidirDut(BoundaryPolicy.Wrap, BoundaryPolicy.Wrap) { dut =>
      pulse(dut.clockDomain, dut.io.inc, 5)
      assert(dut.io.v.toInt == 5)
      dut.io.inc #= true
      dut.io.dec #= true
      dut.clockDomain.waitSampling(10)
      assert(dut.io.v.toInt == 5, "inc+dec should hold")
    }
  }

  test("OneHotCounter[Up] Wrap: rotates through one-hot states") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc = in  Bool()
        val v   = out Bits(8 bits)
      }
      val c = OneHotCounter(8, io.inc)
      io.v := c.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= false
      waitOutOfReset(dut)
      assert(dut.io.v.toInt == 1, "init one-hot at bit 0")
      dut.io.inc #= true
      dut.clockDomain.waitSampling()
      for (i <- 1 until 8) {
        dut.clockDomain.waitSampling()
        assert(dut.io.v.toInt == (1 << i), s"iter $i expected ${1 << i} got ${dut.io.v.toInt}")
      }
      dut.clockDomain.waitSampling()
      assert(dut.io.v.toInt == 1, "wraps to 1")
    }
  }

  test("OneHotCounter[Up] Saturate: pins at top bit") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc = in  Bool()
        val v   = out Bits(8 bits)
        val sat = out Bool()
      }
      val c = new OneHotCounter(8, upper = BoundaryPolicy.Saturate)
      when(io.inc) { c.increment() }
      io.v   := c.value
      io.sat := c.saturatedHigh
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= true
      waitOutOfReset(dut)
      dut.clockDomain.waitSampling(20)
      assert(dut.io.v.toInt == 0x80, s"should pin at MSB got 0x${dut.io.v.toInt.toHexString}")
      assert(dut.io.sat.toBoolean)
    }
  }

  test("OneHotCounter[Up] Freeze: latches on overflow") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc    = in  Bool()
        val clr    = in  Bool()
        val v      = out Bits(8 bits)
        val frozen = out Bool()
      }
      val c = new OneHotCounter(8, upper = BoundaryPolicy.Freeze)
      when(io.inc) { c.increment() }
      when(io.clr) { c.clear() }
      io.v      := c.value
      io.frozen := c.frozen
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.clr #= false
      dut.io.inc #= true
      waitOutOfReset(dut)
      dut.clockDomain.waitSampling(20)
      assert(dut.io.v.toInt == 0x80)
      assert(dut.io.frozen.toBoolean)
      dut.io.inc #= false
      pulse(dut.clockDomain, dut.io.clr)
      assert(dut.io.v.toInt == 1)
      assert(!dut.io.frozen.toBoolean)
    }
  }

  test("OneHotCounter[Both] rotates left on inc, right on dec") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc = in  Bool()
        val dec = in  Bool()
        val v   = out Bits(8 bits)
      }
      val c = OneHotCounter.both(8)
      when(io.inc) { c.increment() }
      when(io.dec) { c.decrement() }
      io.v := c.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= false
      dut.io.dec #= false
      waitOutOfReset(dut)
      assert(dut.io.v.toInt == 1)
      pulse(dut.clockDomain, dut.io.inc, 3)
      assert(dut.io.v.toInt == 8, "after 3 inc should be bit 3 = 0x8")
      pulse(dut.clockDomain, dut.io.dec, 2)
      assert(dut.io.v.toInt == 2, "after 2 dec should be bit 1")
    }
  }

  test("OneHotCounter === Int checks bit position") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc = in  Bool()
        val is2 = out Bool()
        val is5 = out Bool()
        val not3 = out Bool()
      }
      val c = OneHotCounter(8, io.inc)
      io.is2  := c === 2
      io.is5  := c === 5
      io.not3 := c =/= 3
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= false
      waitOutOfReset(dut)
      assert(!dut.io.is2.toBoolean, "bit 0 is set, not bit 2")
      assert(!dut.io.is5.toBoolean)
      assert(dut.io.not3.toBoolean, "not at bit 3")
      pulse(dut.clockDomain, dut.io.inc, 2)
      assert(dut.io.is2.toBoolean)
      assert(!dut.io.is5.toBoolean)
      assert(dut.io.not3.toBoolean)
      pulse(dut.clockDomain, dut.io.inc, 1)
      assert(!dut.io.is2.toBoolean)
      assert(!dut.io.not3.toBoolean, "now at bit 3")
      pulse(dut.clockDomain, dut.io.inc, 2)
      assert(dut.io.is5.toBoolean)
    }
  }

  test("OneHotCounter === UInt compares against runtime ordinal via UIntToOh") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc   = in  Bool()
        val probe = in  UInt(3 bits)
        val eq    = out Bool()
        val ne    = out Bool()
      }
      val c = OneHotCounter(8, io.inc)
      io.eq := c === io.probe
      io.ne := c =/= io.probe
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= false
      dut.io.probe #= 0
      waitOutOfReset(dut)
      assert(dut.io.eq.toBoolean)
      assert(!dut.io.ne.toBoolean)
      dut.io.probe #= 3
      dut.clockDomain.waitSampling()
      assert(!dut.io.eq.toBoolean)
      assert(dut.io.ne.toBoolean)
      pulse(dut.clockDomain, dut.io.inc, 3)
      assert(dut.io.eq.toBoolean, "at bit 3 with probe 3")
      assert(!dut.io.ne.toBoolean)
    }
  }

  test("OneHotCounter === Bits does raw bit-pattern equality") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc     = in  Bool()
        val pattern = in  Bits(8 bits)
        val eq      = out Bool()
        val ne      = out Bool()
      }
      val c = OneHotCounter(8, io.inc)
      io.eq := c === io.pattern
      io.ne := c =/= io.pattern
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= false
      dut.io.pattern #= 1   // bit 0 set
      waitOutOfReset(dut)
      assert(dut.io.eq.toBoolean, "bits 0x01 match")
      assert(!dut.io.ne.toBoolean)
      // change pattern to bit 4 set; counter still at bit 0
      dut.io.pattern #= 1 << 4
      dut.clockDomain.waitSampling()
      assert(!dut.io.eq.toBoolean)
      assert(dut.io.ne.toBoolean)
      // non-one-hot pattern (multiple bits set) should still compare correctly as raw Bits
      dut.io.pattern #= 0x0F
      dut.clockDomain.waitSampling()
      assert(!dut.io.eq.toBoolean)
      assert(dut.io.ne.toBoolean)
    }
  }

  test("OneHotCounter !== is same as =/= (legacy alias, explicit dispatch)") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc   = in  Bool()
        val probe = in  UInt(3 bits)
        val neInt  = out Bool()
        val neUInt = out Bool()
        val neBits = out Bool()
      }
      val c  = OneHotCounter(8, io.inc)
      // The `!==` operator on OneHotCounter shadows nothing and must be called as a method to
      // avoid Scala preferring the inherited `ImplicitArea[Bits]` → `Bits.!==` resolution.
      io.neInt  := c.!==(2)
      io.neUInt := c.!==(io.probe)
      io.neBits := c.!==(B(1, 8 bits))
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= false
      dut.io.probe #= 0
      waitOutOfReset(dut)
      assert(dut.io.neInt.toBoolean, "!== 2 when at bit 0 → true")
      assert(!dut.io.neUInt.toBoolean, "!== 0 when at bit 0 → false")
      assert(!dut.io.neBits.toBoolean, "!== B(1) when at bit 0 → false")
    }
  }

  test("Counter === UInt checks binary-value equality") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc   = in  Bool()
        val probe = in  UInt(4 bits)
        val eq    = out Bool()
        val ne    = out Bool()
      }
      val c = Counter(16, io.inc)
      io.eq := c === io.probe
      io.ne := c =/= io.probe
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= false
      dut.io.probe #= 0
      waitOutOfReset(dut)
      assert(dut.io.eq.toBoolean)
      assert(!dut.io.ne.toBoolean)
      pulse(dut.clockDomain, dut.io.inc, 5)
      dut.io.probe #= 5
      dut.clockDomain.waitSampling()
      assert(dut.io.eq.toBoolean)
      assert(!dut.io.ne.toBoolean)
      dut.io.probe #= 7
      dut.clockDomain.waitSampling()
      assert(!dut.io.eq.toBoolean)
      assert(dut.io.ne.toBoolean)
    }
  }

  test("OneHotCounter load(int) sets bit position") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val loadIt = in  Bool()
        val v      = out Bits(8 bits)
      }
      val c = OneHotCounter(8)
      when(io.loadIt) { c.load(5) }
      io.v := c.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.loadIt #= false
      waitOutOfReset(dut)
      pulse(dut.clockDomain, dut.io.loadIt)
      assert(dut.io.v.toInt == (1 << 5))
    }
  }

  // Pre-computed binary-reflected Gray sequences
  private def graySeq(width: Int): Seq[Int] =
    (0 until (1 << width)).map(i => i ^ (i >> 1))

  for (width <- Seq(2, 3, 4, 5)) {
    test(s"GrayCounter[Up] width=$width Wrap: traverses full Gray sequence") {
      SimConfig.compile(new Component {
        val io = new Bundle {
          val inc = in  Bool()
          val g   = out UInt(width bits)
        }
        val c = GrayCounter(width)
        when(io.inc) { c.increment() }
        io.g := c.value
      }).doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.io.inc #= false
        waitOutOfReset(dut)
        assert(dut.io.g.toInt == 0)
        dut.io.inc #= true
        dut.clockDomain.waitSampling()
        val seen = collect(dut.clockDomain, 1 << width) { dut.io.g.toInt }
        val expected = graySeq(width).tail :+ 0
        assert(seen == expected, s"got $seen expected $expected")
      }
    }
  }

  test("GrayCounter[Both] width=4: inc then dec traverses reverse") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc = in  Bool()
        val dec = in  Bool()
        val g   = out UInt(4 bits)
      }
      val c = GrayCounter.both(4)
      when(io.inc) { c.increment() }
      when(io.dec) { c.decrement() }
      io.g := c.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= false
      dut.io.dec #= false
      waitOutOfReset(dut)
      dut.io.inc #= true
      dut.clockDomain.waitSampling()
      val forward = collect(dut.clockDomain, 16) { dut.io.g.toInt }
      assert(forward == (graySeq(4).tail :+ 0))
      dut.io.inc #= false
      dut.io.dec #= true
      dut.clockDomain.waitSampling(2)
      val backward = collect(dut.clockDomain, 16) { dut.io.g.toInt }
      // decrementing from 0 jumps to top state then traverses reverse
      val expectedBack = Seq(8, 9, 11, 10, 14, 15, 13, 12, 4, 5, 7, 6, 2, 3, 1, 0)
      assert(backward == expectedBack, s"got $backward expected $expectedBack")
    }
  }

  test("GrayCounter[Up] Saturate pins at top state (MSB only)") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc = in  Bool()
        val g   = out UInt(4 bits)
        val sat = out Bool()
      }
      val c = new GrayCounter(4, upper = BoundaryPolicy.Saturate)
      when(io.inc) { c.increment() }
      io.g   := c.value
      io.sat := c.saturatedHigh
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= true
      waitOutOfReset(dut)
      dut.clockDomain.waitSampling(30)
      assert(dut.io.g.toInt == 8, s"top state 1<<3 = 8, got ${dut.io.g.toInt}")
      assert(dut.io.sat.toBoolean)
    }
  }

  test("GrayCounter loadOrdinal sets correct Gray value") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val loadIt = in  Bool()
        val ord    = in  UInt(4 bits)
        val g      = out UInt(4 bits)
      }
      val c = GrayCounter(4)
      when(io.loadIt) { c.loadOrdinal(io.ord) }
      io.g := c.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.loadIt #= false
      dut.io.ord #= 0
      waitOutOfReset(dut)
      for (ord <- Seq(0, 5, 7, 13, 15)) {
        dut.io.loadIt #= true
        dut.io.ord #= ord
        dut.clockDomain.waitSampling()
        dut.io.loadIt #= false
        dut.clockDomain.waitSampling()
        val expected = ord ^ (ord >> 1)
        assert(dut.io.g.toInt == expected, s"ord=$ord expected $expected got ${dut.io.g.toInt}")
      }
    }
  }

  test("GrayCounter legacy function form returns UInt gated by enable") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val en = in  Bool()
        val g  = out UInt(4 bits)
      }
      io.g := GrayCounter(4, io.en)
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.en #= false
      waitOutOfReset(dut)
      assert(dut.io.g.toInt == 0)
      dut.io.en #= true
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling()
      assert(dut.io.g.toInt == 1, "first gray step after 0 is 1")
    }
  }

  test("JohnsonCounter width=4 cycles through all 8 legal states") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc = in  Bool()
        val v   = out Bits(4 bits)
      }
      val c = JohnsonCounter(4)
      when(io.inc) { c.increment() }
      io.v := c.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= false
      waitOutOfReset(dut)
      assert(dut.io.v.toInt == 0)
      dut.io.inc #= true
      dut.clockDomain.waitSampling()
      // legal sequence fills from LSB: 0000 → 0001 → 0011 → 0111 → 1111 → 1110 → 1100 → 1000 → 0000
      val expected = Seq(0x1, 0x3, 0x7, 0xF, 0xE, 0xC, 0x8, 0x0, 0x1)
      val seen = expected.indices.map { _ =>
        dut.clockDomain.waitSampling()
        dut.io.v.toInt
      }
      assert(seen == expected, s"got ${seen.map("0x%X".format(_))} expected ${expected.map("0x%X".format(_))}")
    }
  }

  test("JohnsonCounter stateCount equals 2*width") {
    for (w <- Seq(2, 3, 4, 5, 8, 16)) {
      SimConfig.compile(new Component {
        val jc = JohnsonCounter(w)
        val io = new Bundle { val sc = out UInt(8 bits) }
        io.sc := U(jc.stateCount, 8 bits)
        // elaboration-time sanity: the declared stateCount method must equal 2*w
        require(jc.stateCount == 2 * w, s"width=$w: stateCount=${jc.stateCount}, expected ${2 * w}")
      }).doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.waitSampling()
        assert(dut.io.sc.toInt == 2 * w,
          s"width=$w: sim observed stateCount=${dut.io.sc.toInt}, expected ${2 * w}")
      }
    }
  }

  test("JohnsonCounter clkDiv signal is 50% duty") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val d = out Bool()
      }
      val c = JohnsonCounter(4).freeRun()
      io.d := c.clkDiv
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      waitOutOfReset(dut)
      // Over a full 2*4 = 8-cycle period, clkDiv should be high for 4 cycles and low for 4.
      val samples = (0 until 16).map { _ =>
        dut.clockDomain.waitSampling()
        dut.io.d.toBoolean
      }
      val highs = samples.count(identity)
      val lows  = samples.length - highs
      assert(highs == 8 && lows == 8, s"expected 8/8 got highs=$highs lows=$lows")
    }
  }

  test("JohnsonCounter Freeze: latches at top legal state") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc    = in  Bool()
        val clr    = in  Bool()
        val v      = out Bits(4 bits)
        val frozen = out Bool()
      }
      val c = new JohnsonCounter(4, upper = BoundaryPolicy.Freeze)
      when(io.inc) { c.increment() }
      when(io.clr) { c.clear() }
      io.v      := c.value
      io.frozen := c.frozen
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.clr #= false
      dut.io.inc #= true
      waitOutOfReset(dut)
      dut.clockDomain.waitSampling(20)
      assert(dut.io.frozen.toBoolean, "should freeze after completing a cycle")
      val heldValue = dut.io.v.toInt
      dut.clockDomain.waitSampling(10)
      assert(dut.io.v.toInt == heldValue, "should hold while frozen")
      dut.io.inc #= false
      dut.io.clr #= true
      dut.clockDomain.waitSampling()
      dut.io.clr #= false
      dut.clockDomain.waitSampling()
      assert(!dut.io.frozen.toBoolean)
      assert(dut.io.v.toInt == 0)
    }
  }

  test("Counter(n, someBool) gates increment on the Bool") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val en = in  Bool()
        val v  = out UInt(4 bits)
      }
      val c = Counter(16, io.en)
      io.v := c.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.en #= false
      waitOutOfReset(dut)
      assert(dut.io.v.toInt == 0)
      for (_ <- 0 until 3) dut.clockDomain.waitSampling()
      assert(dut.io.v.toInt == 0)
      dut.io.en #= true
      for (_ <- 0 until 5) dut.clockDomain.waitSampling()
      dut.io.en #= false
      dut.clockDomain.waitSampling()
      assert(dut.io.v.toInt == 5)
    }
  }

  test("Counter[Both] bothWrap + non-pow2 stateCount wraps at stateCount (default handleOverflow)") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc = in  Bool()
        val dec = in  Bool()
        val v   = out UInt(4 bits)
      }
      val c = new spinal.lib.Counter(0, 8, direction = CounterDirection.Both)
      when(io.inc) { c.increment() }
      when(io.dec) { c.decrement() }
      io.v := c.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= false; dut.io.dec #= false
      waitOutOfReset(dut)
      dut.io.inc #= true
      dut.clockDomain.waitSampling()
      // Walk up past end=8: should wrap to start=0, NOT to 9..15
      val up = collect(dut.clockDomain, 11) { dut.io.v.toInt }
      assert(up == Seq(1,2,3,4,5,6,7,8,0,1,2), s"wrap at stateCount: got $up")
    }
  }

  test("Counter[Both] Saturate both + non-pow2 stateCount saturates on inc and on dec") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc = in  Bool()
        val dec = in  Bool()
        val v   = out UInt(4 bits)
      }
      // span = 9 (non-pow2), start=0, end=8; Saturate upper AND lower
      val c = new spinal.lib.Counter(
        0, 8,
        direction = CounterDirection.Both,
        upper = BoundaryPolicy.Saturate,
        lower = BoundaryPolicy.Saturate
      )
      when(io.inc) { c.increment() }
      when(io.dec) { c.decrement() }
      io.v := c.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= false; dut.io.dec #= false
      waitOutOfReset(dut)
      assert(dut.io.v.toInt == 0)
      dut.io.inc #= true
      dut.clockDomain.waitSampling()
      val up = collect(dut.clockDomain, 11) { dut.io.v.toInt }
      assert(up == Seq(1,2,3,4,5,6,7,8,8,8,8), s"walk-up saturate: got $up")
      dut.io.inc #= false
      dut.io.dec #= true
      dut.clockDomain.waitSampling()
      val down = collect(dut.clockDomain, 11) { dut.io.v.toInt }
      assert(down == Seq(7,6,5,4,3,2,1,0,0,0,0), s"walk-down saturate: got $down")
    }
  }

  test("CounterUpDown non-pow2 with handleOverflow=false wraps at 2^width") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc = in  Bool()
        val v   = out UInt(4 bits)
      }
      // stateCount=10, width=4 → wraps at 16, not 10
      val c = CounterUpDown(10, io.inc, False, handleOverflow = false)
      io.v := c.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= false
      waitOutOfReset(dut)
      assert(dut.io.v.toInt == 0)
      dut.io.inc #= true
      dut.clockDomain.waitSampling()
      // Walk 18 cycles; with handleOverflow=false, we expect modular wrap at 16:
      // 1,2,...,15,0,1,2
      val expected = Seq(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0,1,2)
      val seen = collect(dut.clockDomain, expected.length) { dut.io.v.toInt }
      assert(seen == expected, s"handleOverflow=false non-pow2 wrap:\n  got     $seen\n  wanted  $expected")
    }
  }

  test("CounterUpDown non-pow2 with handleOverflow=true wraps at stateCount") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc = in  Bool()
        val v   = out UInt(4 bits)
      }
      val c = CounterUpDown(10, io.inc, False, handleOverflow = true)
      io.v := c.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= false
      waitOutOfReset(dut)
      dut.io.inc #= true
      dut.clockDomain.waitSampling()
      // handleOverflow=true (default): wrap at stateCount=10, NOT at 2^width=16
      val expected = Seq(1,2,3,4,5,6,7,8,9,0,1,2,3)
      val seen = collect(dut.clockDomain, expected.length) { dut.io.v.toInt }
      assert(seen == expected, s"handleOverflow=true non-pow2 wrap:\n  got     $seen\n  wanted  $expected")
    }
  }

  test("Counter[Both] willAdvance OR's inc and dec (not only inc)") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc = in  Bool()
        val dec = in  Bool()
        val adv = out Bool()
      }
      val c = new spinal.lib.Counter(0, 15, direction = CounterDirection.Both)
      when(io.inc) { c.increment() }
      when(io.dec) { c.decrement() }
      io.adv := c.willAdvance
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= false; dut.io.dec #= false
      waitOutOfReset(dut)
      assert(!dut.io.adv.toBoolean)
      // dec alone should still assert willAdvance
      dut.io.dec #= true
      dut.clockDomain.waitSampling()
      assert(dut.io.adv.toBoolean, "dec-only should assert willAdvance for Both-direction counter")
      dut.io.dec #= false
      // inc alone
      dut.io.inc #= true
      dut.clockDomain.waitSampling()
      assert(dut.io.adv.toBoolean)
    }
  }

  test("CounterUpDown legacy class: construct, inc, dec") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val inc = in  Bool()
        val dec = in  Bool()
        val v   = out UInt(4 bits)
      }
      val c = CounterUpDown(16, io.inc, io.dec)
      io.v := c.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.inc #= false
      dut.io.dec #= false
      waitOutOfReset(dut)
      dut.io.inc #= true
      for (_ <- 0 until 5) dut.clockDomain.waitSampling()
      dut.io.inc #= false
      dut.clockDomain.waitSampling()
      assert(dut.io.v.toInt == 5)
    }
  }

  test("Binary Counter loadOrdinal on [5,12] range") {
    SimConfig.compile(new Component {
      val io = new Bundle {
        val loadIt = in  Bool()
        val ord    = in  UInt(3 bits)
        val v      = out UInt(4 bits)
      }
      val c = new Counter(5, 12)
      when(io.loadIt) { c.loadOrdinal(io.ord) }
      io.v := c.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.loadIt #= false
      dut.io.ord #= 0
      waitOutOfReset(dut)
      for (ord <- 0 until 8) {
        dut.io.loadIt #= true
        dut.io.ord #= ord
        dut.clockDomain.waitSampling()
        dut.io.loadIt #= false
        dut.clockDomain.waitSampling()
        assert(dut.io.v.toInt == (5 + ord), s"ord=$ord expected ${5+ord} got ${dut.io.v.toInt}")
      }
    }
  }

  private def johnsonLegalSequence(width: Int): Seq[BigInt] = {
    val mask = (BigInt(1) << width) - 1
    val seq = scala.collection.mutable.ArrayBuffer[BigInt](BigInt(0))
    for (_ <- 0 until 2 * width - 1) {
      val prev = seq.last
      val msbInv = BigInt(1) - ((prev >> (width - 1)) & 1)
      seq += ((prev << 1) | msbInv) & mask
    }
    seq.toSeq
  }

  private def johnsonIllegalStates(width: Int): Seq[BigInt] = {
    val legal = johnsonLegalSequence(width).toSet
    (0 until (1 << width)).map(BigInt(_)).filterNot(legal.contains)
  }

  private class JohnsonDut(val width: Int) extends Component {
    val inc      = in  Bool()
    val clr      = in  Bool()
    val forceEn  = in  Bool()
    val forceVal = in  Bits(width bits)
    val value    = out Bits(width bits)
    val willOverflow = out Bool()
    val clkDiv   = out Bool()

    val jc = JohnsonCounter(width)
    when(inc)      { jc.increment() }
    when(clr)      { jc.clear() }
    when(forceEn)  { jc.value := forceVal }
    value        := jc.value
    willOverflow := jc.willOverflow
    clkDiv       := jc.clkDiv
  }

  private def withJohnsonDut(w: Int)(body: JohnsonDut => Unit): Unit = {
    SimConfig.compile(new JohnsonDut(w)).doSim { dut =>
      dut.inc #= false; dut.clr #= false; dut.forceEn #= false; dut.forceVal #= 0
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling(20)
      body(dut)
    }
  }

  test("JohnsonCounter post-reset value is 0") {
    for (w <- Seq(2, 3, 4, 8)) withJohnsonDut(w) { dut =>
      assert(dut.value.toBigInt == 0, s"width=$w")
    }
  }

  test("JohnsonCounter walks legal sequence under free-running inc") {
    for (w <- Seq(2, 3, 4, 5, 8)) withJohnsonDut(w) { dut =>
      val legal = johnsonLegalSequence(w)
      dut.inc #= true
      dut.clockDomain.waitSampling()
      for (i <- 1 to 3 * legal.size) {
        dut.clockDomain.waitSampling()
        val expected = legal(i % legal.size)
        assert(dut.value.toBigInt == expected,
          s"width=$w cycle $i: got ${dut.value.toBigInt.toString(2)} expected ${expected.toString(2)}")
      }
    }
  }

  test("JohnsonCounter willOverflow pulses exactly once per 2*width cycles") {
    for (w <- Seq(2, 4, 5, 8)) withJohnsonDut(w) { dut =>
      dut.inc #= true
      dut.clockDomain.waitSampling()
      while (dut.value.toBigInt != 0) dut.clockDomain.waitSampling()
      val periods = 7
      val cycles  = 2 * w * periods
      var pulses  = 0
      for (_ <- 0 until cycles) {
        dut.clockDomain.waitSampling()
        if (dut.willOverflow.toBoolean) pulses += 1
      }
      assert(pulses == periods, s"width=$w: $pulses pulses in $cycles cycles")
    }
  }

  test("JohnsonCounter clkDiv is 50% duty divide-by-(2*width)") {
    for (w <- Seq(2, 3, 4, 5, 6, 8)) withJohnsonDut(w) { dut =>
      dut.inc #= true
      dut.clockDomain.waitSampling()
      while (dut.value.toBigInt != 0) dut.clockDomain.waitSampling()
      val cycles = 2 * w * 20
      var highs  = 0
      for (_ <- 0 until cycles) {
        dut.clockDomain.waitSampling()
        if (dut.clkDiv.toBoolean) highs += 1
      }
      assert(highs * 2 == cycles, s"width=$w: $highs highs / $cycles cycles")
    }
  }

  test("JohnsonCounter clear returns to 0 from a non-zero legal state") {
    withJohnsonDut(4) { dut =>
      val legal = johnsonLegalSequence(4)
      dut.inc #= true
      dut.clockDomain.waitSampling(4)
      assert(dut.value.toBigInt == legal(3))
      dut.inc #= false
      dut.clr #= true
      dut.clockDomain.waitSampling(2)
      assert(dut.value.toBigInt == 0)
    }
  }

  test("JohnsonCounter clear beats increment when both asserted") {
    withJohnsonDut(4) { dut =>
      dut.inc #= true
      dut.clockDomain.waitSampling(4)
      assert(dut.value.toBigInt != 0)
      dut.clr #= true    // inc still true
      dut.clockDomain.waitSampling(2)
      assert(dut.value.toBigInt == 0)
    }
  }

  test("JohnsonCounter self-recovers from every illegal state within 2*width increments") {
    for (w <- Seq(3, 4, 5, 6)) withJohnsonDut(w) { dut =>
      val legal = johnsonLegalSequence(w).toSet
      val limit = 2 * w
      for (state <- johnsonIllegalStates(w)) {
        dut.inc #= false
        dut.forceEn  #= true
        dut.forceVal #= state
        dut.clockDomain.waitSampling(2)
        assert(dut.value.toBigInt == state, s"width=$w inject ${state.toString(2)} failed")

        dut.forceEn #= false
        dut.inc     #= true
        dut.clockDomain.waitSampling()
        var steps = 0
        var recovered = false
        while (!recovered && steps < limit) {
          dut.clockDomain.waitSampling()
          steps += 1
          if (legal.contains(dut.value.toBigInt)) recovered = true
        }
        assert(recovered, s"width=$w illegal ${state.toString(2)} stuck at ${dut.value.toBigInt.toString(2)}")
      }
    }
  }

  test("JohnsonCounter illegal-state recovery eventually reaches 0") {
    for (w <- Seq(3, 4, 5, 6)) withJohnsonDut(w) { dut =>
      for (state <- johnsonIllegalStates(w)) {
        dut.inc #= false
        dut.forceEn  #= true
        dut.forceVal #= state
        dut.clockDomain.waitSampling(2)

        dut.forceEn #= false
        dut.inc     #= true
        dut.clockDomain.waitSampling()
        var steps = 0; var hitZero = false
        val limit = 4 * w
        while (!hitZero && steps < limit) {
          dut.clockDomain.waitSampling()
          steps += 1
          if (dut.value.toBigInt == 0) hitZero = true
        }
        assert(hitZero, s"width=$w ${state.toString(2)} never reached 0 within $limit")
      }
    }
  }

  test("JohnsonCounter freeRun() walks the full legal sequence without external enable") {
    SimConfig.compile(new Component {
      val jc = JohnsonCounter(4).freeRun()
      val value = out Bits(4 bits)
      value := jc.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling(20)
      while (dut.value.toBigInt != 0) dut.clockDomain.waitSampling()
      val legal = johnsonLegalSequence(4)
      for (i <- 1 to 2 * legal.size) {
        dut.clockDomain.waitSampling()
        val expected = legal(i % legal.size)
        assert(dut.value.toBigInt == expected, s"freeRun cycle $i: got ${dut.value.toBigInt} expected $expected")
      }
    }
  }

  private class GrayCntRef(size: Int) extends Component {
    val io = new Bundle { val gval = out UInt(size bits) }
    val toggle = Reg(Bool()) init(False)
    val cnt    = Reg(UInt(size bits)) init(0)
    toggle := !toggle
    cnt.lsb := !(cnt.lsb ^ toggle)
    for (i <- 1 to size - 2) {
      val tmp = cnt(i - 1) && cnt(i - 2 downto 0).asBools.fold(True)((acc, b) => acc && !b) && toggle
      cnt(i) := cnt(i) ^ tmp
    }
    cnt.msb := cnt.msb ^ (cnt(size - 3 downto 0) === 0 && toggle)
    io.gval := cnt
  }

  test("GrayCounter(n, enable) legacy form matches hand-written reference") {
    for (w <- Seq(4, 6, 8)) {
      SimConfig.compile(new Component {
        val io = new Bundle {
          val enable = in  Bool()
          val gray   = out UInt(w bits)
          val ref    = out UInt(w bits)
        }
        io.gray := GrayCounter(w, io.enable)
        val refDom = ClockDomain(
          ClockDomain.current.clock, ClockDomain.current.reset, clockEnable = io.enable
        )(new GrayCntRef(w))
        io.ref := refDom.io.gval
      }).doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.io.enable #= false
        dut.clockDomain.waitSampling(10)
        dut.io.enable #= true
        for (_ <- 0 until (1 << w) * 3) {
          dut.clockDomain.waitSampling()
          assert(dut.io.gray.toBigInt == dut.io.ref.toBigInt,
            s"width=$w: gray=${dut.io.gray.toBigInt.toString(2)} ref=${dut.io.ref.toBigInt.toString(2)}")
        }
      }
    }
  }

  test("All counter types expose stateCount and will{Advance,Complete}") {
    SimConfig.compile(new Component {
      val binU  = Counter(16)
      val binD  = Counter.down(16)
      val binB  = Counter.both(16)
      val oh    = OneHotCounter(8)
      val gry   = GrayCounter(4)
      val john  = JohnsonCounter(4)

      // just pin their prescaler-surface signals to outputs to make sure they exist + elaborate
      val io = new Bundle {
        val binUAdv = out Bool()
        val binDAdv = out Bool()
        val binBAdv = out Bool()
        val ohAdv   = out Bool()
        val gryAdv  = out Bool()
        val johnAdv = out Bool()
      }
      io.binUAdv := binU.willAdvance
      io.binDAdv := binD.willAdvance
      io.binBAdv := binB.willAdvance
      io.ohAdv   := oh.willAdvance
      io.gryAdv  := gry.willAdvance
      io.johnAdv := john.willAdvance

      // sanity on stateCount values
      assert(binU.stateCount == 16)
      assert(binD.stateCount == 16)
      assert(binB.stateCount == 16)
      assert(oh.stateCount   == 8)
      assert(gry.stateCount  == 16)
      assert(john.stateCount == 8)
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      waitOutOfReset(dut)
      // just ensure it simulates; actual advance behavior is tested elsewhere
    }
  }
}
