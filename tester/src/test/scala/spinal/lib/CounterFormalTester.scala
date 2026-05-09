package spinal.lib

import spinal.core._
import spinal.core.formal._
import spinal.lib.formal._

/** Formal-verification tests for the counter package.
  *
  * Proves encoding invariants, boundary-policy semantics, and sticky-latch
  * behavior inductively. Widths are small so induction closes in seconds.
  */
class CounterFormalTester extends SpinalFormalFunSuite {

  private def reset: Bool = ClockDomain.current.isResetActive

  private def prove(depth: Int = 5)(body: => Unit): Unit =
    FormalConfig.withSyncResetDefault.withProve(depth).doVerify(
      new Component { assumeInitial(reset); body }
    )

  private def coverFormal(depth: Int = 10)(body: => Unit): Unit =
    FormalConfig.withSyncResetDefault.withCover(depth).doVerify(
      new Component { assumeInitial(reset); body }
    )

  // Binary counter fixtures: build counter + free symbolic inputs + wire them.
  private def upCounter(start: Int = 0, end: Int = 7,
                        upper: BoundaryPolicy = BoundaryPolicy.Wrap
                       ): (Counter, Bool, Bool) = {
    val c   = new Counter(start, end, upper = upper)
    val inc = anyseq(Bool()); val clr = anyseq(Bool())
    when(inc) { c.increment() }
    when(clr) { c.clear() }
    (c, inc, clr)
  }

  private def downCounter(start: Int = 0, end: Int = 7,
                          lower: BoundaryPolicy = BoundaryPolicy.Wrap
                         ): (Counter, Bool, Bool) = {
    val c   = new Counter(start, end, lower = lower, direction = CounterDirection.Down)
    val dec = anyseq(Bool()); val clr = anyseq(Bool())
    when(dec) { c.decrement() }
    when(clr) { c.clear() }
    (c, dec, clr)
  }

  private def bothCounter(end: Int = 7,
                          upper: BoundaryPolicy = BoundaryPolicy.Wrap,
                          lower: BoundaryPolicy = BoundaryPolicy.Wrap
                         ): (Counter, Bool, Bool, Bool) = {
    val c   = new Counter(0, end, upper = upper, lower = lower, direction = CounterDirection.Both)
    val inc = anyseq(Bool()); val dec = anyseq(Bool()); val clr = anyseq(Bool())
    when(inc) { c.increment() }
    when(dec) { c.decrement() }
    when(clr) { c.clear() }
    (c, inc, dec, clr)
  }

  test("Counter[Up] value always <= end") {
    prove() { val (c, _, _) = upCounter(); assert(c.value <= U(7)) }
  }

  test("Counter[Down] value always in [0, 7]") {
    prove() { val (c, _, _) = downCounter(); assert(c.value <= U(7)) }
  }

  test("Counter[Both] value stays in [0, end]") {
    prove() { val (c, _, _, _) = bothCounter(); assert(c.value <= U(7)) }
  }

  test("Counter[Up] Saturate: past at end + inc implies still at end") {
    prove() {
      val (c, inc, clr) = upCounter(upper = BoundaryPolicy.Saturate)
      when(!past(reset) && past(c.value === U(7)) && past(inc) && !past(clr)) {
        assert(c.value === U(7))
      }
    }
  }

  test("Counter[Up] Saturate: saturatedHigh iff value at end") {
    prove() {
      val (c, _, _) = upCounter(upper = BoundaryPolicy.Saturate)
      assert(c.saturatedHigh === (c.value === U(7)))
    }
  }

  test("Counter[Up] Freeze: frozen is sticky while no clear") {
    prove() {
      val (c, _, clr) = upCounter(upper = BoundaryPolicy.Freeze)
      when(!past(reset) && past(c.frozen) && !past(clr)) { assert(c.frozen) }
    }
  }

  test("Counter[Up] Freeze: frozen holds value stable") {
    prove() {
      val (c, _, clr) = upCounter(upper = BoundaryPolicy.Freeze)
      when(!past(reset) && past(c.frozen) && !past(clr)) { assert(stable(c.value)) }
    }
  }

  test("Counter[Up] Clear always returns to start in one cycle") {
    prove() {
      val (c, _, clr) = upCounter()
      when(past(clr) && !reset) { assert(c.value === U(0)) }
    }
  }

  test("OneHotCounter[Up] always has exactly one bit set") {
    prove() {
      val c   = new OneHotCounter(8)
      val inc = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(clr) { c.clear() }
      assert(CountOne(c.value) === U(1))
    }
  }

  test("OneHotCounter[Both] always has exactly one bit set") {
    prove() {
      val c   = new OneHotCounter(8, direction = CounterDirection.Both)
      val inc = anyseq(Bool()); val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(dec) { c.decrement() }
      when(clr) { c.clear() }
      assert(CountOne(c.value) === U(1))
    }
  }

  test("GrayCounter[Up] consecutive states differ by at most 1 bit") {
    prove() {
      val c   = new GrayCounter(4)
      val inc = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(clr) { c.clear() }
      when(!past(clr) && !reset && !past(reset)) {
        assert(CountOne(c.value.asBits ^ past(c.value).asBits) <= U(1))
      }
    }
  }

  test("GrayCounter[Both] consecutive states differ by at most 1 bit") {
    prove() {
      val c   = new GrayCounter(4, direction = CounterDirection.Both)
      val inc = anyseq(Bool()); val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(dec) { c.decrement() }
      when(clr) { c.clear() }
      when(!past(clr) && !reset && !past(reset)) {
        assert(CountOne(c.value.asBits ^ past(c.value).asBits) <= U(1))
      }
    }
  }

  test("JohnsonCounter width=4 stays in legal state set from reset") {
    prove() {
      val c   = new JohnsonCounter(4)
      val inc = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(clr) { c.clear() }
      val legal = Seq(0x0, 0x1, 0x3, 0x7, 0xF, 0xE, 0xC, 0x8).map(B(_, 4 bits))
      assert(legal.map(c.value === _).reduce(_ || _))
    }
  }

  test("Counter[Up] Wrap: every state 0..7 is reachable") {
    coverFormal() {
      val (c, _, _) = upCounter()
      for (v <- 0 to 7) cover(c.value === U(v))
    }
  }

  test("OneHotCounter[Both] every bit-position is reachable") {
    coverFormal() {
      val c   = new OneHotCounter(8, direction = CounterDirection.Both)
      val inc = anyseq(Bool()); val dec = anyseq(Bool())
      when(inc) { c.increment() }
      when(dec) { c.decrement() }
      for (i <- 0 until 8) cover(c.value === B(1 << i, 8 bits))
    }
  }

  test("JohnsonCounter width=4: every legal state is reachable") {
    coverFormal(depth = 20) {
      val c   = new JohnsonCounter(4)
      val inc = anyseq(Bool())
      when(inc) { c.increment() }
      val legal = Seq(0x0, 0x1, 0x3, 0x7, 0xF, 0xE, 0xC, 0x8)
      for (v <- legal) cover(c.value === B(v, 4 bits))
    }
  }

  test("Counter[Down] Saturate: past at 0 + dec implies still at 0") {
    prove() {
      val (c, dec, clr) = downCounter(lower = BoundaryPolicy.Saturate)
      when(!past(reset) && past(c.value === U(0)) && past(dec) && !past(clr)) {
        assert(c.value === U(0))
      }
    }
  }

  test("Counter[Down] Saturate: saturatedLow iff value at start") {
    prove() {
      val (c, _, _) = downCounter(lower = BoundaryPolicy.Saturate)
      assert(c.saturatedLow === (c.value === U(0)))
    }
  }

  test("Counter[Down] Freeze: frozen is sticky while no clear") {
    prove() {
      val (c, _, clr) = downCounter(lower = BoundaryPolicy.Freeze)
      when(!past(reset) && past(c.frozen) && !past(clr)) {
        assert(c.frozen && stable(c.value))
      }
    }
  }

  test("Counter[Down] Clear returns to end (not start) in one cycle") {
    prove() {
      val (c, _, clr) = downCounter()
      when(past(clr) && !reset && !past(reset)) { assert(c.value === U(7)) }
    }
  }

  test("Counter[Up] Wrap: past(willOverflow) implies value === start") {
    prove() {
      val (c, _, clr) = upCounter()
      when(!past(reset) && past(c.willOverflow) && !past(clr)) { assert(c.value === U(0)) }
    }
  }

  test("Counter[Down] Wrap: past(willUnderflow) implies value === end") {
    prove() {
      val (c, _, clr) = downCounter()
      when(!past(reset) && past(c.willUnderflow) && !past(clr)) { assert(c.value === U(7)) }
    }
  }

  test("Counter[Both] Saturate upper + Wrap lower: pinned at end under inc-only") {
    prove() {
      val (c, inc, dec, clr) = bothCounter(upper = BoundaryPolicy.Saturate)
      when(!past(reset) && past(c.value === U(7)) && past(inc) && !past(dec) && !past(clr)) {
        assert(c.value === U(7))
      }
    }
  }

  test("Counter[Both] Freeze upper + Wrap lower: inc cannot escape frozen") {
    prove() {
      val (c, inc, dec, clr) = bothCounter(upper = BoundaryPolicy.Freeze)
      when(!past(reset) && past(c.frozen) && past(inc) && !past(dec) && !past(clr)) {
        assert(stable(c.value))
      }
    }
  }

  test("Counter[Both] Freeze both: value holds under any input except clr") {
    prove() {
      val (c, _, _, clr) = bothCounter(
        upper = BoundaryPolicy.Freeze, lower = BoundaryPolicy.Freeze)
      when(!past(reset) && past(c.frozen) && !past(clr)) {
        assert(stable(c.value))
      }
    }
  }

  test("Counter[Up] Load: past(load) implies value === past(loadVal)") {
    prove() {
      val c       = new Counter(0, 7)
      val inc     = anyseq(Bool())
      val loadIt  = anyseq(Bool())
      val loadVal = anyseq(UInt(3 bits))
      when(inc)    { c.increment() }
      when(loadIt) { c.load(loadVal) }
      when(!past(reset) && past(loadIt)) { assert(c.value === past(loadVal)) }
    }
  }

  test("Counter[Up] Freeze: load escapes the latch") {
    prove() {
      val c       = new Counter(0, 7, upper = BoundaryPolicy.Freeze)
      val inc     = anyseq(Bool())
      val loadIt  = anyseq(Bool())
      val loadVal = anyseq(UInt(3 bits))
      when(inc)    { c.increment() }
      when(loadIt) { c.load(loadVal) }
      when(!past(reset) && past(loadIt)) {
        assert(!c.frozen)
        assert(c.value === past(loadVal))
      }
    }
  }

  test("OneHotCounter[Up] inc rotates value left by exactly one bit") {
    prove() {
      val c   = new OneHotCounter(8)
      val inc = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(clr) { c.clear() }
      when(!past(reset) && past(inc) && !past(clr)) {
        assert(c.value === past(c.value).rotateLeft(1))
      }
    }
  }

  test("OneHotCounter[Both] dec rotates right by exactly one bit") {
    prove() {
      val c   = new OneHotCounter(8, direction = CounterDirection.Both)
      val inc = anyseq(Bool()); val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(dec) { c.decrement() }
      when(clr) { c.clear() }
      when(!past(reset) && !past(inc) && past(dec) && !past(clr)) {
        assert(c.value === past(c.value).rotateRight(1))
      }
    }
  }

  test("GrayCounter[Up] even === !XOR-reduction(gray) always") {
    prove() {
      val c   = new GrayCounter(4)
      val inc = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(clr) { c.clear() }
      assert(c.even === !c.value.asBits.xorR)
    }
  }

  test("GrayCounter[Both] even === !XOR-reduction(gray) always") {
    prove() {
      val c   = new GrayCounter(4, direction = CounterDirection.Both)
      val inc = anyseq(Bool()); val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(dec) { c.decrement() }
      when(clr) { c.clear() }
      assert(c.even === !c.value.asBits.xorR)
    }
  }

  test("Counter[Up] non-zero start: value always in [5, 12]") {
    prove() {
      val (c, _, _) = upCounter(start = 5, end = 12)
      assert(c.value >= U(5, 4 bits))
      assert(c.value <= U(12, 4 bits))
    }
  }

  test("Counter[Up] non-zero start Wrap: past(willOverflow) implies value === 5") {
    prove() {
      val (c, _, clr) = upCounter(start = 5, end = 12)
      when(!past(reset) && past(c.willOverflow) && !past(clr)) {
        assert(c.value === U(5, 4 bits))
      }
    }
  }

  test("Counter[Up] init: after reset releases, value === start") {
    prove() {
      val (c, _, _) = upCounter(start = 3, end = 12)
      when(past(reset) && !reset) { assert(c.value === U(3, 4 bits)) }
    }
  }

  test("Counter[Down] init: after reset releases, value === end") {
    prove() {
      val (c, _, _) = downCounter()
      when(past(reset) && !reset) { assert(c.value === U(7)) }
    }
  }

  test("OneHotCounter init: after reset releases, value === (1 << initialValue)") {
    prove() {
      val c   = new OneHotCounter(8, initialValue = 3)
      val inc = anyseq(Bool())
      when(inc) { c.increment() }
      when(past(reset) && !reset) { assert(c.value === B(1 << 3, 8 bits)) }
    }
  }

  test("GrayCounter init: gray === 0 and even === True after reset") {
    prove() {
      val c   = new GrayCounter(4)
      val inc = anyseq(Bool())
      when(inc) { c.increment() }
      when(past(reset) && !reset) {
        assert(c.value === U(0, 4 bits))
        assert(c.even)
      }
    }
  }

  test("Counter[Up] willOverflow iff (value === end && willIncrement)") {
    prove() {
      val (c, _, _) = upCounter()
      assert(c.willOverflow === ((c.value === U(7)) && c.willIncrement))
    }
  }

  test("Counter[Down] willUnderflow iff (value === start && willDecrement)") {
    prove() {
      val (c, _, _) = downCounter()
      assert(c.willUnderflow === ((c.value === U(0)) && c.willDecrement))
    }
  }

  test("Counter[Both] overflow and underflow mutually exclusive") {
    prove() {
      val (c, _, _, _) = bothCounter()
      assert(!(c.willOverflow && c.willUnderflow))
    }
  }

  test("Counter[Both] Wrap upper: end+inc-only wraps to 0") {
    prove() {
      val (c, inc, dec, clr) = bothCounter()
      when(!past(reset) && past(c.value === U(7)) && past(inc) && !past(dec) && !past(clr)) {
        assert(c.value === U(0))
      }
    }
  }

  test("Counter[Both] Wrap lower: 0+dec-only wraps to end") {
    prove() {
      val (c, inc, dec, clr) = bothCounter()
      when(!past(reset) && past(c.value === U(0)) && past(dec) && !past(inc) && !past(clr)) {
        assert(c.value === U(7))
      }
    }
  }

  test("Counter[Both] simultaneous inc+dec holds value") {
    prove() {
      val (c, inc, dec, clr) = bothCounter()
      when(!past(reset) && past(inc) && past(dec) && !past(clr)) {
        assert(stable(c.value))
      }
    }
  }

  test("Counter[Both] Saturate both + non-pow2 span: value always in [0, 8]") {
    prove() {
      val c = new Counter(0, 8,
        direction = CounterDirection.Both,
        upper = BoundaryPolicy.Saturate,
        lower = BoundaryPolicy.Saturate
      )
      val inc = anyseq(Bool()); val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(dec) { c.decrement() }
      when(clr) { c.clear() }
      assert(c.value <= U(8))
    }
  }

  test("Counter[Both] Saturate upper + non-pow2 span: at-end + inc only holds at end") {
    prove() {
      val c = new Counter(0, 8,
        direction = CounterDirection.Both,
        upper = BoundaryPolicy.Saturate,
        lower = BoundaryPolicy.Wrap
      )
      val inc = anyseq(Bool()); val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(dec) { c.decrement() }
      when(clr) { c.clear() }
      when(!past(reset) && past(c.value === U(8)) && past(inc) && !past(dec) && !past(clr)) {
        assert(c.value === U(8))
      }
    }
  }

  test("Counter[Both] Saturate lower + non-pow2 span: at-start + dec only holds at start") {
    prove() {
      val c = new Counter(0, 8,
        direction = CounterDirection.Both,
        upper = BoundaryPolicy.Wrap,
        lower = BoundaryPolicy.Saturate
      )
      val inc = anyseq(Bool()); val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(dec) { c.decrement() }
      when(clr) { c.clear() }
      when(!past(reset) && past(c.value === U(0)) && past(dec) && !past(inc) && !past(clr)) {
        assert(c.value === U(0))
      }
    }
  }

  test("Counter[Up] Clear wins over inc") {
    prove() {
      val (c, _, clr) = upCounter()
      when(past(clr) && !reset && !past(reset)) { assert(c.value === U(0)) }
    }
  }

  test("Counter[Both] Clear wins over inc and dec") {
    prove() {
      val (c, _, _, clr) = bothCounter()
      when(past(clr) && !reset && !past(reset)) { assert(c.value === U(0)) }
    }
  }

  test("GrayCounter[Up] loadOrdinal: past(load k) implies gray === k ^ (k>>1)") {
    prove() {
      val c       = new GrayCounter(4)
      val loadIt  = anyseq(Bool())
      val loadOrd = anyseq(UInt(4 bits))
      when(loadIt) { c.loadOrdinal(loadOrd) }
      when(!past(reset) && past(loadIt)) {
        val k = past(loadOrd)
        assert(c.value === (k ^ (k >> U(1)).resized))
      }
    }
  }

  test("Counter[Up] Freeze: frozen implies value at end") {
    prove() {
      val (c, _, _) = upCounter(upper = BoundaryPolicy.Freeze)
      when(c.frozen) { assert(c.value === U(7)) }
    }
  }

  test("Counter[Up] Freeze: frozen rises only after overflow") {
    prove() {
      val (c, _, _) = upCounter(upper = BoundaryPolicy.Freeze)
      when(!past(reset) && !past(c.frozen) && c.frozen) {
        assert(past(c.willOverflow))
      }
    }
  }

  test("Counter[Up] willAdvance === willIncrement") {
    prove() {
      val (c, _, _) = upCounter()
      assert(c.willAdvance  === c.willIncrement)
      assert(c.willComplete === c.willOverflow)
    }
  }

  test("Counter[Down] willAdvance === willDecrement") {
    prove() {
      val (c, _, _) = downCounter()
      assert(c.willAdvance  === c.willDecrement)
      assert(c.willComplete === c.willUnderflow)
    }
  }

  test("Counter[Up] willOverflow implies willIncrement") {
    prove() {
      val (c, _, _) = upCounter()
      when(c.willOverflow) { assert(c.willIncrement) }
    }
  }

  test("Counter[Both] no overflow without inc, no underflow without dec") {
    prove() {
      val (c, _, _, _) = bothCounter()
      when(c.willOverflow)  { assert(c.willIncrement) }
      when(c.willUnderflow) { assert(c.willDecrement) }
    }
  }

  test("JohnsonCounter[width=4] recovery: illegal → legal within 2*width cycles") {
    FormalConfig.withSyncResetDefault.withBMC(12).doVerify(new Component {
      assumeInitial(reset)
      val c = JohnsonCounter(4)
      val injectVal = anyconst(Bits(4 bits))
      val injected  = Reg(Bool()) init(False)
      when(!injected) {
        c.value  := injectVal
        injected := True
      }
      c.increment()   // free-run

      val legal = Seq(0x0, 0x1, 0x3, 0x7, 0xF, 0xE, 0xC, 0x8).map(B(_, 4 bits))
      assume(!legal.map(injectVal === _).reduce(_ || _))   // only illegal starts

      val cyclesSinceInject = Reg(UInt(4 bits)) init(0)
      when(injected) { cyclesSinceInject := cyclesSinceInject + 1 }
      when(cyclesSinceInject === U(8)) {
        assert(legal.map(c.value === _).reduce(_ || _))
      }
    })
  }

  test("JohnsonCounter[width=4] recovery: eventually reaches 0 within 4*width cycles") {
    FormalConfig.withSyncResetDefault.withBMC(20).doVerify(new Component {
      assumeInitial(reset)
      val c = JohnsonCounter(4)
      val injectVal = anyconst(Bits(4 bits))
      val injected  = Reg(Bool()) init(False)
      when(!injected) {
        c.value  := injectVal
        injected := True
      }
      c.increment()

      val legal = Seq(0x0, 0x1, 0x3, 0x7, 0xF, 0xE, 0xC, 0x8).map(B(_, 4 bits))
      assume(!legal.map(injectVal === _).reduce(_ || _))

      val reachedZero = Reg(Bool()) init(False)
      when(injected && c.value === B(0, 4 bits)) { reachedZero := True }

      val cyclesSinceInject = Reg(UInt(5 bits)) init(0)
      when(injected) { cyclesSinceInject := cyclesSinceInject + 1 }
      when(cyclesSinceInject === U(16)) { assert(reachedZero) }
    })
  }

  test("GrayCounter[Up] gray === (ord ^ ord>>1) where ord is cycles since clear") {
    FormalConfig.withSyncResetDefault.withBMC(20).doVerify(new Component {
      assumeInitial(reset)
      val c   = new GrayCounter(4)
      val inc = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(clr) { c.clear() }

      // Shadow ordinal: counts increments modulo 2^width, reset by clear.
      val ord = Reg(UInt(4 bits)) init(0)
      when(clr) { ord := 0 }.elsewhen(inc) { ord := ord + 1 }

      assert(c.value === toGray(ord).asUInt)
    })
  }

  test("GrayCounter[Up] width=4: every state 0..15 is reachable") {
    coverFormal(depth = 20) {
      val c   = new GrayCounter(4)
      val inc = anyseq(Bool())
      when(inc) { c.increment() }
      for (v <- 0 until 16) cover(c.value === U(v, 4 bits))
    }
  }

  test("Counter[Both] willAdvance === willIncrement || willDecrement") {
    prove() {
      val (c, inc, dec, clr) = bothCounter()
      assert(c.willAdvance === (c.willIncrement || c.willDecrement))
    }
  }

  test("Counter[Both] willComplete === willOverflow || willUnderflow") {
    prove() {
      val (c, inc, dec, clr) = bothCounter()
      assert(c.willComplete === (c.willOverflow || c.willUnderflow))
    }
  }

  test("Counter[Both] inc+dec cancel: !willOverflow and !willUnderflow") {
    prove() {
      val (c, _, _, _) = bothCounter()
      when(c.willIncrement && c.willDecrement) {
        assert(!c.willOverflow)
        assert(!c.willUnderflow)
      }
    }
  }

  test("GrayCounter[Both] past inc + current dec → gray === 2-cycles-ago gray") {
    prove() {
      val c   = new GrayCounter(4, direction = CounterDirection.Both)
      val inc = anyseq(Bool()); val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(dec) { c.decrement() }
      when(clr) { c.clear() }

      // If two cycles ago we incremented (no dec/clr) and last cycle we
      // decremented (no inc/clr), the gray value must equal its own past-by-2.
      when(!reset && !past(reset) && !past(reset, 2)
           && past(inc, 2) && !past(dec, 2) && !past(clr, 2)
           && past(dec, 1) && !past(inc, 1) && !past(clr, 1)) {
        assert(c.value === past(c.value, 2))
      }
    }
  }

  test("OneHotCounter[Up] Saturate: saturatedHigh iff value.msb") {
    prove() {
      val c   = new OneHotCounter(8, upper = BoundaryPolicy.Saturate)
      val inc = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(clr) { c.clear() }
      assert(c.saturatedHigh === c.value.msb)
    }
  }

  test("OneHotCounter[Up] Saturate: past at msb + inc implies still at msb") {
    prove() {
      val c   = new OneHotCounter(8, upper = BoundaryPolicy.Saturate)
      val inc = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(clr) { c.clear() }
      when(!past(reset) && past(c.value.msb) && past(inc) && !past(clr)) {
        assert(c.value.msb)
      }
    }
  }

  test("OneHotCounter[Up] Freeze: frozen is sticky and holds value stable") {
    prove() {
      val c   = new OneHotCounter(8, upper = BoundaryPolicy.Freeze)
      val inc = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(clr) { c.clear() }
      when(!past(reset) && past(c.frozen) && !past(clr)) { assert(c.frozen && stable(c.value)) }
    }
  }

  test("OneHotCounter[Down] always has exactly one bit set") {
    prove() {
      val c   = new OneHotCounter(8, direction = CounterDirection.Down)
      val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(dec) { c.decrement() }
      when(clr) { c.clear() }
      assert(CountOne(c.value) === U(1))
    }
  }

  test("OneHotCounter[Down] Saturate: saturatedLow iff value.lsb") {
    prove() {
      val c   = new OneHotCounter(8, direction = CounterDirection.Down,
                                  lower = BoundaryPolicy.Saturate)
      val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(dec) { c.decrement() }
      when(clr) { c.clear() }
      assert(c.saturatedLow === c.value.lsb)
    }
  }

  test("OneHotCounter[Down] Freeze: frozen is sticky and holds value stable") {
    prove() {
      val c   = new OneHotCounter(8, direction = CounterDirection.Down,
                                  lower = BoundaryPolicy.Freeze)
      val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(dec) { c.decrement() }
      when(clr) { c.clear() }
      when(!past(reset) && past(c.frozen) && !past(clr)) { assert(c.frozen && stable(c.value)) }
    }
  }

  test("OneHotCounter[Both] Saturate both: at msb + inc-only stays at msb") {
    prove() {
      val c   = new OneHotCounter(8, direction = CounterDirection.Both,
                                  upper = BoundaryPolicy.Saturate,
                                  lower = BoundaryPolicy.Saturate)
      val inc = anyseq(Bool()); val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(dec) { c.decrement() }
      when(clr) { c.clear() }
      when(!past(reset) && past(c.value.msb) && past(inc) && !past(dec) && !past(clr)) {
        assert(c.value.msb)
      }
    }
  }

  test("OneHotCounter[Both] Freeze both: value holds under any input except clr") {
    prove() {
      val c   = new OneHotCounter(8, direction = CounterDirection.Both,
                                  upper = BoundaryPolicy.Freeze,
                                  lower = BoundaryPolicy.Freeze)
      val inc = anyseq(Bool()); val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(dec) { c.decrement() }
      when(clr) { c.clear() }
      when(!past(reset) && past(c.frozen) && !past(clr)) { assert(stable(c.value)) }
    }
  }

  test("GrayCounter[Up] Saturate: saturatedHigh iff gray === topState") {
    prove() {
      val c   = new GrayCounter(4, upper = BoundaryPolicy.Saturate)
      val inc = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(clr) { c.clear() }
      assert(c.saturatedHigh === (c.value === c.topState))
    }
  }

  test("GrayCounter[Up] Saturate: past at topState + inc stays at topState") {
    prove() {
      val c   = new GrayCounter(4, upper = BoundaryPolicy.Saturate)
      val inc = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(clr) { c.clear() }
      when(!past(reset) && past(c.value === c.topState) && past(inc) && !past(clr)) {
        assert(c.value === c.topState)
      }
    }
  }

  test("GrayCounter[Up] Freeze: frozen is sticky and holds gray stable") {
    prove() {
      val c   = new GrayCounter(4, upper = BoundaryPolicy.Freeze)
      val inc = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(clr) { c.clear() }
      when(!past(reset) && past(c.frozen) && !past(clr)) { assert(c.frozen && stable(c.value)) }
    }
  }

  test("GrayCounter[Down] consecutive states differ by at most 1 bit") {
    prove() {
      val c   = new GrayCounter(4, direction = CounterDirection.Down)
      val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(dec) { c.decrement() }
      when(clr) { c.clear() }
      when(!past(clr) && !reset && !past(reset)) {
        assert(CountOne(c.value.asBits ^ past(c.value).asBits) <= U(1))
      }
    }
  }

  test("GrayCounter[Down] Saturate: past at 0 + dec implies still at 0") {
    prove() {
      val c   = new GrayCounter(4, direction = CounterDirection.Down,
                                lower = BoundaryPolicy.Saturate)
      val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(dec) { c.decrement() }
      when(clr) { c.clear() }
      when(!past(reset) && past(c.value === U(0, 4 bits)) && past(dec) && !past(clr)) {
        assert(c.value === U(0, 4 bits))
      }
    }
  }

  test("GrayCounter[Down] Freeze: frozen is sticky and holds gray stable") {
    prove() {
      val c   = new GrayCounter(4, direction = CounterDirection.Down,
                                lower = BoundaryPolicy.Freeze)
      val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(dec) { c.decrement() }
      when(clr) { c.clear() }
      when(!past(reset) && past(c.frozen) && !past(clr)) { assert(c.frozen && stable(c.value)) }
    }
  }

  test("GrayCounter[Both] Freeze both: gray holds under any input except clr") {
    prove() {
      val c   = new GrayCounter(4, direction = CounterDirection.Both,
                                upper = BoundaryPolicy.Freeze,
                                lower = BoundaryPolicy.Freeze)
      val inc = anyseq(Bool()); val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(dec) { c.decrement() }
      when(clr) { c.clear() }
      when(!past(reset) && past(c.frozen) && !past(clr)) { assert(stable(c.value)) }
    }
  }

  test("JohnsonCounter Freeze: frozen is sticky and holds value stable") {
    prove() {
      val c   = new JohnsonCounter(4, upper = BoundaryPolicy.Freeze)
      val inc = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(clr) { c.clear() }
      when(!past(reset) && past(c.frozen) && !past(clr)) { assert(c.frozen && stable(c.value)) }
    }
  }

  // Counter[Both] Wrap/Wrap + non-pow2 span (handleOverflow=true): the stepOne-based
  // else branch that CounterUpDown(10) exercises at elaboration.

  private def bothWrapNonPow2: (Counter, Bool, Bool, Bool) = {
    val c   = new Counter(0, 9, direction = CounterDirection.Both)
    val inc = anyseq(Bool()); val dec = anyseq(Bool()); val clr = anyseq(Bool())
    when(inc) { c.increment() }
    when(dec) { c.decrement() }
    when(clr) { c.clear() }
    (c, inc, dec, clr)
  }

  test("Counter[Both] Wrap/Wrap non-pow2: value always in [0, 9]") {
    prove() { val (c, _, _, _) = bothWrapNonPow2; assert(c.value <= U(9)) }
  }

  test("Counter[Both] Wrap/Wrap non-pow2: past at end + inc-only wraps to start") {
    prove() {
      val (c, inc, dec, clr) = bothWrapNonPow2
      when(!past(reset) && past(c.value === U(9)) && past(inc) && !past(dec) && !past(clr)) {
        assert(c.value === U(0))
      }
    }
  }

  test("Counter[Both] Wrap/Wrap non-pow2: past at start + dec-only wraps to end") {
    prove() {
      val (c, inc, dec, clr) = bothWrapNonPow2
      when(!past(reset) && past(c.value === U(0)) && past(dec) && !past(inc) && !past(clr)) {
        assert(c.value === U(9))
      }
    }
  }

  test("Counter[Both] Wrap/Wrap non-pow2: simultaneous inc+dec holds value") {
    prove() {
      val (c, inc, dec, clr) = bothWrapNonPow2
      when(!past(reset) && past(inc) && past(dec) && !past(clr)) { assert(stable(c.value)) }
    }
  }

  test("Counter[Both] Wrap/Wrap non-pow2: willOverflow iff (value===end && inc && !dec)") {
    prove() {
      val (c, _, _, _) = bothWrapNonPow2
      assert(c.willOverflow === (c.value === U(9) && c.willIncrement && !c.willDecrement))
    }
  }

  test("Counter[Both] Wrap/Wrap non-pow2: willUnderflow iff (value===start && dec && !inc)") {
    prove() {
      val (c, _, _, _) = bothWrapNonPow2
      assert(c.willUnderflow === (c.value === U(0) && c.willDecrement && !c.willIncrement))
    }
  }

  // Counter[Both] Wrap/Wrap + non-pow2 with handleOverflow=false: the stepTrick
  // branch that CounterUpDown(10, …, handleOverflow=false) elaborates to.

  test("Counter[Both] handleOverflow=false non-pow2: value fits in 2^width (not stateCount)") {
    prove() {
      val c   = new Counter(0, 9, direction = CounterDirection.Both, handleOverflow = false)
      val inc = anyseq(Bool()); val dec = anyseq(Bool()); val clr = anyseq(Bool())
      when(inc) { c.increment() }
      when(dec) { c.decrement() }
      when(clr) { c.clear() }
      // width = log2Up(10) = 4, so value is a 4-bit UInt whose natural range is 0..15
      assert(c.value <= U(15, 4 bits))
    }
  }
}
