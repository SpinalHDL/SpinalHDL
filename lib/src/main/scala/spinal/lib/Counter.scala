package spinal.lib

import spinal.core._

sealed trait CounterDirection
object CounterDirection {
  case object Up   extends CounterDirection
  case object Down extends CounterDirection
  case object Both extends CounterDirection
}

sealed trait BoundaryPolicy
object BoundaryPolicy {
  case object Wrap     extends BoundaryPolicy
  case object Saturate extends BoundaryPolicy
  case object Freeze   extends BoundaryPolicy
}

trait CounterLike[T <: BitVector] extends ImplicitArea[T] {
  def value: T
  def valueNext: T
  val willClear: Bool
  val willLoad: Bool
  val willAdvance: Bool
  val willComplete: Bool

  def stateCount: BigInt

  def clear(): Unit = willClear := True

  def load(raw: T): Unit = {
    valueNext := raw
    willLoad := True
  }

  def frozen: Bool = False

  def toFlow(): Flow[T] = {
    val flow = Flow(cloneOf(value))
    flow.payload := value
    flow.valid := willAdvance
    flow
  }

  override def implicitValue: T = value
}

trait CounterAddressable[T <: BitVector] extends CounterLike[T] {
  def loadOrdinal(index: UInt): Unit
  def loadOrdinal(index: Int   ): Unit = loadOrdinal(U(index, log2Up(stateCount) bits))
  def loadOrdinal(index: BigInt): Unit = loadOrdinal(U(index, log2Up(stateCount) bits))
}

abstract class BoundedCounter[T <: BitVector](
  val direction: CounterDirection,
  val upper: BoundaryPolicy,
  val lower: BoundaryPolicy
) extends ImplicitArea[T]
  with CounterLike[T]
  with CounterAddressable[T] {

  protected val hasUp: Boolean = direction != CounterDirection.Down
  protected val hasDown: Boolean = direction != CounterDirection.Up

  val willIncrement = False.allowOverride
  val willDecrement = False.allowOverride
  val willClear     = False.allowOverride
  val willLoad      = False.allowOverride

  def willOverflowIfInc: Bool
  def willUnderflowIfDec: Bool

  lazy val willOverflow  = Counter.guardedComplete(direction)(willOverflowIfInc,  willIncrement, willDecrement)
  lazy val willUnderflow = Counter.guardedComplete(direction)(willUnderflowIfDec, willDecrement, willIncrement)
  lazy val willAdvance   = Counter.byDirection(direction)(willIncrement, willDecrement)
  lazy val willComplete  = Counter.byDirection(direction)(willOverflow,  willUnderflow)

  private lazy val freezeReg: Bool = Counter.freezeLatch(
    upperFreeze = hasUp   && upper == BoundaryPolicy.Freeze,
    lowerFreeze = hasDown && lower == BoundaryPolicy.Freeze,
    willOverflow, willUnderflow, willClear, willLoad
  )
  override def frozen: Bool = freezeReg

  protected lazy val effectiveInc: Bool =
    if (hasUp   && upper == BoundaryPolicy.Freeze) willIncrement && !freezeReg else willIncrement
  protected lazy val effectiveDec: Bool =
    if (hasDown && lower == BoundaryPolicy.Freeze) willDecrement && !freezeReg else willDecrement

  protected lazy val incOnly: Bool = effectiveInc && !effectiveDec
  protected lazy val decOnly: Bool = effectiveDec && !effectiveInc

  protected def enableStandardPruning(): Unit = {
    willOverflowIfInc.allowPruning()
    willOverflow.allowPruning()
    willUnderflowIfDec.allowPruning()
    willUnderflow.allowPruning()
  }

  private def kindName: String = getClass.getSimpleName

  def increment(): Unit = {
    require(hasUp, s"$kindName.increment() requires direction Up or Both, got $direction")
    willIncrement := True
  }

  def decrement(): Unit = {
    require(hasDown, s"$kindName.decrement() requires direction Down or Both, got $direction")
    willDecrement := True
  }

  def freeRun(): this.type = {
    require(hasUp, s"$kindName.freeRun() requires direction Up or Both, got $direction")
    willIncrement.removeAssignments()
    willIncrement := True
    this
  }

  def freeRunDown(): this.type = {
    require(hasDown, s"$kindName.freeRunDown() requires direction Down or Both, got $direction")
    willDecrement.removeAssignments()
    willDecrement := True
    this
  }

  def saturatedHigh: Bool = {
    require(hasUp, s"$kindName.saturatedHigh requires direction Up or Both, got $direction")
    if (upper == BoundaryPolicy.Saturate) willOverflowIfInc else False
  }

  def saturatedLow: Bool = {
    require(hasDown, s"$kindName.saturatedLow requires direction Down or Both, got $direction")
    if (lower == BoundaryPolicy.Saturate) willUnderflowIfDec else False
  }
}

/** Creates an always running counter
  *
  * See [[https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Libraries/utils.html?highlight=counter#counter]]
  */
object CounterFreeRun {
  def apply(stateCount: BigInt): Counter = Counter(stateCount).freeRun()
  def apply(bitCount: BitCount): Counter = Counter(bitCount).freeRun()
}

/** Creates a counter
  *
  * See [[https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Libraries/utils.html?highlight=counter#counter]]
  */
object Counter {

  private[lib] def freezeLatch(upperFreeze: Boolean, lowerFreeze: Boolean,
                               willOverflow: Bool, willUnderflow: Bool,
                               willClear: Bool, willLoad: Bool): Bool = {
    if (!upperFreeze && !lowerFreeze) return False
    val setTrig =
      if (upperFreeze && lowerFreeze) willOverflow || willUnderflow
      else if (upperFreeze) willOverflow
      else willUnderflow
    RegInit(False).setWhen(setTrig).clearWhen(willClear || willLoad)
  }

  private[lib] def byDirection(direction: CounterDirection)(up: => Bool, down: => Bool): Bool =
    direction match {
      case CounterDirection.Up => up
      case CounterDirection.Down => down
      case CounterDirection.Both => up || down
    }

  private[lib] def guardedComplete(direction: CounterDirection)
                                  (ifSig: Bool, trig: Bool, cancel: Bool): Bool =
    if (direction == CounterDirection.Both) ifSig && trig && !cancel
    else                                    ifSig && trig

  /** Create a counter on `[start, end]` */
  def apply(start: BigInt, end: BigInt): Counter = new Counter(start, end)

  /** Create a counter on `[range.low, range.high]` */
  def apply(range: Range): Counter = {
    require(range.step == 1)
    apply(range.low, range.high)
  }

  /** Create a counter on `[0, stateCount-1]` */
  def apply(stateCount: BigInt): Counter = new Counter(0, stateCount - 1)

  /** Create a counter on `[0, 2^bitCount-1]` */
  def apply(bitCount: BitCount): Counter = new Counter(0, (BigInt(1) << bitCount.value) - 1)

  /** Create a counter on `[start, end]` with `inc` signal as increment enable */
  def apply(start: BigInt, end: BigInt, inc: Bool): Counter = {
    val c = apply(start, end)
    when(inc) { c.increment() }
    c
  }

  /** Create a counter on `[range.low, range.high]` with `inc` signal as increment enable */
  def apply(range: Range, inc: Bool): Counter = {
    require(range.step == 1)
    apply(range.low, range.high, inc)
  }

  /** Create a counter on `[0, stateCount-1]` with `inc` signal as increment enable */
  def apply(stateCount: BigInt, inc: Bool): Counter = apply(0, stateCount - 1, inc)

  /** Create a counter on `[0, 2^bitCount-1]` with `inc` signal as increment enable */
  def apply(bitCount: BitCount, inc: Bool): Counter = apply(0, (BigInt(1) << bitCount.value) - 1, inc)

  /** Create a counter on `[0, Clocks for given Time]` */
  def apply(time: TimeNumber): Counter = apply(
    ((time.toBigDecimal * ClockDomain.current.frequency.getValue.toBigDecimal)
      .setScale(0, BigDecimal.RoundingMode.UP)).toBigInt
  )

  /** Create a counter on `[0, Clocks for given Time]` with `inc` signal as increment enable */
  def apply(time: TimeNumber, inc: Bool): Counter = apply(
    ((time.toBigDecimal * ClockDomain.current.frequency.getValue.toBigDecimal)
      .setScale(0, BigDecimal.RoundingMode.UP)).toBigInt,
    inc
  )

  def down(stateCount: BigInt): Counter = new Counter(0, stateCount - 1, CounterDirection.Down)
  def both(stateCount: BigInt): Counter = new Counter(0, stateCount - 1, CounterDirection.Both)
}

// start and end inclusive. `handleOverflow=false` opts into 2^width modular wrap
// for Both + both-Wrap + non-pow2 + start==0; default (true) wraps at stateCount.
class Counter(
  val start: BigInt,
  val end: BigInt,
  direction: CounterDirection = CounterDirection.Up,
  upper: BoundaryPolicy = BoundaryPolicy.Wrap,
  lower: BoundaryPolicy = BoundaryPolicy.Wrap,
  val handleOverflow: Boolean = true
) extends BoundedCounter[UInt](direction, upper, lower) {

  require(start <= end)

  private val initVal: BigInt = if (direction == CounterDirection.Down) end else start
  private val w = log2Up(end + 1)

  val valueNext = UInt(w bit)
  val value = RegNext(valueNext) init(initVal)

  val willOverflowIfInc  = value === end
  val willUnderflowIfDec = value === start

  def stepOne(arith: UInt, boundary: Bool, policy: BoundaryPolicy,
              wrapTo: BigInt, pinTo: BigInt): Unit = {
    valueNext := arith.resized
    val naturalWrap = policy == BoundaryPolicy.Wrap && isPow2(end + 1) && start == 0
    if (!naturalWrap) {
      val pin = if (policy == BoundaryPolicy.Wrap) wrapTo else pinTo
      when(boundary) { valueNext := U(pin, w bits) }
    }
  }

  direction match {
    case CounterDirection.Up =>
      stepOne(value + U(effectiveInc), willOverflow,  upper, wrapTo = start, pinTo = end)
    case CounterDirection.Down =>
      stepOne(value - U(effectiveDec), willUnderflow, lower, wrapTo = end,   pinTo = start)
    case CounterDirection.Both =>
      val bothWrap = upper == BoundaryPolicy.Wrap && lower == BoundaryPolicy.Wrap
      val span = end - start + 1
      // stepTrick: `step.maxValue` acts as `-1` only when the register wraps at
      // exactly `span`. Natural for pow2+start==0, or opted into via handleOverflow=false.
      val stepTrick = bothWrap && start == 0 && (isPow2(span) || !handleOverflow)

      if (stepTrick) {
        val step = UInt(log2Up(span) bit)
        when(incOnly) { step := 1 }
          .elsewhen(decOnly) { step := step.maxValue }
          .otherwise { step := 0 }
        valueNext := (value + step).resized
      } else {
        valueNext := value
        when(incOnly) { stepOne(value + 1, willOverflow, upper, wrapTo = start, pinTo = end) }
        when(decOnly) { stepOne(value - 1, willUnderflow, lower, wrapTo = end, pinTo = start) }
      }
  }

  when(willClear) { valueNext := U(initVal, w bits) }

  enableStandardPruning()
  willOverflow.setCompositeName(this, "willOverflow", true)
  willUnderflow.setCompositeName(this, "willUnderflow", true)

  def stateCount: BigInt = end - start + 1

  def loadOrdinal(index: UInt): Unit = load((index.resize(w) + U(start, w bits)).resized)

  def init(initValue: BigInt): this.type = {
    value.removeInitAssignments()
    value.init(initValue)
    this
  }

  def ===(that: UInt): Bool = value === that
  def =/=(that: UInt): Bool = value =/= that
  def !==(that: UInt): Bool = =/=(that)

  override def implicitValue: UInt = this.value
}


class CounterUpDown(
  stateCountArg: BigInt,
  handleOverflow: Boolean = true
) extends Counter(0, stateCountArg - 1, CounterDirection.Both, handleOverflow = handleOverflow) {
  val incrementIt = willIncrement
  val decrementIt = willDecrement
  val mayOverflow = willOverflowIfInc
  val mayUnderflow = willUnderflowIfDec
}

object CounterUpDown {
  def apply(stateCount: BigInt): CounterUpDown = new CounterUpDown(stateCount)

  def apply(stateCount: BigInt, incWhen: Bool, decWhen: Bool): CounterUpDown =
    apply(stateCount, incWhen, decWhen, handleOverflow = true)

  def apply(stateCount: BigInt, incWhen: Bool, decWhen: Bool, handleOverflow: Boolean): CounterUpDown = {
    val c = new CounterUpDown(stateCount, handleOverflow)
    when(incWhen) { c.increment() }
    when(decWhen) { c.decrement() }
    c
  }
}

object DownCounter {
  def apply(stateCount: BigInt): Counter = Counter.down(stateCount)

  def apply(stateCount: BigInt, dec: Bool): Counter = {
    val c = Counter.down(stateCount)
    when(dec) { c.decrement() }
    c
  }

  def apply(bitCount: BitCount): Counter =
    new Counter(0, (BigInt(1) << bitCount.value) - 1, CounterDirection.Down)
}


// One-hot encoded counter with stateCount states
class OneHotCounter(
  val stateCount: BigInt,
  val initialValue: BigInt = 0,
  direction: CounterDirection = CounterDirection.Up,
  upper: BoundaryPolicy = BoundaryPolicy.Wrap,
  lower: BoundaryPolicy = BoundaryPolicy.Wrap
) extends BoundedCounter[Bits](direction, upper, lower) {

  require(stateCount > 0)
  require(initialValue >= 0 && initialValue < stateCount)

  private val resetValue = Bits(stateCount bits)
  resetValue := B(BigInt(1) << initialValue.toInt, stateCount bits)

  val valueNext = Bits(stateCount bits)
  val value = RegNext(valueNext) init(resetValue)

  val willOverflowIfInc  = value.msb
  val willUnderflowIfDec = value.lsb

  valueNext := value

  if (hasUp) {
    val guard = if (upper == BoundaryPolicy.Wrap) incOnly else incOnly && !willOverflowIfInc
    when(guard) { valueNext := value.rotateLeft(1) }
  }
  if (hasDown) {
    val guard = if (lower == BoundaryPolicy.Wrap) decOnly else decOnly && !willUnderflowIfDec
    when(guard) { valueNext := value.rotateRight(1) }
  }
  when(willClear) { valueNext := resetValue }

  enableStandardPruning()

  def ===(that: Bits  ): Bool = value === that
  def ===(that: Int   ): Bool = value(that)
  def ===(that: BigInt): Bool = value(that.toInt)
  def ===(that: UInt  ): Bool = value === UIntToOh(that, stateCount.toInt)

  def =/=(that: Bits  ): Bool = value =/= that
  def =/=(that: Int   ): Bool = !value(that)
  def =/=(that: BigInt): Bool = !value(that.toInt)
  def =/=(that: UInt  ): Bool = value =/= UIntToOh(that, stateCount.toInt)

  def !==(that: Bits  ): Bool = =/=(that)
  def !==(that: Int   ): Bool = =/=(that)
  def !==(that: BigInt): Bool = =/=(that)
  def !==(that: UInt  ): Bool = =/=(that)

  override def implicitValue: Bits = this.value

  def load(index: Int ): Unit = { valueNext := B(BigInt(1) << index, stateCount bits); willLoad := True }
  def load(index: UInt): Unit = { valueNext := UIntToOh(index, stateCount.toInt);     willLoad := True }

  def loadOrdinal(index: UInt): Unit = load(index)

  private def reinit(newReset: Bits): this.type = {
    resetValue.removeAssignments()
    resetValue := newReset
    value.removeInitAssignments()
    value.init(resetValue)
    this
  }

  def init(initValue: Int): this.type = reinit(B(BigInt(1) << initValue, stateCount bits))
  def init(initValue: BigInt): this.type = init(initValue.toInt)
  def init(initValue: Bits): this.type = reinit(initValue)
  def init(initValue: UInt): this.type = reinit(UIntToOh(initValue, stateCount.toInt))
}

/** Creates a one-hot encoded counter */
object OneHotCounter {
  /** Create a one-hot counter with `stateCount` states */
  def apply(stateCount: BigInt): OneHotCounter = new OneHotCounter(stateCount)

  /** Create a one-hot counter with `bitCount` states */
  def apply(bitCount: BitCount): OneHotCounter = new OneHotCounter(bitCount.value)

  /** Create a one-hot counter with `stateCount` states and `inc` signal as increment enable */
  def apply(stateCount: BigInt, inc: Bool): OneHotCounter = {
    val c = new OneHotCounter(stateCount)
    when(inc) { c.increment() }
    c
  }

  /** Create a one-hot counter with `2^bitCount` states and `inc` signal as increment enable */
  def apply(bitCount: BitCount, inc: Bool): OneHotCounter =
    apply(BigInt(1) << bitCount.value, inc)

  def up(stateCount: BigInt): OneHotCounter = new OneHotCounter(stateCount, direction = CounterDirection.Up)
  def down(stateCount: BigInt): OneHotCounter = new OneHotCounter(stateCount, direction = CounterDirection.Down)
  def both(stateCount: BigInt): OneHotCounter = new OneHotCounter(stateCount, direction = CounterDirection.Both)
}

/** Creates a Johnson counter (also known as a twisted-ring or Möbius counter):
  * a shift register whose inverted MSB feeds back into the LSB, producing a `2*width`-state
  * sequence with only one bit transition per cycle.
  */
object JohnsonCounter {
  /** Create a Johnson counter of the given width */
  def apply(width: Int): JohnsonCounter = new JohnsonCounter(width)

  /** Create a Johnson counter of the given width with `inc` signal as increment enable */
  def apply(width: Int, inc: Bool): JohnsonCounter = {
    val c = JohnsonCounter(width)
    when(inc) { c.increment() }
    c
  }
}

// Johnson (twisted-ring) counter with `2*width` legal states, self-recovering from illegal states
class JohnsonCounter(
  val width: Int,
  val upper: BoundaryPolicy = BoundaryPolicy.Wrap
) extends ImplicitArea[Bits]
  with CounterLike[Bits] {
  require(width >= 2, "JohnsonCounter needs at least 2 bits for stuck-state recovery")
  require(upper != BoundaryPolicy.Saturate, "Johnson counter does not support Saturate")

  val willIncrement = False.allowOverride
  val willClear = False.allowOverride
  val willLoad = False.allowOverride

  val value = Reg(Bits(width bits)).initZero()
  val valueNext = cloneOf(value)
  valueNext := value

  // True on the end-of-cycle legal state 10..0 and on any illegal state whose top two bits are 10;
  // both snap to 0 on increment. Remaining illegal states reach a detected state within a few shifts,
  // so the counter is self-recovering but not necessarily in a single increment.
  val willOverflowIfInc = value(width - 1) && !value(width - 2)
  val willOverflow = willOverflowIfInc && willIncrement

  val willAdvance: Bool = willIncrement
  val willComplete: Bool = willOverflow
  def stateCount: BigInt = 2 * width

  private val freezeReg: Bool =
    if (upper == BoundaryPolicy.Freeze)
      RegInit(False).setWhen(willOverflow).clearWhen(willClear || willLoad)
    else False
  override def frozen: Bool = freezeReg

  private val effectiveInc =
    if (upper == BoundaryPolicy.Freeze) willIncrement && !freezeReg else willIncrement

  when(effectiveInc) {
    valueNext := willOverflowIfInc ? B(0, width bits) |
                   (value(width - 2 downto 0) ## !value(width - 1))
  }
  when(willClear) { valueNext := 0 }

  value := valueNext

  willOverflowIfInc.allowPruning()
  willOverflow.allowPruning()

  def increment(): Unit = willIncrement := True

  /** Make this counter free-running (increments every cycle) */
  def freeRun(): this.type = {
    willIncrement.removeAssignments()
    willIncrement := True
    this
  }

  /** A 50%-duty-cycle signal at 1/(2*width) of the clock. */
  def clkDiv: Bool = value((width - 1) / 2)

  override def implicitValue: Bits = value
}


// Gray counter with 2^width states. Bidirectional via parity flip:
//   increment: word = Cat(1, gray[width-3:0],  even)
//   decrement: word = Cat(1, gray[width-3:0], !even)
class GrayCounter(
  val width: Int,
  direction: CounterDirection = CounterDirection.Up,
  upper: BoundaryPolicy = BoundaryPolicy.Wrap,
  lower: BoundaryPolicy = BoundaryPolicy.Wrap
) extends BoundedCounter[UInt](direction, upper, lower) {

  require(width >= 2, "GrayCounter needs width >= 2")

  val value = Reg(UInt(width bits)) init(0)
  val valueNext = UInt(width bits)
  private[lib] val even = RegInit(True)

  // Top (ordinal = 2^N - 1) is the MSB-only pattern `1 << (N-1)`; bottom is all zeros.
  private[lib] val topState = U(BigInt(1) << (width - 1), width bits)
  val willOverflowIfInc  = value === topState
  val willUnderflowIfDec = value === U(0, width bits)

  private val upperBlock: Bool = if (upper == BoundaryPolicy.Wrap) False else willOverflowIfInc
  private val lowerBlock: Bool = if (lower == BoundaryPolicy.Wrap) False else willUnderflowIfDec

  private val shouldInc = incOnly && !upperBlock
  private val shouldDec = decOnly && !lowerBlock

  private val midSlice = value(width - 3 downto 0).asBits
  private val incWord = Cat(True, midSlice,  even)
  private val decWord = Cat(True, midSlice, !even)

  valueNext := value

  // Flip the first set bit of `word`; direction is in `word` (inc vs dec).
  private def applyGrayStep(step: Bool, word: Bits): Unit = {
    when(step) {
      val next = CombInit(value)
      var found = False
      for (i <- 0 until width) {
        when(word(i) && !found) {
          next(i) := !value(i)
          found \= True
        }
      }
      valueNext := next
      even := !even
    }
  }

  if (hasUp)   applyGrayStep(shouldInc, incWord)
  if (hasDown) applyGrayStep(shouldDec, decWord)
  when(willClear) { valueNext := 0; even := True }

  value := valueNext

  enableStandardPruning()

  def stateCount: BigInt = BigInt(1) << width

  def loadOrdinal(index: UInt): Unit = {
    val i = index.resize(width)
    valueNext := toGray(i).asUInt
    willLoad := True
    even := !i.lsb
  }

  override def load(raw: UInt): Unit = {
    valueNext := raw
    willLoad := True
    even := !raw.xorR
  }
}

object GrayCounter {
  def apply(width: Int): GrayCounter = new GrayCounter(width)

  /** Function-style: returns the underlying gray `UInt` directly, gated by `enable` */
  def apply(width: Int, enable: Bool): UInt = {
    val c = new GrayCounter(width)
    when(enable) { c.increment() }
    c.value
  }

  def up(width: Int): GrayCounter = new GrayCounter(width, CounterDirection.Up)
  def down(width: Int): GrayCounter = new GrayCounter(width, CounterDirection.Down)
  def both(width: Int): GrayCounter = new GrayCounter(width, CounterDirection.Both)
}


object CounterMultiRequest {
  def apply(width: Int, requests : (Bool,(UInt) => UInt)*): UInt = {
    val counter = Reg(UInt(width bit)) init(0)
    var counterNext = cloneOf(counter)
    counterNext := counter
    for((cond,func) <- requests){
      when(cond){
        counterNext \= func(counterNext)
      }
    }
    counter := counterNext
    counter
  }
}
