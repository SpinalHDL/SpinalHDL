package spinal.lib.formal

import spinal.core._
import spinal.core.formal._
import spinal.lib._

case class GlobalClock() {
  val domain = ClockDomain.internal("_global").withBootReset()
  domain.clock.addAttribute("gclk")

  def assumeClockTiming(target: ClockDomain, period: Int, aligned: Boolean = false) = new ClockingArea(domain) {
    val timer = CounterFreeRun(period)
    val phase = if (!aligned) timer.value + anyconst(cloneOf(timer.value)) else timer.value
    assume(target.readClockWire === phase(timer.getBitsWidth - 1))
  }

  def assumeResetReleaseSync(target: ClockDomain) = new ClockingArea(domain) {
    if (target.hasResetSignal) {
      val activeEdge = if (target.config.clockEdge == RISING) rose(target.readClockWire) else fell(target.readClockWire)
      if (target.config.resetKind == SYNC) {
        when(pastValid & !activeEdge) { assume(!changed(target.isResetActive)) }
      } else {
        when(pastValid & !activeEdge) { assume(!fell(target.isResetActive)) }
      }
    }
  }

  def assumeIOSync2Clock(target: ClockDomain, signal: Data) = new ClockingArea(domain) {
    val activeEdge = if (target.config.clockEdge == RISING) rose(target.readClockWire) else fell(target.readClockWire)
    when(pastValid & !activeEdge) { assume(!changed(signal)) }
  }

  def keepBoolLeastCycles(target: Bool, period: Int) = new ClockingArea(domain) {
    val timer = Timeout(period)
    when(!target & timer.counter.value === 0) { timer.clear() }
    when(timer.counter.value > 0) { assume(target === True) }
  }

  def alignAsyncResetStart(src: ClockDomain, dst: ClockDomain) = new ClockingArea(domain) {
    if (src.hasResetSignal && dst.hasResetSignal && src.config.resetKind == ASYNC && dst.config.resetKind == ASYNC) {
      assume(rose(src.isResetActive) === rose(dst.isResetActive))
      when(!src.isResetActive & rose(dst.readClockWire)) { assume(dst.isResetActive === False) }
      when(!dst.isResetActive & rose(src.readClockWire)) { assume(src.isResetActive === False) }
    }
  }
}
