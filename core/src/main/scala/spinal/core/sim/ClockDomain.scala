package spinal.core.sim

import spinal.core.{Bool, ClockDomain, EdgeKind, HIGH, LOW, Polarity}
import spinal.core.sim._

object DoReset {
  def apply(reset : Bool, duration : Long, activeLevel: Polarity): Unit@suspendable = {
    reset #= (activeLevel match {
      case HIGH => true
      case LOW => false
    })
    sleep(duration)
    reset #= (activeLevel match {
      case HIGH => false
      case LOW => true
    })
  }
}

object DoClock {
  def apply(clk : Bool, period : Long): Unit@suspendable = {
    var value = clk.toBoolean
    while(true){
      value = !value
      clk #= value
      sleep(period >> 1)
    }
  }
}

object ForkClock {
  def apply(clk : Bool, period : Long): Unit = fork(DoClock(clk, period))
}