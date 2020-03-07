package spinal.tester.pending

import spinal.core.ClockDomain
import spinal.core.sim._
import spinal.lib.soc.pinsec.{Pinsec, PinsecConfig}

object SpinalSimPinsec {
  def main(args: Array[String]): Unit = {
    SimConfig
      .allOptimisation
      .doSimUntilVoid(new Pinsec(PinsecConfig.default)){dut =>
        ClockDomain(dut.io.axiClk, dut.io.asyncReset).forkStimulus(10)
        ClockDomain(dut.io.axiClk, dut.io.asyncReset).forkSimSpeedPrinter()
      }
  }
}
