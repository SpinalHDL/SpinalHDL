package spinal.core

import spinal.lib.{ResetCtrl, CounterFreeRun, Counter}
import spinal.tester.SpinalTesterCocotbBase


object PllAAssertSDeassertTester{
  class PLL extends BlackBox{
    val io = new Bundle{
      val clk_in = in Bool()
      val clk_out = out Bool()
    }

    noIoPrefix()
  }

  class PllAAssertSDeassertTester extends Component{
    val aReset     = in Bool()
    val clk_100Mhz = in Bool()

    val pllBB = new PLL
    pllBB.io.clk_in := clk_100Mhz

//    val coreClockDomain = ClockDomain(
//      clock = pllBB.io.clk_out,
//      reset = Bool.setName("coreReset")
//    )
    val coreClockDomain = ClockDomain.internal("core")
    coreClockDomain.clock := pllBB.io.clk_out
    coreClockDomain.reset := ResetCtrl.asyncAssertSyncDeassert(
      input       = aReset,
      clockDomain = coreClockDomain
    )

    val counter = out(coreClockDomain(CounterFreeRun(10).value)).setName("counter")
  }
}




class PllAAssertSDeassertTesterBoot extends SpinalTesterCocotbBase {
  override def getName: String = "PllAAssertSDeassertTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/PllAAssertSDeassertTester"
  override def createToplevel: Component = new PllAAssertSDeassertTester.PllAAssertSDeassertTester

  override def backendConfig(config: SpinalConfig): SpinalConfig = config
    .copy(defaultConfigForClockDomains = config.defaultConfigForClockDomains.copy(resetKind = SYNC))
}