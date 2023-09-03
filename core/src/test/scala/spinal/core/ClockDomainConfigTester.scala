package spinal.core

import spinal.tester.SpinalTesterCocotbBase
// import spinal.core._
// import spinal.tester.scalatest.ClockDomainConfigTester.ClockDomainConfigTester
// import spinal.tester.scalatest.GrayCounterTester.GrayCounterTester
// import language.postfixOps

object ClockDomainConfigTester {
  class ClockDomainConfigTester() extends Component {
    val clk,syncReset,asyncReset,softReset,enable    = in Bool()
    val clkn,syncResetn,asyncResetn,softResetn,enablen = in Bool()

    class TestArea(cd : ClockDomain,withInit : Boolean = true) extends ClockingArea(cd){
      val regWithoutReset = out(Reg(UInt(8 bits)) randBoot())
      regWithoutReset := regWithoutReset + 1
      val regWithReset = if(withInit) {
        val reg = out(Reg(UInt(8 bits)) init(42) randBoot())
        reg := reg + 1
        reg
      } else null
    }

    val test_clk = new TestArea(ClockDomain(
      clock = clk,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = ASYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ),false)

    val test_clkn = new TestArea(ClockDomain(
      clock = clkn,
      config = ClockDomainConfig(
        clockEdge = FALLING,
        resetKind = ASYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ),false)


    val test_clk_boot = new TestArea(ClockDomain(
      clock = clk,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = BOOT,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_async_reset = new TestArea(ClockDomain(
      clock = clk,
      reset = asyncReset,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = ASYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_async_resetn = new TestArea(ClockDomain(
      clock = clk,
      reset = asyncResetn,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = ASYNC,
        resetActiveLevel = LOW,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_sync_reset = new TestArea(ClockDomain(
      clock = clk,
      reset = syncReset,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_sync_resetn = new TestArea(ClockDomain(
      clock = clk,
      reset = syncResetn,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = LOW,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_enable = new TestArea(ClockDomain(
      clock = clk,
      clockEnable = enable,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = ASYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ),false)

    val test_enablen = new TestArea(ClockDomain(
      clock = clk,
      clockEnable = enablen,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = ASYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= LOW
      )
    ),false)

    val test_sync_reset_enable = new TestArea(ClockDomain(
      clock = clk,
      reset = syncReset,
      clockEnable = enable,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_softReset = new TestArea(ClockDomain(
      clock = clk,
      softReset = softReset,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = ASYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_softResetn = new TestArea(ClockDomain(
      clock = clk,
      softReset = softResetn,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = ASYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = LOW,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_async_reset_softReset = new TestArea(ClockDomain(
      clock = clk,
      reset = asyncReset,
      softReset = softReset,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = ASYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_sync_reset_softReset = new TestArea(ClockDomain(
      clock = clk,
      reset = syncReset,
      softReset = softReset,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))



  }

}


class ClockDomainConfigTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "ClockDomainConfigTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/ClockDomainConfigTester"
  override def createToplevel: Component = new ClockDomainConfigTester.ClockDomainConfigTester()
}