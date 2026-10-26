package spinal.lib.com.swd

import spinal.core._
import spinal.lib._
import spinal.lib.io.InOutWrapper
import spinal.tester.SpinalAnyFunSuite

/** Minimal SWD target: the pins, and PHY + DP clocked by SWCLK. */
case class SwdBundleTestTop() extends Component {
  val io = new Bundle {
    val swd = slave(Swd())
  }
  val core = io.swd.clockDomain on SwdPhyDp()
  io.swd.swdio << core.io.swdio
  core.io.ap.rsp.setIdle()
}

/**
 * Run: sbt "tester/testOnly spinal.lib.com.swd.SwdBundleTest"
 */
class SwdBundleTest extends SpinalAnyFunSuite {
  test("slave(Swd()) only flips SWCLK: the target still owns a tristate driver") {
    val top = SpinalConfig().generateVerilog(SwdBundleTestTop()).toplevel
    assert(top.io.swd.swclk.isInput)
    assert(top.io.swd.swdio.read.isInput)
    assert(top.io.swd.swdio.write.isOutput)
    assert(top.io.swd.swdio.writeEnable.isOutput)
  }

  test("InOutWrapper turns SWDIO into a single inout pin") {
    val top = SpinalConfig().generateVerilog(InOutWrapper(SwdBundleTestTop())).toplevel
    val ios = top.getAllIo.map(io => io.getName() -> io).toMap
    assert(ios("io_swd_swdio").isInOut)
    assert(ios("io_swd_swclk").isInput)
    assert(ios.size == 2)
  }
}
