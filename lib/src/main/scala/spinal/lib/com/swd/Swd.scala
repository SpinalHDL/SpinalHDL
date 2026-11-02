package spinal.lib.com.swd

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState

/**
 * ARM Serial Wire Debug (ADIv5/ADIv6) pins: a probe-driven clock and one bidirectional data line.
 *
 * SWDIO is push-pull tristate, not open-drain: the host drives 0 and 1 during the request,
 * WDATA and line reset, the target drives 0 and 1 during ACK and RDATA, and both release the
 * line for the turnarounds. Hence a TriState, which InOutWrapper turns into a real inout.
 *
 * master = the host (debug probe), slave = the target. Both ends own a tristate driver, so
 * asSlave() is not the plain flip of asMaster(): only SWCLK changes direction.
 */
case class Swd(useSwclk : Boolean = true) extends Bundle with IMasterSlave {
  val swclk = if(useSwclk) Bool() else null
  val swdio = TriState(Bool())

  override def asMaster(): Unit = {
    if(useSwclk) out(swclk)
    master(swdio)
  }

  override def asSlave(): Unit = {
    if(useSwclk) in(swclk)
    master(swdio)
  }

  /**
   * The clock domain SwdPhy / SwdDp run in. BOOT reset: the 2-wire interface has no reset
   * wire — line reset (50+ SWCLK cycles with SWDIO high) is the protocol-level reset.
   */
  def clockDomain = ClockDomain(clock = swclk, config = ClockDomainConfig(resetKind = BOOT))
}
