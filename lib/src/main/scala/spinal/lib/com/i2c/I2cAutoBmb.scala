package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._

/**
 * Bmb-specific glue for the I2cAuto register bus.
 *
 * `I2cAutoMaster` / `I2cAutoSlave` are bus-agnostic: their register map is bound
 * through a caller-supplied `BusSlaveFactory` (see `driveFrom`). This object only
 * provides the *recommended* Bmb port parameter for integrators who wire the
 * register map with a Bmb bus, so that the controllers themselves carry no
 * dependency on any particular bus type.
 *
 * Contract implied by the register map (see the companion `driveFrom`):
 *   - address width >= 7 bits (highest offset 0x38)
 *   - data width 32 bits (widest register access)
 *   - single-beat only: `BmbSlaveFactory` hardcodes `rsp.last := True` and
 *     performs no beat iteration, so `lengthWidth = 2` keeps `allowBurst = false`
 *   - `canMask = true` (BmbSourceParameter default): `BmbSlaveFactory.writeByteEnable()`
 *     references `bus.cmd.mask` unconditionally
 */
object I2cAutoBmb {
  val registerBusParameter: BmbParameter = BmbAccessParameter(8, 32)
    .addSources(1, BmbSourceParameter(contextWidth = 0, lengthWidth = 2))
    .toBmbParameter()
}
