package spinal.lib.bus.amba3.ahblite.sim

import hardware.Identity

import master.AhbSlaveController
import slave.AhbSlaveSubstitute

import spinal.core.sim._

package software {
  case class IdentityDut(dut: Identity) {
    val cd = dut.clockDomain
    val master = AhbSlaveController(dut.io.m, cd)
    val slave = AhbSlaveSubstitute(dut.io.s, cd)

    assert(isOk(dut.cfg))
    assert(isOk(cd))

    def init() = {
      cd.forkStimulus(period = 10)
      master.init()
      slave.init()
    }
  }
}
