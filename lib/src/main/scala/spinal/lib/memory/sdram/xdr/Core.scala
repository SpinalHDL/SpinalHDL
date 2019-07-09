package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._

case class Core(cp : CoreParameter) extends Component {
  val io = new Bundle {
    val config = in(CoreConfig(cp))
    val init = slave(InitBus(cp))
    val ports = Vec(slave(CorePort(cp)), cp.portCount)
    val phy = master(SdramXdrPhyCtrl(cp.pl))
  }

  val refresher = Refresher(cp)
  refresher.io.config <> io.config

  val tasker = Tasker(cp)
  tasker.io.inputs <> Vec(io.ports.map(_.cmd))
  tasker.io.refresh <> refresher.io.refresh

  val timingEnforcer = TimingEnforcer(cp)
  timingEnforcer.io.config <> io.config
  timingEnforcer.io.input << tasker.io.output.stage()

  val backend = Backend(cp)
  backend.io.config <> io.config
  backend.io.input << timingEnforcer.io.output.stage()
  backend.io.phy <> io.phy
  backend.io.full <> tasker.io.backendFull
  backend.io.init <> io.init
}