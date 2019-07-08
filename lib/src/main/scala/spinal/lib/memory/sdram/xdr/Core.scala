package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._

case class Core(cp : CoreParameter) extends Component {
  val io = new Bundle {
    val config = in(CoreConfig(cp))
    val ports = Vec(slave(CorePort(cp)), cp.portCount)
    val phy = master(SdramXdrPhyCtrl(cp.pl))
  }

  val refresher = Refresher(cp)
  refresher.io.config <> io.config

  val tasker = Tasker(cp)
  tasker.io.inputs <> Vec(io.ports.map(_.cmd))

  val timingEnfocer = TimingEnforcer(cp)
  timingEnfocer.io.config <> io.config
  timingEnfocer.io.input << tasker.io.output.stage()

  val backend = Backend(cp)
  backend.io.config <> io.config
  backend.io.input << timingEnfocer.io.output.stage()
  backend.io.phy <> io.phy
  backend.io.full <> timingEnfocer.io.backendFull
}