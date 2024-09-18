package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.Bmb
import spinal.lib.memory.sdram.dfi.foundation._
import spinal.lib.memory.sdram.dfi.interface._

case class DfiController(ctp: CtrlParameter, pl: PhyConfig, config: DfiConfig) extends Component {
  val io = new Bundle {
    val bmb = slave(Bmb(ctp.bmbp))
    val dfi = master(Dfi(config))
  }

  val tpa = TaskParameterAggregate(ctp.task, pl, BmbAdapter.taskPortParameter(ctp.bmbp, pl, ctp.task), config)

  val bmbBridge = BmbBridge(ctp.bmbp, tpa)
  bmbBridge.io.bmb <> io.bmb

  val control = Control(tpa)
  control.io.inport <> bmbBridge.io.taskPort

  val alignment = Alignment(tpa.config)
  alignment.io.inIdfiport <> control.io.outport
  alignment.io.outDfiport <> io.dfi
}
