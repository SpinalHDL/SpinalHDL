package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.memory.sdram.dfi.function._
import spinal.lib.memory.sdram.dfi.interface._

case class DfiController(bmbp: BmbParameter, task: TaskParameter, dfiConfig: DfiConfig) extends Component {
  val io = new Bundle {
    val bmb = slave(Bmb(bmbp))
    val dfi = master(Dfi(dfiConfig))
  }

  val taskConfig = BmbAdapter.taskConfig(bmbp, dfiConfig, task)

  val bmbBridge = BmbBridge(bmbp, taskConfig, dfiConfig)
  bmbBridge.io.bmb <> io.bmb

  val control = Control(taskConfig, dfiConfig)
  control.io.inport <> bmbBridge.io.taskPort

  val alignment = Alignment(dfiConfig)
  alignment.io.inIdfiport <> control.io.outport
  alignment.io.outDfiport <> io.dfi
}
