package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.memory.sdram.dfi.foundation._
import spinal.lib.memory.sdram.dfi.interface._

case class DfiController(bmbp: BmbParameter, task: TaskParameter, dc: DfiConfig) extends Component {
  val io = new Bundle {
    val bmb = slave(Bmb(bmbp))
    val dfi = master(Dfi(dc))
  }

  val tc = BmbAdapter.taskConfig(bmbp, dc, task)

  val bmbBridge = BmbBridge(bmbp, tc, dc)
  bmbBridge.io.bmb <> io.bmb

  val control = Control(tc, dc)
  control.io.inport <> bmbBridge.io.taskPort

  val alignment = Alignment(dc)
  alignment.io.inIdfiport <> control.io.outport
  alignment.io.outDfiport <> io.dfi
}
