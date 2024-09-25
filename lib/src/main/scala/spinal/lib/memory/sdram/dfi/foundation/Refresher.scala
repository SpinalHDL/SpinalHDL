package spinal.lib.memory.sdram.dfi.foundation

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.dfi.interface._
case class Refresher(tc: TaskConfig, dc: DfiConfig) extends Component {
  import tc._
  val io = new Bundle {
    val refresh = master(Event)
  }
  val timeConfig = TaskTimingConfig(dc)
  val value = Reg(UInt(taskParameter.refWidth bits)) init (0)
  val hit = value === 0
  value := value - 1
  when(hit || !timeConfig.autoRefresh) {
    value := timeConfig.REF
  }

  val pending = RegInit(False) clearWhen (io.refresh.ready) setWhen (hit) clearWhen (!timeConfig.autoRefresh)
  io.refresh.valid := pending
}
