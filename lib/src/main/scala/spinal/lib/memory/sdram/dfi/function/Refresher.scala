package spinal.lib.memory.sdram.dfi.function

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.dfi.interface._
case class Refresher(taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Component {
  import taskConfig._
  val io = new Bundle {
    val refresh = master(Event)
  }
  val timeConfig = TaskTimingConfig(dfiConfig)
  val value = Reg(UInt(taskParameter.refWidth bits)) init (0)
  val hit = value === 0
  value := value - 1
  when(hit || !timeConfig.autoRefresh) {
    value := timeConfig.REF
  }

  val pending = RegInit(False) clearWhen (io.refresh.ready) setWhen (hit) clearWhen (!timeConfig.autoRefresh)
  io.refresh.valid := pending
}
