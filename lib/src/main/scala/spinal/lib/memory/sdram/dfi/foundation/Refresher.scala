package spinal.lib.memory.sdram.dfi.foundation

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.dfi.interface._
case class Refresher(tpa: TaskParameterAggregate) extends Component {
  import tpa._
  val io = new Bundle {
    val refresh = master(Event)
  }
  val config = TaskTimingConfig(tpa)
  val value = Reg(UInt(tp.refWidth bits)) init (0)
  val hit = value === 0
  value := value - 1
  when(hit || !config.autoRefresh) {
    value := config.REF
  }

  val pending = RegInit(False) clearWhen (io.refresh.ready) setWhen (hit) clearWhen (!config.autoRefresh)
  io.refresh.valid := pending
}
