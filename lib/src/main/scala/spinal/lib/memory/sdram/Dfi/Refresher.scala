package spinal.lib.memory.sdram.Dfi

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.Dfi.Interface._
case class Refresher(cpa : TaskParameterAggregate) extends Component{
  import cpa._
  val io = new Bundle {
    val refresh = master(Event)
  }
  val config = TaskTimingConfig(cpa)
  val value = Reg(UInt(cp.refWidth bits)) init(0)
  val hit = value === 0
  value := value - 1
  when(hit || !config.autoRefresh) {
    value := config.REF
  }

  val pending = RegInit(False) clearWhen(io.refresh.ready) setWhen(hit) clearWhen(!config.autoRefresh)
  io.refresh.valid := pending
}
