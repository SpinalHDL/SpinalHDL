package spinal.lib.memory.sdram.xdr.Dfi.CtrlWithBmb

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.xdr.Dfi.Interface._
case class Refresher(cpa : CoreParameterAggregate) extends Component{
  import cpa._
  val io = new Bundle {
//    val config = in(CoreConfig(cpa))
    val refresh = master(Event)
  }
  val config = CoreConfig(cpa)
  val value = Reg(UInt(cp.refWidth bits)) init(0)
  val hit = value === 0
  value := value - 1
  when(hit || !config.autoRefresh) {
    value := config.REF
  }

  val pending = RegInit(False) clearWhen(io.refresh.ready) setWhen(hit) clearWhen(!config.autoRefresh)
  io.refresh.valid := pending
}
