package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._

case class Refresher(cpa : CoreParameterAggregate) extends Component{
  import cpa._

  val io = new Bundle {
    val config = in(CoreConfig(cpa))
    val refresh = master(Event)
  }

  val value = Reg(UInt(cp.refWidth bits)) init(0)
  val hit = value === 0
  value := value - 1
  when(hit || !io.config.autoRefresh) {
    value := io.config.REF
  }

  val pending = RegInit(False) clearWhen(io.refresh.ready) setWhen(hit) clearWhen(!io.config.autoRefresh)
  io.refresh.valid := pending
}
