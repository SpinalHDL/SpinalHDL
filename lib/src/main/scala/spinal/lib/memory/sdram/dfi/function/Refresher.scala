package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._

case class Refresher(taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Component {
  val io = new Bundle {
    val refresh = master(Event)
  }
  val timeConfig = TaskTimingConfig(dfiConfig)
  val value = Reg(UInt(taskConfig.taskParameter.refWidth bits)) init (0)
  // tREFI pre-trigger margin: the counter counts down from REF, so the request must fire
  // `margin` MC cycles early for the REF command itself to land inside the tREFI window.
  val margin = timeConfig.RFC + 2
  val hit = value <= U(margin, taskConfig.taskParameter.refWidth bits)
  value := value - 1
  // The refresh request is serviced by the MakeTask refresh FSM, which precharges every bank and
  // then idles tRFC before the REF command leaves: the wall-clock REF -> REF gap is the counter
  // period PLUS that service latency. Triggering at zero therefore issues refreshes tRFC+2 MC
  // cycles too late and the observed gap drifts past the JESD79-4B tREFI window (data-retention
  // violation; the DRAM-side watchdog in DfiMemoryAgent measures exactly that gap). Pre-trigger by
  // the service latency so the gap lands on tREFI.
  when(hit || !timeConfig.autoRefresh) {
    value := timeConfig.REF
  }

  val pending = RegInit(False) clearWhen (io.refresh.ready) setWhen (hit) clearWhen (!timeConfig.autoRefresh)
  io.refresh.valid := pending
}
