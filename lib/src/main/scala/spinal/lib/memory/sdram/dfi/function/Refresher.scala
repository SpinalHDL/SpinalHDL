package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._

case class Refresher(taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Component {
  val io = new Bundle {
    val refresh = master(Event)
  }
  val timeConfig = TaskTimingConfig(dfiConfig)
  val value = Reg(UInt(taskConfig.taskParameter.refWidth bits)) init (0)
  // tREFI pre-trigger margin, in DFI-clock units (the counter below ticks once per DFI clock, and
  // the DRAM-side watchdog measures the REF->REF gap in the same DFI-clock units, where tREFI =
  // 390 for this config). The counter reloads REF when value <= margin, so the hit period is
  // REF - margin; the REF command then lands ~`service` DFI clocks after the hit, where
  // `service` = (entry waits for the in-flight burst to drain, up to a burst length) + (FSM
  // entry->REF, ~RP + a couple). The observed REF->REF gap is therefore (REF - margin) + service
  // and JITTERS with the drain wait. If margin only covered the FSM dwell (RP + RFC) the baseline
  // gap sat ~31 DFI clocks under tREFI and the drain jitter pushed it over -> tREFI violations.
  // Adding an explicit drain-wait slack (+20) pulls the baseline gap down so the jitter stays
  // inside tREFI. Refreshing a little more often than tREFI is always spec-safe (tREFI is a max).
  val margin = timeConfig.RP + timeConfig.RFC + 20
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
