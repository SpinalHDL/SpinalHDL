package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}

case class DfiController(bmbp: BmbParameter, task: TaskParameter, dfiConfig: DfiConfig, addrMap: AddrMap)
    extends Component {
  // Supported command-bus subset: this controller implements the fixed RAS/CAS/WE command encoding of
  // DDR3 and below. Configs enabling the DDR4 command bus (cid / act_n / bg) or disabling the mandatory
  // control signals are rejected here rather than failing with an obscure elaboration error.
  assert(
    !dfiConfig.signalConfig.useCid && !dfiConfig.signalConfig.useAckN && !dfiConfig.signalConfig.useBg &&
      dfiConfig.signalConfig.useRasN && dfiConfig.signalConfig.useCasN && dfiConfig.signalConfig.useWeN &&
      dfiConfig.signalConfig.useBank,
    "DfiController supports only DDR3-and-below command buses: requires rasN/casN/weN/bank and rejects cid/act_n/bg"
  )
  // The read-data phase alignment (RdAlignment) dynamically tracks the lane holding the first slice of
  // each burst, so any command phase in [0, frequencyRatio) is supported. The DfiMemoryAgent reference
  // model decodes the command at the configured phase and applies the same lane tracking.
  assert(
    dfiConfig.timeConfig.cmdPhase >= 0 && dfiConfig.timeConfig.cmdPhase < dfiConfig.frequencyRatio,
    "DfiController requires 0 <= cmdPhase < frequencyRatio"
  )
  // A10 carries the auto-precharge (write/read) and all-banks (precharge) bit; the command encoder
  // indexes it directly, so the command address bus must be at least 11 bits wide.
  assert(
    dfiConfig.addressWidth >= 11,
    s"DfiController requires addressWidth >= 11 (A10 precharge bit); got ${dfiConfig.addressWidth}"
  )
  // RowColumnBank interleaves burst-adjacent bits (the low transferPerBurst word bits of the column
  // stay low while the next burst-count bits map into the bank field), so consecutive bursts of one
  // station span different banks. MakeTask's station tracks a single bank per task, so this mapping
  // is rejected rather than failing with an obscure station-boundary assertion.
  assert(
    addrMap != RowColumnBank,
    "DfiController does not support RowColumnBank address mapping: it breaks the station's single-bank burst continuity"
  )
  val io = new Bundle {
    val bmb = slave(Bmb(bmbp))
    val dfi = master(Dfi(dfiConfig))
  }

  val taskConfig = BmbAdapter.taskConfig(bmbp, dfiConfig, task)

  val bmbBridge = BmbBridge(bmbp, taskConfig, dfiConfig, addrMap)
  bmbBridge.io.bmb <> io.bmb

  val control = Control(taskConfig, dfiConfig)
  control.io.input <> bmbBridge.io.taskPort

  val alignment = Alignment(dfiConfig)
  alignment.io.input <> control.io.output
  alignment.io.output <> io.dfi
}
