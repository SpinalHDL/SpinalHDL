package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}

case class DfiController(bmbp: BmbParameter, task: TaskParameter, dfiConfig: DfiConfig, addrMap: AddrMap)
    extends Component {
  // Supported command-bus subset: DDR3-and-below configs use the fixed RAS/CAS/WE encoding;
  // DDR4 configs additionally enable the DDR4 command bus (act_n / bg / cid). Anything else is
  // rejected here rather than failing with an obscure elaboration error.
  val isDdr4 = dfiConfig.sdram.generation == SdramGeneration.DDR4
  assert(
    dfiConfig.signalConfig.useRasN && dfiConfig.signalConfig.useCasN && dfiConfig.signalConfig.useWeN &&
      dfiConfig.signalConfig.useBank &&
      (if (isDdr4) dfiConfig.signalConfig.useAckN && dfiConfig.signalConfig.useBg && dfiConfig.signalConfig.useCid
       else !dfiConfig.signalConfig.useCid && !dfiConfig.signalConfig.useAckN && !dfiConfig.signalConfig.useBg),
    "DfiController supports DDR3-and-below buses (rasN/casN/weN/bank, no cid/act_n/bg) " +
      "and DDR4 buses (with act_n/bg/cid)"
  )
  // DDR4 needs the bank-group bits carved out of the flat bank index: bankWidth >= bgWidth + 1 (BA is non-empty).
  if (isDdr4) {
    assert(
      dfiConfig.sdram.bankWidth >= dfiConfig.sdram.bgWidth + 1 && dfiConfig.sdram.bgWidth >= 1,
      s"DDR4 requires bankWidth(${dfiConfig.sdram.bankWidth}) >= bgWidth(${dfiConfig.sdram.bgWidth}) + 1"
    )
    // A16/A15/A14 carry RA[15:13] on ACT; with rowWidth < 16 those pins are driven from bit 0 (V per spec).
    assert(
      dfiConfig.sdram.rowWidth >= 13,
      s"DDR4 requires rowWidth >= 13 (RA[15:13] for the ACT A16/A15/A14 pins); got ${dfiConfig.sdram.rowWidth}"
    )
  }
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
