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
