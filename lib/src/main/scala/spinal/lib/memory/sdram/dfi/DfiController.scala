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
    // DFI 6.0.1 ctrlmsg training channel (Table 12): the upper layer / testbench drives one
    // message (CtrlMsgCode, e.g. WRLVL/ZQCAL) and observes completion / the sticky error.
    // The ports always exist (stable API); with useCtrlMsg off they are inert (trainDone
    // stuck low, ctrlMsgError stuck low, trainReq dangling-legal).
    // The train ports exist on the Scala API for every config (stable API), but the
    // hardware ports are gated: a top-level port present in legacy configs shifts the
    // simulator's t0 randomization stream (randBoot registers), which re-rolls the
    // BmbRegionAllocator address stream and flips which rank the init MRS lands on —
    // breaking the old-23-tests verbatim-log guarantee. All testbench references are
    // already conditional on useCtrlMsg, so a null port is never dereferenced.
    val trainReq = dfiConfig.signalConfig.useCtrlMsg generate slave(Flow(CtrlMsgReq()))
    val trainDone = dfiConfig.signalConfig.useCtrlMsg generate out(Bool())
    val ctrlMsgError = dfiConfig.signalConfig.useCtrlMsg generate out(Bool())
    // Separate top-level ctrlmsg port (NOT a Dfi member): keeps the Dfi<>Dfi zip in
    // alignment.io.output <> io.dfi away from the arbiter-driven wires entirely. Gated by
    // useCtrlMsg (DFI Table 12: all four wires Optional — absent entirely when off).
    val dfiCtrlMsg = dfiConfig.signalConfig.useCtrlMsg generate master(DfiCtrlMsgInterface(dfiConfig))
    // P2 boot-training orchestrator ports (present only when useCtrlMsg && bootTraining):
    // bootTrainEnable = testbench enable (the orchestrator leaves IDLE only after it fires,
    // guaranteeing a controllable start), bootTrainDone = one-shot completion pulse. Failure
    // is observed on the existing io.ctrlMsgError (sticky) with the MC permanently halted.
    val bootTrainEnable = (dfiConfig.signalConfig.useCtrlMsg && dfiConfig.bootTraining) generate slave(Flow(Bool()))
    val bootTrainDone = (dfiConfig.signalConfig.useCtrlMsg && dfiConfig.bootTraining) generate out(Bool())
    // P3 legacy (DDR2/DDR3) write-leveling Stream handshake ports (present only when
    // useLegacyWrLvl; the useCtrlMsg-style gating keeps legacy-config elaborations and t0
    // randomization streams untouched): testbench trigger, one-shot done pulse, sticky
    // timeout error, and the separate two-wire dfi_wrlvl port (see DfiLegacyWrLvlInterface
    // for why it is NOT a Dfi bundle member).
    val legacyWrLvlTrigger = dfiConfig.signalConfig.useLegacyWrLvl generate slave(Flow(Bool()))
    val legacyWrLvlDone = dfiConfig.signalConfig.useLegacyWrLvl generate out(Bool())
    val legacyWrLvlError = dfiConfig.signalConfig.useLegacyWrLvl generate out(Bool())
    val dfiWrLvl = dfiConfig.signalConfig.useLegacyWrLvl generate master(DfiLegacyWrLvlInterface(dfiConfig))
  }

  val taskConfig = BmbAdapter.taskConfig(bmbp, dfiConfig, task)

  // The ctrlmsg channel (DDR4) and the legacy write-leveling handshake (DDR2/DDR3) are
  // alternative training transports; wiring both would need a trainBusy arbiter that does
  // not exist.
  assert(
    !(dfiConfig.signalConfig.useCtrlMsg && dfiConfig.signalConfig.useLegacyWrLvl),
    "useLegacyWrLvl is the DDR2/DDR3 training transport; it must not be combined with useCtrlMsg"
  )

  val bmbBridge = BmbBridge(bmbp, taskConfig, dfiConfig, addrMap)
  bmbBridge.io.bmb <> io.bmb

  val control = Control(taskConfig, dfiConfig)
  control.io.input <> bmbBridge.io.taskPort

  val alignment = Alignment(dfiConfig)
  alignment.io.input <> control.io.output
  alignment.io.output <> io.dfi

  // DFI 6.0.1 ctrlmsg training channel (Table 12, all four wires Optional on both sides):
  // the arbiter drives dfi_ctrlmsg/dfi_ctrlmsg_data/dfi_ctrlmsg_req and watches
  // dfi_ctrlmsg_ack; while a handshake is in flight its trainBusy output pauses MakeTask
  // task-spawn/refresh globally. Instantiated only when useCtrlMsg is set — disabled
  // configs elaborate with none of it (no wires, no arbiter, no gates). The arbiter drives
  // io.dfiCtrlMsg (the separate top-level port, see io bundle): assignable from this
  // component, and immune to the Dfi<>Dfi zip in `alignment.io.output <> io.dfi` above
  // (which under a Dfi-bundle member produced ASSIGNMENT OVERLAP or silent NO DRIVER).
  val ctrlMsgArb = if (dfiConfig.signalConfig.useCtrlMsg) {
    val arb = CtrlMsgArbiter(dfiConfig)
    // Boot-training configs inject through the same trainReq path (member-wise assignment;
    // the requester mux is built in the bootTrainer block below), every other config keeps
    // the direct testbench connection unchanged.
    if (!dfiConfig.bootTraining) arb.io.trainReq << io.trainReq
    io.dfiCtrlMsg.ctrlmsg := arb.io.ctrlmsg
    io.dfiCtrlMsg.ctrlmsgData := arb.io.ctrlmsgData
    io.dfiCtrlMsg.ctrlmsgReq := arb.io.ctrlmsgReq
    arb.io.ctrlmsgAck := io.dfiCtrlMsg.ctrlmsgAck
    // With boot training the OR with the orchestrator's trainGate is assigned in the
    // bootTrainer block below (a second full assignment here would be an overlap error).
    if (!dfiConfig.bootTraining) bmbBridge.io.trainBusy := arb.io.trainBusy
    arb
  } else null
  if (ctrlMsgArb != null) {
    io.trainDone := ctrlMsgArb.io.trainDone
    io.ctrlMsgError := ctrlMsgArb.io.ctrlMsgError
  }

  // P2 boot training orchestrator: after the init engine's initDone, chains ZQCAL -> WRLVL
  // (fixed order) through the arbiter's trainReq path and pulses io.bootTrainDone once.
  // The orchestrator's trainGate ORs into the MakeTask pause so traffic stays held across the
  // whole orchestration window and, on any arbiter timeout (sticky ctrlMsgError), FOREVER —
  // permanent MC halt, no retry. Present only when useCtrlMsg && bootTraining; every other
  // config leaves the arbiter wiring (and everything else) byte-identical to before.
  val bootTrainer = if (dfiConfig.signalConfig.useCtrlMsg && dfiConfig.bootTraining) {
    val trainer = BootTrainer()
    trainer.io.initDone := bmbBridge.io.initDone
    trainer.io.enable << io.bootTrainEnable
    trainer.io.arbDone := ctrlMsgArb.io.trainDone
    trainer.io.arbError := ctrlMsgArb.io.ctrlMsgError
    ctrlMsgArb.io.trainReq.valid := io.trainReq.valid || trainer.io.req.valid
    ctrlMsgArb.io.trainReq.msgCode := trainer.io.req.valid ? trainer.io.req.msgCode | io.trainReq.msgCode
    ctrlMsgArb.io.trainReq.rank := trainer.io.req.valid ? trainer.io.req.rank | io.trainReq.rank
    bmbBridge.io.trainBusy := ctrlMsgArb.io.trainBusy || trainer.io.trainGate
    io.bootTrainDone := trainer.io.done
    trainer
  } else null

  // P3 legacy write-leveling Stream handshake (MC side): the component raises dfi_wrlvl_en
  // (wrLvlStart level), pauses MakeTask spawn/refresh through the same trainBusy line the
  // ctrlmsg arbiter uses, waits for the dfi_wrlvl_resp rising edge and pulses done once.
  // On the tWrlvlResp watchdog expiry it latches the sticky legacyWrLvlError and halts the
  // MC permanently (wrLvlBusy never drops — no retry, same caliber as the ctrlmsg timeout).
  // Present only when useLegacyWrLvl; every other config leaves the ctrlmsg/bootTraining
  // wiring (and everything else) byte-identical to before.
  val legacyWrLvl = if (dfiConfig.signalConfig.useLegacyWrLvl) {
    val wrLvlHs = LegacyWrLvlHandshake(dfiConfig)
    wrLvlHs.io.trigger << io.legacyWrLvlTrigger
    wrLvlHs.io.wrlvlResp := io.dfiWrLvl.wrlvlResp
    io.dfiWrLvl.wrLvlEn := wrLvlHs.io.wrLvlEn
    bmbBridge.io.trainBusy := wrLvlHs.io.wrLvlBusy
    io.legacyWrLvlDone := wrLvlHs.io.done
    io.legacyWrLvlError := wrLvlHs.io.wrLvlError
    wrLvlHs
  } else null

  // DDR4 CA parity (JESD79-4B §4.17): on a command-bus parity error the DRAM ignores the command
  // and pulls ALERT_n low within tPAR_ALERT_ON. The MC records the event in a sticky register
  // (record-and-expose only; no recovery protocol). The raw dfi_alert_n stays observable at the
  // DFI boundary for the testbench.
  val caParityError = if (dfiConfig.useCaParity) {
    val seen = RegInit(False)
    seen.setWhen(!io.dfi.status.alertN.andR) // alert_n is low-active: any low bit = alert
    seen
  } else null
}
