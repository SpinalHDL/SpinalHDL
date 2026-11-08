package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._

case class Alignment(config: DfiConfig) extends Component {

  val io = new Bundle {
    val input = slave(IDFI(config))
    val output = master(Dfi(config))
    // NOTE: no ctrlmsg pass-through here. The ctrlmsg channel is a separate top-level port
    // of DfiController (io.dfiCtrlMsg, driven by CtrlMsgArbiter): the Dfi<>Dfi zip
    // (alignment.io.output <> io.dfi) neither double-drives nor silently drops it, and
    // Alignment stays ctrlmsg-agnostic (training does not touch command alignment).
  }

  val caAlignment = CAAlignment(config)
  caAlignment.io.cke.setAll()
  caAlignment.io.cmd <> io.input.cmd
  caAlignment.io.address <> io.input.address
  caAlignment.io.output <> io.output.control

  val wrAlignment = WrAlignment(config)
  wrAlignment.io.input <> io.input.wrData
  if (config.useWrdataCsN) wrAlignment.io.inputCs <> io.input.wrCs
  wrAlignment.io.output <> io.output.write

  val rdAlignment = RdAlignment(config)
  rdAlignment.io.input <> io.input.rdData
  if (config.useRddataCsN) rdAlignment.io.inputCs <> io.input.rdCs
  rdAlignment.io.inputEn <> io.input.rdEn
  rdAlignment.io.output <> io.output.read
  rdAlignment.io.phaseClear := False

  // DFI 5.2 Table 37: the MC must drive dfi_odt in all phases. Assert ODT for the chip-select
  // whose data window is active this phase (write or read), park (0) otherwise. Derived from
  // the actual DFI-side window signals (wrdata_en / wrdata_cs_n, rddata_valid / rddata_cs_n)
  // so the ODT waveform tracks the real data transfers. odt is phase-major
  // (odt(i*csNum + cs)), matching UsPhy's phaseSel take/drop layout. First-order model: the
  // 1-bit-per-CS bus only expresses on/off; receiver-side ODT tracking and MR-programmed RTT
  // encoding are a stage-2 refinement.
  if (config.useOdt) {
    for (i <- 0 until config.frequencyRatio) {
      val wrEn  = io.output.write.wr(i).wrdataEn
      val rdVal = io.output.read.rd(i).rddataValid
      val odtCs = Vec.tabulate(config.chipSelectNumber) { c =>
        val wrHit = if (config.useWrdataCsN) wrEn && !io.output.write.wr(i).wrdataCsN(c) else wrEn
        val rdHit = if (config.useRddataCsN) rdVal && !io.output.read.rdCs(i).rddataCsN(c) else rdVal
        wrHit || rdHit
      }
      caAlignment.io.odtEn(i) := odtCs.asBits
    }
  }

  // dfi_parity_in (DFI status): MC-side CA-parity-enabled indication. This controller does not
  // implement the PHY parity-reporting handshake, so it idles at 0 (DDR4 CA parity itself is
  // generated in CAAlignment on dfi_parity, not here).
  if (config.useParityIn) io.output.status.parityIn.clearAll()

  // DFI 5.2 management plane (status channel), minimal honest drive when useStatusSignals
  // is set (ADR-0016 v1): this controller has no internal initialization state, so
  // dfi_init_start is a single-cycle pulse at reset release (DFI Figure 6: the MC
  // de-asserts reset along with dfi_init_start); dfi_freq_ratio is static; the unused
  // low-power indications (data_byte_disable / dram_clk_disable) idle at 0.
  if (config.useInitStart) {
    val initStartPulse = RegInit(True)
    initStartPulse := False
    io.output.status.initStart := initStartPulse
  }
  // dfi_freq_ratio is the DFI 5.2 frequency RATIO CODE (sec. 3.5.1, sec. 3.5.3: 1:1=00,
  // 1:2=01, 1:4=10, 1:8=11), i.e. log2(ratio) -- not the ratio itself.
  if (config.useFreqRatio) io.output.status.freqRatio := U(log2Up(config.frequencyRatio), 2 bits).asBits
  if (config.useDataByteDisable) io.output.status.dataByteDisable.clearAll()
  if (config.useStatusSignals) io.output.status.dramClkDisable.clearAll()

  // DFI-level TRAINING INITIATION is out of scope for this controller: with useTrainingSignals
  // the bundle carries the MC-side training outputs, but the MC never initiates DFI-level
  // training here (boot training walks the ctrlmsg channel; legacy write leveling uses the
  // separate io.dfiWrLvl port). Idle every MC-driven training output at 0 so the master
  // interface is fully driven (same pattern as status.parityIn above). A future training
  // initiator replaces these ties. The PHY-side inputs (req/resp/csN) are driven by the PHY
  // or the testbench, not here.
  if (config.useRdlvlEn)       io.output.rdTraining.rdlvlEn.clearAll()
  if (config.useRdlvlGateEn)   io.output.rdTraining.rdlvlGateEn.clearAll()
  if (config.useWrlvlEn)       io.output.wrTraining.wrlvlEn.clearAll()
  if (config.useWrlvlStrobe)   io.output.wrTraining.wrlvlStrobe.clearAll()
  if (config.useCalvlEn)       io.output.caTraining.calvlEn.clearAll()
  if (config.useCalvlCapture)  io.output.caTraining.calvlCapture.clearAll()
  if (config.useLvlPattern)    io.output.levelingTraining.lvlPattern.clearAll()
  if (config.useLvlPeriodic)   io.output.levelingTraining.lvlPeriodic.clearAll()
  if (config.usePhylvlReqCsN)  io.output.phyRequestedTraining.phylvlReqCsN.setAll()

  // DFI 5.2 update channel (sec. 4.9): this controller implements no MC-initiated update
  // sequence (sec. 4.9.1) and never accepts a PHY-initiated update (sec. 4.9.2), so its two
  // MC-driven outputs idle at 0: ctrlupd_req is never asserted, phyupd_ack never
  // acknowledges. The PHY ties phyupd_req low for the matching reason, so no handshake is
  // started by either side and the wires are legal, driven, and observable.
  if (config.useCtrlupdReq) io.output.update.ctrlupdReq := False
  if (config.usePhyupdAck)  io.output.update.phyupdAck := False

  // DFI 5.2 low-power channel (sec. 3.6, sec. 4.13): DRAM power-down / self-refresh entry is
  // not implemented (no CKE path -- see the CKE tie in CAAlignment), so the MC never requests
  // a low-power state and lp_wake_up (the DRAM-side wake indication) idles at 0.
  if (config.useLpCtrlReq) io.output.lowPowerControl.lpCtrlReq := False
  if (config.useLpDataReq) io.output.lowPowerControl.lpDataReq := False
  if (config.useLpWakeUp)  io.output.lowPowerControl.lpWakeUp.clearAll()
}
