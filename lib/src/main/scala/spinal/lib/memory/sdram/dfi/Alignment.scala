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

  // dfi_parity_in (DFI status): MC-side CA-parity-enabled indication. This controller does not
  // implement the PHY parity-reporting handshake, so it idles at 0 (DDR4 CA parity itself is
  // generated in CAAlignment on dfi_parity, not here).
  if (config.useParityIn) io.output.status.parityIn.clearAll()

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
}
