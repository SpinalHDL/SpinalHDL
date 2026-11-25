package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._

/** Power-up training orchestrator (P2): once the init engine's initDone is seen, automatically
  * chain the two controller messages that have drivers, in fixed order ZQCAL -> WRLVL (driver
  * impedance calibration before write leveling), through the existing CtrlMsgArbiter trainReq
  * path, then pulse io.done (bootTrainDone) exactly once.
  *
  * Failure policy (fixed): any arbiter-detected timeout — the sticky ctrlMsgError latch, i.e.
  * no dfi_ctrlmsg_ack within tctrlmsg_resp or ack held beyond tctrlmsg_max — moves the FSM to
  * HALT and io.trainGate stays high FOREVER: the MC never resumes traffic (no retry, no
  * release) and io.done is never pulsed. The gate also covers the whole orchestration window
  * (from the first initDone cycle while enabled, through the done pulse) so no ACT/WR/RD can
  * slip between the two handshakes or before bootTrainDone.
  *
  * Instantiated only when signalConfig.useCtrlMsg && dfiConfig.bootTraining; every other
  * config has no component, no wires, no gates (pre-bootTraining elaborations unchanged).
  * The testbench enable (io.enable Flow) must fire before the FSM leaves IDLE: start =
  * enabled (latched) && initDone, in either arrival order.
  */
case class BootTrainer() extends Component {
  val io = new Bundle {
    val initDone = in Bool ()
    val enable = slave(Flow(Bool()))
    val req = master(Flow(CtrlMsgReq()))
    val arbDone = in Bool () // arbiter's one-cycle trainDone pulse (clean handshake)
    val arbError = in Bool () // arbiter's sticky ctrlMsgError (either timeout flavor)
    val trainGate = out Bool () // OR-ed into the arbiter trainBusy line (MakeTask pause)
    val done = out Bool () // one-shot bootTrainDone pulse; never fires on the HALT path
  }
  val enabled = RegInit(False)
  enabled.setWhen(io.enable.valid)

  // 0=idle 1=ZQCAL req 2=ZQCAL wait 3=WRLVL req 4=WRLVL wait 5=HALT 6=FIN (absorbing)
  val IDLE = 0; val ZQ_REQ = 1; val ZQ_WAIT = 2; val WL_REQ = 3; val WL_WAIT = 4; val HALT = 5; val FIN = 6
  val state = RegInit(U(0, 3 bits))

  io.req.valid := False
  io.req.msgCode := 0
  io.req.rank := 0
  io.done := False
  // Combinational on initDone: closes the one-cycle spawn gap between initDone rising and the
  // FSM's first registered transition (a queued BMB command would otherwise slip through).
  io.trainGate := enabled && (io.initDone || state =/= IDLE) && state =/= FIN

  switch(state) {
    is(IDLE) { when(enabled && io.initDone) { state := ZQ_REQ } }
    is(ZQ_REQ) { io.req.valid := True; io.req.msgCode := CtrlMsgCode.ZQCAL; state := ZQ_WAIT }
    is(ZQ_WAIT) {
      when(io.arbDone) { state := WL_REQ } elsewhen (io.arbError) { state := HALT }
    }
    is(WL_REQ) { io.req.valid := True; io.req.msgCode := CtrlMsgCode.WRLVL; state := WL_WAIT }
    is(WL_WAIT) {
      when(io.arbDone) { io.done := True; state := FIN } elsewhen (io.arbError) { state := HALT }
    }
    is(HALT) {} // permanent MC halt: trainGate high forever, done never pulses
    is(FIN) {} // inert forever after completion
  }
}
