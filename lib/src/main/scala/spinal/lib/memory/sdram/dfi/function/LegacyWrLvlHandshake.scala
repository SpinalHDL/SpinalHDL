package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._

/** Legacy (DDR2/DDR3) write-leveling MC-side handshake: the Stream-style training handshake of
  * the non-ctrlmsg PHYs — io.wrLvlEn (dfi_wrlvl_en) is the MC->PHY wrLvlStart LEVEL, the
  * io.wrlvlResp lines (dfi_wrlvl_resp) carry the PHY->MC wrLvlDone (the MC watches the orR
  * rising edge). One handshake at a time:
  *   - idle + io.trigger fires (testbench, same pattern as trainReq) -> assert wrLvlEn (level
  *     held) and raise io.wrLvlBusy (the MakeTask spawn/refresh pause, same line the ctrlmsg
  *     arbiter uses)
  *   - wait for the wrLvlDone rising edge within tWrlvlResp -> drop wrLvlEn and wrLvlBusy,
  *     pulse io.done for one cycle
  *   - no edge in the window -> latch the sticky io.wrLvlError AND keep io.wrLvlBusy HIGH
  *     FOREVER: permanent MC halt, no retry (same caliber as the ctrlmsg timeout policy) —
  *     only the PHY request level is revoked (nothing left to wait for)
  * Semantic boundary: the MC fulfills the handshake protocol only (pause traffic -> start ->
  * wait done -> resume, with the watchdog). The delay-search convergence itself is PHY work
  * (dfi_wrlvl_req/strobe/dfi_phy_wrlvl_cs_n stay out of MC scope) — nothing here claims
  * "calibration passed"; done only means the PHY answered inside the window.
  *
  * Instantiated only when DfiSignalConfig.useLegacyWrLvl; every other config has no
  * component, no wires, no gates (pre-P3 elaborations unchanged). */
case class LegacyWrLvlHandshake(dfiConfig: DfiConfig) extends Component {
  val tResp = dfiConfig.timeConfig.tWrlvlResp
  assert(tResp >= 1, s"tWrlvlResp must be >= 1 (MC watchdog on dfi_wrlvl_resp); got $tResp")

  val io = new Bundle {
    val trigger = slave(Flow(Bool())) // testbench-driven, same pattern as trainReq
    val done = out Bool ()            // one-cycle pulse after the wrLvlDone rising edge
    val wrLvlError = out Bool ()      // sticky timeout latch (= the permanent-halt flag)
    val wrLvlBusy = out Bool ()       // OR-ed into the MakeTask pause line (trainBusy)
    // MC->PHY wrLvlStart (dfi_wrlvl_en): LEVEL held for the whole handshake window
    val wrLvlEn = out Bool ()
    // PHY->MC wrLvlDone source (dfi_wrlvl_resp): rising edge of orR = done
    val wrlvlResp = in Bits (dfiConfig.writeLevelingResponseWidth bits)
  }

  val busy = RegInit(False) // traffic gate: high for the window; FOREVER on timeout (halt)
  val error = RegInit(False) // sticky timeout record; latching it = permanent halt
  val respPrev = RegInit(False) // wrLvlDone edge detector
  val timer = Reg(UInt(log2Up(tResp + 1) bits)) init (0)

  val respLevel = io.wrlvlResp.orR
  val respDone = busy && respLevel && !respPrev

  io.wrLvlEn := busy && !error
  io.wrLvlBusy := busy
  io.wrLvlError := error
  io.done := False

  when(busy) {
    when(!error) {
      when(respDone) {
        busy := False
        io.done := True
      } otherwise {
        when(timer === 0) {
          // No wrLvlDone within tWrlvlResp: record the error and keep the traffic gate HIGH
          // forever (permanent MC halt — same no-retry policy as the ctrlmsg timeout).
          error := True
        } otherwise {
          timer := timer - 1
        }
      }
    }
  } otherwise {
    when(io.trigger.valid) {
      busy := True
      timer := tResp
    }
  }
  respPrev := respLevel
}
