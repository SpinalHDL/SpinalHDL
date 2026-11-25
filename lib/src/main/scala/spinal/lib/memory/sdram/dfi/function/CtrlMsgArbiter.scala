package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._

/** DFI 6.0.1 Table 55 controller message codes. The channel itself (CtrlMsgArbiter) is
  * code-agnostic; only the two codes that have a driver today are wired to callers:
  * WRLVL and ZQCAL, both driven by the testbench through DfiController.io.trainReq.
  * The remaining entries are placeholders kept for Table 55 completeness — no caller,
  * no wiring, nothing invented (add the caller before wiring the code).
  */
object CtrlMsgCode {
  val CA_TRAIN = 0x00 // Command Bus Training
  val RD_EYE = 0x01 // Read Data Eye Training
  val RD_GATE = 0x02 // Read Gate Training
  val CS_TRAIN = 0x03 // CS Training
  val WRLVL = 0x04 // Write Leveling (wired: testbench-driven)
  val WR_EYE = 0x05 // Write Data Eye Training
  val DB_TRAIN = 0x06 // DB Bus Training
  val ZQCAL = 0x07 // ZQ Calibration (wired: testbench-driven)
  val DQS_OSC = 0x08 // DQS Interval Oscillator
}

/** Payload of one MC->PHY controller message (DFI 6.0.1 Table 47): dfi_ctrlmsg carries the
  * Table 55 message code, rank selects the target physical rank for dfi_ctrlmsg_data. */
case class CtrlMsgReq() extends Bundle {
  val msgCode = UInt(8 bits)
  val rank = UInt(4 bits)
}

/** MC-side arbiter for the optional DFI 6.0.1 ctrlmsg training channel (Table 12: all four
  * wires are Optional on both sides). One message at a time:
  *   - idle + io.trainReq fires -> latch msg/data, assert dfi_ctrlmsg_req
  *   - wait dfi_ctrlmsg_ack within tctrlmsg_resp; no ack in the window -> latch the sticky
  *     io.ctrlMsgError, revoke req and stop (record-and-expose, no retry — same policy as
  *     caParityError)
  *   - once acked, dfi_ctrlmsg_req stays asserted until ack de-asserts; ack held beyond
  *     tctrlmsg_max -> latch io.ctrlMsgError (record-and-expose; the handshake still
  *     completes on de-assert, no retry)
  *   - clean completion -> one-cycle io.trainDone pulse, back to idle
  * io.trainReq arriving while busy is ignored (no queueing — out of scope for this channel).
  * dfi_ctrlmsg_data follows Table 56: bit0=0 + bits[7:4]=physical rank for multi-rank
  * systems, bit0=1 (all physical ranks) in single-rank systems; bit1=1 (all logical ranks,
  * no 3DS in this controller). */
case class CtrlMsgArbiter(dfiConfig: DfiConfig) extends Component {
  val tResp = dfiConfig.timeConfig.tCtrlMsgResp
  val tMax = dfiConfig.timeConfig.tCtrlMsgMax
  assert(tResp >= 1, s"tctrlmsg_resp must be >= 1 (DFI 6.0.1 Table 48); got $tResp")
  assert(tMax >= 1, s"tctrlmsg_max must be >= 1 (DFI 6.0.1 Table 48); got $tMax")

  val io = new Bundle {
    val trainReq = slave(Flow(CtrlMsgReq()))
    val trainDone = out Bool ()
    val ctrlMsgError = out Bool ()
    val trainBusy = out Bool ()
    // MC->PHY / PHY->MC message wires (dfi_ctrlmsg / dfi_ctrlmsg_data / dfi_ctrlmsg_req / dfi_ctrlmsg_ack)
    val ctrlmsg = out Bits (8 bits)
    val ctrlmsgData = out Bits (16 bits)
    val ctrlmsgReq = out Bool ()
    val ctrlmsgAck = in Bool ()
  }

  def ctrlMsgData(rank: UInt): Bits = {
    // Generation-time split: single-rank systems must set bit0 (all physical ranks).
    if (dfiConfig.chipSelectNumber == 1) B(0x0003, 16 bits)
    else B(0x0002, 16 bits) | (rank.asBits << 4).resized
  }

  val busy = RegInit(False)
  val acked = RegInit(False)
  val error = RegInit(False)
  val msgReg = Reg(UInt(8 bits)) init (0)
  val dataReg = Reg(Bits(16 bits)) init (0)
  val respTimer = Reg(UInt(log2Up(tResp + 1) bits)) init (0)
  val maxTimer = Reg(UInt(log2Up(tMax + 1) bits)) init (0)

  val done = False
  io.trainDone := done
  io.ctrlMsgError := error
  io.trainBusy := busy
  io.ctrlmsg := msgReg.asBits
  io.ctrlmsgData := dataReg
  io.ctrlmsgReq := busy

  when(busy) {
    when(!acked) {
      when(io.ctrlmsgAck) {
        acked := True
        maxTimer := tMax
      } otherwise {
        when(respTimer === 0) {
          // No ack within tctrlmsg_resp: record the error, revoke req, stop. No retry.
          error := True
          busy := False
        } otherwise {
          respTimer := respTimer - 1
        }
      }
    } otherwise {
      when(!io.ctrlmsgAck) {
        busy := False
        acked := False
        done := True
      } otherwise {
        when(maxTimer === 0) {
          // Ack held beyond tctrlmsg_max: record the error and hold in place (req stays
          // asserted per protocol while ack is high; recovery is out of scope).
          error := True
        } otherwise {
          maxTimer := maxTimer - 1
        }
      }
    }
  } otherwise {
    when(io.trainReq.valid) {
      busy := True
      msgReg := io.trainReq.msgCode
      dataReg := ctrlMsgData(io.trainReq.rank)
      respTimer := tResp
    }
  }
}
