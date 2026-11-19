package spinal.lib.memory.sdram.dfi

import spinal.tester.code.SpinalAnyFunSuite

/**
  * Golden unit test for [[DfiCmdDecoder]] — the independent Table-16 decode.
  *
  * Sim-free: feeds the decoder every legal opcode (plus the non-emittable ones) and asserts the
  * result, pinning the decoder's correctness against the hand-transcribed JESD79-4 Table 16
  * independently of both the DUT and [[DfiMemoryAgent]]. If the decoder itself drifts from the
  * spec, this suite fails before it can poison the live-traffic cross-check in
  * [[DfiControllerTester]].
  *
  * Expected values transcribed from the same golden table as [[SdramCmdTruthTable16Spec]]
  * (Pin(actN, ras, cas, we) + a10):
  *   MRS  (1,0,0,0)   REF  (1,0,0,1)   PRE/PREA (1,0,1,0) a10 L/H
  *   ACT  actN=0      WR   (1,1,0,0)   RD   (1,1,0,1)
  *   ZQCL (1,1,1,0) a10=H  NOP  (1,1,1,1)
  *   (1,1,1,0) a10=L = ZQCS, (1,0,1,1) = RFU/SRE/SRX  -> ILLEGAL (MC never emits)
  */
class DfiCmdDecoderSpec extends SpinalAnyFunSuite {
  import DfiCmdDecoder.Kind

  test("DfiCmdDecoder: legal Table-16 opcodes decode to the right command") {
    // Bank activate: ACT_n LOW, RAS/CAS/WE become address pins (don't care).
    assert(DfiCmdDecoder.decode(0, 1, 1, 1, 0) == Kind.ACT)
    assert(DfiCmdDecoder.decode(0, 0, 0, 0, 0) == Kind.ACT)
    assert(DfiCmdDecoder.decode(0, 1, 0, 1, 1) == Kind.ACT)

    // ACT_n HIGH: RAS/CAS/WE are command pins.
    assert(DfiCmdDecoder.decode(1, 1, 0, 0, 0) == Kind.WR)   // write
    assert(DfiCmdDecoder.decode(1, 1, 0, 0, 1) == Kind.WR)   // a10 don't-care for WR
    assert(DfiCmdDecoder.decode(1, 1, 0, 1, 0) == Kind.RD)   // read
    assert(DfiCmdDecoder.decode(1, 1, 0, 1, 1) == Kind.RD)
    assert(DfiCmdDecoder.decode(1, 0, 1, 0, 0) == Kind.PRE)  // precharge one bank
    assert(DfiCmdDecoder.decode(1, 0, 1, 0, 1) == Kind.PRE)  // precharge all (PREA, same label)
    assert(DfiCmdDecoder.decode(1, 0, 0, 1, 0) == Kind.REF)  // refresh
    assert(DfiCmdDecoder.decode(1, 0, 0, 0, 0) == Kind.MRS)  // mode register set
    assert(DfiCmdDecoder.decode(1, 1, 1, 0, 1) == Kind.ZQCL) // ZQ calibration long
    assert(DfiCmdDecoder.decode(1, 1, 1, 1, 0) == Kind.NOP)  // no operation
  }

  test("DfiCmdDecoder: non-emittable opcodes are flagged ILLEGAL, not silently dropped") {
    // ZQCS (short): same pins as ZQCL but A10=L — the MC never emits it (golden mcOp=None).
    assert(DfiCmdDecoder.decode(1, 1, 1, 0, 0) == Kind.ILLEGAL)
    // RFU / SRE / SRX: (1,0,1,1) is a CKE-transition command the MC never drives.
    assert(DfiCmdDecoder.decode(1, 0, 1, 1, 0) == Kind.ILLEGAL)
    assert(DfiCmdDecoder.decode(1, 0, 1, 1, 1) == Kind.ILLEGAL)
  }

  test("DfiCmdDecoder: kind labels match the agent's [AGENTCMD] spelling (cross-check contract)") {
    // The live-traffic cross-check compares these labels to DfiMemoryAgent's decodedCmdLog, so
    // they must spell the commands exactly as the agent prints them.
    assert(DfiCmdDecoder.Kind.labels(Kind.ACT) == "ACT")
    assert(DfiCmdDecoder.Kind.labels(Kind.WR) == "WR")
    assert(DfiCmdDecoder.Kind.labels(Kind.RD) == "RD")
    assert(DfiCmdDecoder.Kind.labels(Kind.PRE) == "PRE")
    assert(DfiCmdDecoder.Kind.labels(Kind.REF) == "REF")
    assert(DfiCmdDecoder.Kind.labels(Kind.MRS) == "MRS")
    assert(DfiCmdDecoder.Kind.labels(Kind.ZQCL) == "ZQCL")
  }

  test("DfiCmdDecoder.decodeLegacy: legal legacy (DDR2/DDR3) opcodes decode correctly") {
    // Micron ddr3.v / ddr2.v model encoding (RAS_n, CAS_n, WE_n), 1 = pin HIGH, chip CS-selected.
    assert(DfiCmdDecoder.decodeLegacy(0, 1, 1, 0) == Kind.ACT)   // ACTIVATE
    assert(DfiCmdDecoder.decodeLegacy(1, 0, 0, 0) == Kind.WR)    // WRITE
    assert(DfiCmdDecoder.decodeLegacy(1, 0, 1, 0) == Kind.RD)    // READ
    assert(DfiCmdDecoder.decodeLegacy(0, 1, 0, 0) == Kind.PRE)   // PRECHARGE one bank (A10=L)
    assert(DfiCmdDecoder.decodeLegacy(0, 1, 0, 1) == Kind.PRE)   // PRECHARGE all (PREA, A10=H)
    assert(DfiCmdDecoder.decodeLegacy(0, 0, 1, 0) == Kind.REF)   // REFRESH
    assert(DfiCmdDecoder.decodeLegacy(0, 0, 0, 0) == Kind.MRS)   // LOAD_MODE / EMRS
    assert(DfiCmdDecoder.decodeLegacy(1, 1, 0, 1) == Kind.ZQCL)  // ZQ cal long (A10=H, DDR3)
    assert(DfiCmdDecoder.decodeLegacy(1, 1, 1, 0) == Kind.NOP)   // NOP / DESELECT pattern
  }

  test("DfiCmdDecoder.decodeLegacy: non-emittable legacy patterns are flagged ILLEGAL") {
    // (1,1,0) with A10=L is not a command the MC emits (ZQCL requires A10=H).
    assert(DfiCmdDecoder.decodeLegacy(1, 1, 0, 0) == Kind.ILLEGAL)
    // (0,1,1) is ACT already; a few other combos the MC never drives on an active CS.
    assert(DfiCmdDecoder.decodeLegacy(1, 1, 1, 1) == Kind.NOP)
    assert(DfiCmdDecoder.decodeLegacy(1, 0, 0, 1) == Kind.WR)    // A10 don't-care for WRITE
  }
}
