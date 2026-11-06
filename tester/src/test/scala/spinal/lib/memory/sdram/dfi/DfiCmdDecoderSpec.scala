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
    assertEquals(DfiCmdDecoder.decode(0, 1, 1, 1, 0), Kind.ACT)
    assertEquals(DfiCmdDecoder.decode(0, 0, 0, 0, 0), Kind.ACT)
    assertEquals(DfiCmdDecoder.decode(0, 1, 0, 1, 1), Kind.ACT)

    // ACT_n HIGH: RAS/CAS/WE are command pins.
    assertEquals(DfiCmdDecoder.decode(1, 1, 0, 0, 0), Kind.WR)   // write
    assertEquals(DfiCmdDecoder.decode(1, 1, 0, 0, 1), Kind.WR)   // a10 don't-care for WR
    assertEquals(DfiCmdDecoder.decode(1, 1, 0, 1, 0), Kind.RD)   // read
    assertEquals(DfiCmdDecoder.decode(1, 1, 0, 1, 1), Kind.RD)
    assertEquals(DfiCmdDecoder.decode(1, 0, 1, 0, 0), Kind.PRE)  // precharge one bank
    assertEquals(DfiCmdDecoder.decode(1, 0, 1, 0, 1), Kind.PRE)  // precharge all (PREA, same label)
    assertEquals(DfiCmdDecoder.decode(1, 0, 0, 1, 0), Kind.REF)  // refresh
    assertEquals(DfiCmdDecoder.decode(1, 0, 0, 0, 0), Kind.MRS)  // mode register set
    assertEquals(DfiCmdDecoder.decode(1, 1, 1, 0, 1), Kind.ZQCL) // ZQ calibration long
    assertEquals(DfiCmdDecoder.decode(1, 1, 1, 1, 0), Kind.NOP)  // no operation
  }

  test("DfiCmdDecoder: non-emittable opcodes are flagged ILLEGAL, not silently dropped") {
    // ZQCS (short): same pins as ZQCL but A10=L — the MC never emits it (golden mcOp=None).
    assertEquals(DfiCmdDecoder.decode(1, 1, 1, 0, 0), Kind.ILLEGAL)
    // RFU / SRE / SRX: (1,0,1,1) is a CKE-transition command the MC never drives.
    assertEquals(DfiCmdDecoder.decode(1, 0, 1, 1, 0), Kind.ILLEGAL)
    assertEquals(DfiCmdDecoder.decode(1, 0, 1, 1, 1), Kind.ILLEGAL)
  }

  test("DfiCmdDecoder: kind labels match the agent's [AGENTCMD] spelling (cross-check contract)") {
    // The live-traffic cross-check compares these labels to DfiMemoryAgent's decodedCmdLog, so
    // they must spell the commands exactly as the agent prints them.
    assertEquals(DfiCmdDecoder.Kind.labels(Kind.ACT), "ACT")
    assertEquals(DfiCmdDecoder.Kind.labels(Kind.WR), "WR")
    assertEquals(DfiCmdDecoder.Kind.labels(Kind.RD), "RD")
    assertEquals(DfiCmdDecoder.Kind.labels(Kind.PRE), "PRE")
    assertEquals(DfiCmdDecoder.Kind.labels(Kind.REF), "REF")
    assertEquals(DfiCmdDecoder.Kind.labels(Kind.MRS), "MRS")
    assertEquals(DfiCmdDecoder.Kind.labels(Kind.ZQCL), "ZQCL")
  }
}
