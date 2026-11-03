package spinal.lib.memory.sdram.dfi

import spinal.tester.code.SpinalAnyFunSuite

/** Pins the BUILT-IN init presets (SdramInitStep.defaultFor) as protocol facts.
  *
  * These lists are dual-source verified (see Ddr3InitSequencer/Ddr2InitSequencer docs in
  * MakeTask.scala): litedram master litedram/init.py (step order + MR formulas) and the
  * in-repo Micron ddr3.v/ddr2.v models (legacy command encodings). This spec guards against
  * silent drift of the shipped defaults WITHOUT running the full simulation suite — the
  * DfiControllerTester "DfiController_DDR3/DDR2_fr2_init" tests separately prove the presets
  * work end-to-end through the engine + DfiMemoryAgent reference model. */
class SdramInitPresetSpec extends SpinalAnyFunSuite {

  test("defaultFor(DDR3) = litedram-verified MRS MR2/MR3/MR1/MR0(DLL reset) + ZQCL") {
    val steps = SdramInitStep.defaultFor(SdramGeneration.DDR3, Ddr4ModeRegs())
    assert(steps == List(
      SdramInitStep(SdramInitStep.MRS, 2, 0x200, 8), // CWL=5, RTT_WR=60 Ohm
      SdramInitStep(SdramInitStep.MRS, 3, 0x000, 8), // DLL enabled
      SdramInitStep(SdramInitStep.MRS, 1, 0x006, 8), // RON=34 Ohm, RTT_NOM=60 Ohm
      SdramInitStep(SdramInitStep.MRS, 0, 0x310, 8), // BL8, CL5, WR5, DLL reset
      SdramInitStep(SdramInitStep.ZQ, 0, 0, 512)
    ))
  }

  test("defaultFor(DDR2) = litedram-verified PRE/EMRS/MRS(DLL reset)/PRE/REF x2/MRS/OCD dance, no ZQ") {
    val steps = SdramInitStep.defaultFor(SdramGeneration.DDR2, Ddr4ModeRegs())
    assert(steps == List(
      SdramInitStep(SdramInitStep.PRE_ALL, 0, 0, 8),
      SdramInitStep(SdramInitStep.MRS, 3, 0x000, 8), // EMRS3
      SdramInitStep(SdramInitStep.MRS, 2, 0x000, 8), // EMRS2
      SdramInitStep(SdramInitStep.MRS, 1, 0x000, 8), // EMRS1
      SdramInitStep(SdramInitStep.MRS, 0, 0x552, 256), // MRS MR0 + DLL reset (BL4, CL5, WR5)
      SdramInitStep(SdramInitStep.PRE_ALL, 0, 0, 8),
      SdramInitStep(SdramInitStep.REF, 0, 0, 8),
      SdramInitStep(SdramInitStep.REF, 0, 0, 8),
      SdramInitStep(SdramInitStep.MRS, 0, 0x452, 8), // MRS MR0 (no DLL reset)
      SdramInitStep(SdramInitStep.MRS, 1, 0x380, 8), // EMRS1 OCD default (7<<7)
      SdramInitStep(SdramInitStep.MRS, 1, 0x000, 8) // EMRS1 OCD exit
    ))
    assert(!steps.exists(_.kind == SdramInitStep.ZQ), "DDR2 has no ZQCL step")
  }

  test("defaultFor(DDR4) honors the caller's mode registers (MR3/6/5/4/2/1/0 filed, then ZQCL)") {
    val regs = Ddr4ModeRegs(mr0 = 0x111, mr1 = 0x222, mr2 = 0x333, mr3 = 0x444, mr4 = 0x555, mr5 = 0x666, mr6 = 0x777)
    val ops = List(regs.mr0, regs.mr1, regs.mr2, regs.mr3, regs.mr4, regs.mr5, regs.mr6)
    val steps = SdramInitStep.defaultFor(SdramGeneration.DDR4, regs)
    val expected =
      List(3, 6, 5, 4, 2, 1, 0).map(mr => SdramInitStep(SdramInitStep.MRS, mr, ops(mr), 4)) ++
        List(SdramInitStep(SdramInitStep.ZQ, 0, 0, 512))
    assert(steps == expected)
  }

  test("defaultFor(SDR/MYDDR) = Nil (init-less; explicit DfiConfig.initSteps remains the escape hatch)") {
    assert(SdramInitStep.defaultFor(SdramGeneration.SDR, Ddr4ModeRegs()).isEmpty)
    assert(SdramInitStep.defaultFor(SdramGeneration.MYDDR, Ddr4ModeRegs()).isEmpty)
  }
}
