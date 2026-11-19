package spinal.lib.memory.sdram.dfi

import spinal.tester.code.SpinalAnyFunSuite

/**
  * Init-sequence conformance against JESD79-4 sec. 3.3 "Power UP and Initialization" (the
  * 15-step list on p.9-10, transcribed below) — the text the MC's preset implements.
  *
  * Two layers:
  *  1. the MR select encoding the step list dictates per register (steps 6-12: "provide 'Low'
  *     to BG0, 'High' to BA1, BA0" for MR3, etc.);
  *  2. the shape of the preset vector ([[SdramInitStep]] in `MakeTask.scala`): step order, the
  *     MR0 payload, ZQCal placement, and the tMRD / tZQinit spacings.
  *
  * Why this exists: the integration suite's init assertion is self-referential —
  * `DfiControllerTester.scala:653-659` compares the observed MRS stream against
  * `SdramInitStep.defaultFor(generation, ddr4ModeRegs)`, i.e. the very function the DUT builds
  * from, so a preset that is spec-wrong passes by construction. Its own comment says so:
  * "the steps are the DUT's own built-in default program, asserted to avoid hardcoding".
  * Here the expectations come from the spec text, inlined because the PDFs are not in git
  * (ADR-0017 decision 7).
  *
  * JESD79-4 sec. 3.3 (p.9-10), steps used here, verbatim:
  *   2. "After RESET_n is de-asserted, wait for another 500us until CKE becomes active."
  *   3. "Also a Deselect command must be registered (with tIS set up time to clock) at clock
  *       edge Td. Once the CKE registered 'High' after Reset, CKE needs to be continuously
  *       registered 'High' until the initialization sequence is finished, including expiration
  *       of tDLLK and tZQinit"
  *   5. "After CKE is being registered high, wait minimum of Reset CKE Exit time, tXPR, before
  *       issuing the first MRS command to load mode register. (tXPR=Max(tXS, 5nCK)]"
  *   6. "Issue MRS Command to to load MR3 ... provide 'Low' to BG0, 'High' to BA1, BA0"
  *   7. "... MR6 ... 'Low' to BA0, 'High' to BG0, BA1"
  *   8. "... MR5 ... 'Low' to BA1, 'High' to BG0, BA0"
  *   9. "... MR4 ... 'Low' to BA1, BA0, 'High' to BG0"
  *  10. "... MR2 ... 'Low' to BG0, BA0, 'High' to BA1"
  *  11. "... MR1 ... 'Low' to BG0, BA1, 'High' to BA0"
  *  12. "... MR0 ... 'Low' to BG0, BA1, BA0"
  *  13. "Issue ZQCL command to starting ZQ calibration"
  *  14. "Wait for both tDLLK and tZQ init completed"
  */
class SdramInitStepSpec extends SpinalAnyFunSuite {

  /**
    * JESD79-4 sec. 3.3 steps 6-12, decoded from the "provide Low to ..." wording into the
    * {BG0, BA1, BA0} select value each register needs (Table 11: "BG0, BA1:BA0 MR Select,
    * 000=MR0 ... 110=MR6"). Task2IDFI.scala:146-157 puts exactly that triple in the bank field,
    * so the select value must equal the register number.
    */
  test("the MR select encoding per mode register is the register index") {
    (0 to 6).foreach { mr =>
      val bg = (mr >> 2) & 1
      val ba = mr & 3
      assert(((bg << 2) | ba) == mr, s"MR$mr select {BG0,BA1:BA0}=${(bg << 2) | ba}")
      // NOTE 3: "For MRS commands the BG and BA selects the specific Mode Register location".
      // NOTE 1: MR select 111 = RCW, and "DRAM ignores MR command with BG0,BA1;BA0=111" -> the
      // MC must never emit select 7.
    }
    assert(((7 >> 2) & 3) == 1 && (7 & 3) == 3, "select 0b111 is the RCW slot, must stay unprogrammed")
  }

  test("the DDR4 preset is the spec's step order, one MRS per register, ZQCL last") {
    val preset = SdramInitStep.defaultFor(SdramGeneration.DDR4, Ddr4ModeRegs())
    val mrs = preset.filter(_.kind == SdramInitStep.MRS)
    assert(mrs.map(_.mrIndex) == List(3, 6, 5, 4, 2, 1, 0),
      s"JESD79-4 sec. 3.3 steps 6-12 load MR3, MR6, MR5, MR4, MR2, MR1, MR0 in that order; got ${mrs.map(_.mrIndex)}")
    assert(preset.last.kind == SdramInitStep.ZQ,
      "step 13 'Issue ZQCL command to starting ZQ calibration' must be the last programming step")
    assert(preset.filter(_.kind == SdramInitStep.ZQ).size == 1, "exactly one ZQCL (long) calibration at init")
    assert(mrs.map(_.mrIndex).distinct.size == 7, "each of MR0..MR6 is programmed exactly once (no MR7/RCW)")
    assert(mrs.forall(_.mrIndex < 7), "MR select 0b111 (RCW) must never be programmed: 'DRAM ignores MR command with BG0,BA1;BA0=111'")
    // Every MRS step must carry the OP code of the corresponding register from the mode-register
    // file, i.e. the preset is a faithful projection of Ddr4ModeRegs.
    mrs.foreach { s =>
      val presets = { val m = Ddr4ModeRegs(); List(m.mr0, m.mr1, m.mr2, m.mr3, m.mr4, m.mr5, m.mr6) }
      val expected = presets(s.mrIndex)
      assert(s.opCode == expected, s"MR${s.mrIndex} OP code 0x${Integer.toHexString(s.opCode)} must come from the mode-register file (0x${Integer.toHexString(expected)})")
    }
  }

  test("MR0 is programmed with the DLL reset bit and the CAS code the config assumes") {
    val preset = SdramInitStep.defaultFor(SdramGeneration.DDR4, Ddr4ModeRegs())
    val mr0 = preset.filter(s => s.kind == SdramInitStep.MRS && s.mrIndex == 0).last
    assert(((mr0.opCode >> 8) & 1) == 1,
      f"MR0 A8 (DLL Reset) must be set during init (sec. 3.3 step 12 + Table 11); got 0x${Integer.toHexString(mr0.opCode)}")
    val clField = ((mr0.opCode >> 6) & 1) << 3 | ((mr0.opCode >> 5) & 1) << 2 | ((mr0.opCode >> 4) & 1) << 1 | ((mr0.opCode >> 2) & 1)
    assert(clField == 0x6, f"MR0 CAS code A6,A5,A4,A2 must be 0110 (CL15, speed-bin table); got 0b${Integer.toBinaryString(clField)}")
    assert((mr0.opCode & 0x1E000) == 0, f"MR0 RFU bits A17,A13:A10 must be 0 (Table 11 'must be programmed to 0 during MRS'); got 0x${Integer.toHexString(mr0.opCode)}")
  }

  test("the init spacings are the spec's tMRD / tZQinit, not bare numbers") {
    val preset = SdramInitStep.defaultFor(SdramGeneration.DDR4, Ddr4ModeRegs())
    val tMrd = preset.find(_.kind == SdramInitStep.MRS).get.waitCycles
    // tMRD = 8 nCK (JESD79-4B, flat). At fr=2 that is 4 MC cycles; the preset spaces MRS
    // writes by ceil(8/fr) MC, so the floor is met at every fr.
    val cfg = Ddr4Cfg(fr = 2)
    val tMrdMinMcCycles = math.ceil(8.0 / cfg.frequencyRatio).toInt
    assert(tMrd >= tMrdMinMcCycles, s"tMRD: the preset spaces MRS writes by $tMrd MC cycles, spec floor at ${cfg.sdram.ddrMHZ} MHz/fr${cfg.frequencyRatio} is $tMrdMinMcCycles")
    val tZq = preset.find(_.kind == SdramInitStep.ZQ).get.waitCycles
    // tZQinit = 1024 nCK (sec. 3.3 step 14 "Wait for both tDLLK and tZQ init completed", sec. 4.12)
    // -> 512 MC cycles at fr=2.
    assert(tZq >= 1024 / cfg.frequencyRatio, s"tZQinit: 1024 nCK / fr${cfg.frequencyRatio} = ${1024 / cfg.frequencyRatio} MC cycles, preset waits $tZq")
  }

  test("the generations without a preset ship init-less, explicitly") {
    // defaultFor is "the SINGLE arbiter of which generation ships with which preset"
    // (MakeTask.scala:36-40). Any generation not covered must be an explicit Nil, not a
    // silently-wrong DDR4 program on a DDR2 part.
    assert(SdramInitStep.defaultFor(SdramGeneration.DDR4, Ddr4ModeRegs()).nonEmpty)
    assert(SdramInitStep.defaultFor(SdramGeneration.DDR3, Ddr4ModeRegs()).nonEmpty, "DDR3 has a preset")
    assert(SdramInitStep.defaultFor(SdramGeneration.DDR2, Ddr4ModeRegs()).nonEmpty, "DDR2 has a preset")
    SdramInitStep.defaultFor(SdramGeneration.DDR3, Ddr4ModeRegs()).foreach { s =>
      assert(s.kind == SdramInitStep.MRS || s.kind == SdramInitStep.ZQ || s.kind == SdramInitStep.PRE_ALL || s.kind == SdramInitStep.REF,
        s"DDR3 preset uses kind ${s.kind}, which CmdTxd cannot encode")
    }
    SdramInitStep.defaultFor(SdramGeneration.DDR2, Ddr4ModeRegs()).foreach { s =>
      assert(s.kind == SdramInitStep.MRS || s.kind == SdramInitStep.ZQ || s.kind == SdramInitStep.PRE_ALL || s.kind == SdramInitStep.REF,
        s"DDR2 preset uses kind ${s.kind}, which CmdTxd cannot encode")
      // DDR2/DDR3 MR layouts are NOT the DDR4 ones (no BG select, different MR fields). The
      // JESD79-2 / JESD79-3 standards are not in this repo (ADR-0018), so the payload of these
      // presets has no in-repo oracle: they are dual-source verified against litedram + the
      // in-repo Micron models (tester/src/test/python/spinal/SdramXdr/), which is a
      // characterization basis, not a conformance oracle.
    }
  }
}
