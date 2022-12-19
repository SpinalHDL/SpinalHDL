package spinal.lib.blackbox.lattice.ecp5

import spinal.core._

object EHXPLLLConfig {

  trait FeedbackType {
    override def toString: String = this.getClass.getSimpleName.dropRight(1)
  }
  // Uses clock but expects this to be fed back in through CLKFB at the selected frequency
  object CLKOP extends FeedbackType {}
  object CLKOS extends FeedbackType {}
  object CLKOS2 extends FeedbackType {}
  object CLKOS3 extends FeedbackType {}
  // Uses internal connections from the selected clock
  object INT_CLKOP extends FeedbackType {}
  object INT_CLKOS extends FeedbackType {}
  object INT_CLKOS2 extends FeedbackType {}
  object INT_CLKOS3 extends FeedbackType {}
  object USERCLOCK extends FeedbackType {}

  private def VCO_MAX: HertzNumber = 800 MHz
  private def VCO_MIN: HertzNumber = 400 MHz

  private def CLK_IN_MIN: HertzNumber = 8 MHz
  private def CLK_IN_MAX: HertzNumber = 400 MHz

  private def CLK_OUT_MIN: HertzNumber = 3.125 MHz
  private def CLK_OUT_MAX: HertzNumber = 400 MHz


  /**
    * Calculates the dividers required for the CLKOP path
    * @param inFreq CLKI frequency
    * @param outFreq CLKOP desired frequency
    * @param tolerance Tolerance for error in CLKOP
    * @return EHXPLLL configuration
    */
  def singleOutput(inFreq: HertzNumber, outFreq: HertzNumber, tolerance: BigDecimal = 0.02): EHXPLLLConfig = {
    def calculateError(clki: BigDecimal, clko: BigDecimal, factor: BigDecimal): BigDecimal = {
      (clko-(clki*factor))/clko
    }

    var mDiv: BigDecimal = 1
    var fbDiv: BigDecimal = 1
    var err: BigDecimal = calculateError(inFreq.toBigDecimal, outFreq.toBigDecimal, fbDiv/mDiv)
    while(mDiv < 128 && fbDiv < 128 &&
          (err.abs > tolerance || (inFreq.toBigDecimal*fbDiv)/mDiv <= VCO_MIN.toBigDecimal) &&
          (inFreq.toBigDecimal*fbDiv)/mDiv <= VCO_MAX.toBigDecimal) {
      err = calculateError(inFreq.toBigDecimal, outFreq.toBigDecimal, fbDiv/mDiv)
      if (err < 0) {
        mDiv += 1
      } else if (err > 0) {
        fbDiv += 1
      }
    }

    var opDiv: BigDecimal = 1
    while(opDiv < 128 && (inFreq.toBigDecimal*opDiv*fbDiv)/mDiv < VCO_MAX.toBigDecimal) {
      opDiv += 1
    }
    if (opDiv > 1 && (inFreq.toBigDecimal*opDiv*fbDiv)/mDiv > VCO_MAX.toBigDecimal)
      opDiv -= 1

    EHXPLLLConfig(
      clkiFreq = inFreq,
      mDiv = mDiv.setScale(0, BigDecimal.RoundingMode.FLOOR).toInt,
      fbDiv = fbDiv.setScale(0, BigDecimal.RoundingMode.FLOOR).toInt,
      opDiv = opDiv.setScale(0, BigDecimal.RoundingMode.FLOOR).toInt,
      fbPath = EHXPLLLConfig.INT_CLKOP
    )
  }
}

case class EHXPLLLConfig(clkiFreq: HertzNumber,
                         mDiv: Int = 1,
                         fbDiv: Int = 1,
                         opDiv: Int = 1,
                         osDiv: Int = -1,
                         os2Div: Int = -1,
                         os3Div: Int = -1,
                         clkfbFreq: HertzNumber = null,
                         fbPath: EHXPLLLConfig.FeedbackType = EHXPLLLConfig.CLKOP) {

  val vcoFreq: HertzNumber = fbPath match {
    case EHXPLLLConfig.CLKOP | EHXPLLLConfig.INT_CLKOP => (clkiFreq * opDiv * fbDiv) / mDiv
    case EHXPLLLConfig.CLKOS | EHXPLLLConfig.INT_CLKOS => (clkiFreq * osDiv * fbDiv) / mDiv
    case EHXPLLLConfig.CLKOS2 | EHXPLLLConfig.INT_CLKOS2 => (clkiFreq * os2Div * fbDiv) / mDiv
    case EHXPLLLConfig.CLKOS3 | EHXPLLLConfig.INT_CLKOS3 => (clkiFreq * os3Div * fbDiv) / mDiv
    case EHXPLLLConfig.USERCLOCK => clkiFreq / mDiv
  }

  val clkopFreq = vcoFreq/opDiv
  val clkosFreq = vcoFreq/osDiv
  val clkos2Freq = vcoFreq/os2Div
  val clkos3Freq = vcoFreq/os3Div

  // Check frequencies
  assert(clkiFreq >= EHXPLLLConfig.CLK_IN_MIN &&
    clkiFreq <= EHXPLLLConfig.CLK_IN_MAX, s"EHXPLLL CLKI frequency must be >= 8MHz and <= 400MHz. Was ${clkiFreq}")
  assert(clkopFreq >= EHXPLLLConfig.CLK_OUT_MIN &&
    clkopFreq <= EHXPLLLConfig.CLK_OUT_MAX, s"EHXPLLL CLKOP frequency must be >= 3.125MHz and <= 400MHz. Was ${clkopFreq}")
  if (osDiv != -1)
    assert(clkosFreq >= EHXPLLLConfig.CLK_OUT_MIN &&
      clkosFreq <= EHXPLLLConfig.CLK_OUT_MAX, "EHXPLLL CLKOS frequency must be >= 3.125MHz and <= 400MHz")
  if (os2Div != -1)
    assert(clkos2Freq >= EHXPLLLConfig.CLK_OUT_MIN &&
      clkos2Freq <= EHXPLLLConfig.CLK_OUT_MAX, "EHXPLLL CLKOS2 frequency must be >= 3.125MHz and <= 400MHz")
  if (os3Div != -1)
    assert(clkos3Freq >= EHXPLLLConfig.CLK_OUT_MIN &&
      clkos3Freq <= EHXPLLLConfig.CLK_OUT_MAX, "EHXPLLL CLKOS3 frequency must be >= 3.125MHz and <= 400MHz")
  if (clkfbFreq != null)
    assert(clkfbFreq >= EHXPLLLConfig.CLK_IN_MIN &&
      clkfbFreq <= EHXPLLLConfig.CLK_IN_MAX, "EHXPLLL CLKFB frequency must be >= 8MHz and <= 400MHz")
  assert(vcoFreq >= EHXPLLLConfig.VCO_MIN &&
         vcoFreq <= EHXPLLLConfig.VCO_MAX, "EHXPLLL resulting VCO frequency must be >= 400MHz and <= 800MHz")

}

object EHXPLLL {
  /**
    * Generates a basic PLL and clock domain from a source clock domain. Resulting frequency may differ from requested frequency.
    * @param sourceCd Source clock domain to derive the PLL source from
    * @param reqFreq The requrested output frequency
    * @return PLL clock domain
    */
  def makePLL(sourceCd: ClockDomain, reqFreq: HertzNumber, tolerance: BigDecimal = 0.02): ClockDomain = {
    val pll = new EHXPLLL(EHXPLLLConfig.singleOutput(sourceCd.frequency.getValue, reqFreq, tolerance))

    pll.io.CLKI := sourceCd.readClockWire
    pll.io.CLKFB := pll.io.CLKOP
    pll.io.STDBY := False
    pll.io.RST := False
    pll.io.ENCLKOP := True
    pll.io.ENCLKOS := False
    pll.io.ENCLKOS2 := False
    pll.io.ENCLKOS3 := False
    pll.io.PLLWAKESYNC := False
    pll.io.PHASESEL0 := False
    pll.io.PHASESEL1 := False
    pll.io.PHASEDIR := False
    pll.io.PHASESTEP := False
    pll.io.PHASELOADREG := False

    val pllCd = sourceCd.copy(clock = pll.io.CLKOP, frequency = FixedFrequency(pll.config.clkopFreq))
    pllCd
  }
}

case class EHXPLLL(config: EHXPLLLConfig) extends BlackBox {
  val generic = new Generic {
    val CLKI_DIV: Int = config.mDiv
    val CLKFB_DIV: Int = config.fbDiv
    val CLKOP_DIV: Int = config.opDiv
    val CLKOS_DIV: Int = if (config.osDiv != -1) config.osDiv else config.opDiv
    val CLKOS2_DIV: Int = if (config.osDiv != -1) config.os2Div else config.opDiv
    val CLKOS3_DIV: Int = if (config.osDiv != -1) config.os3Div else config.opDiv
    val CLKOP_ENABLE: String = "ENABLED"
    val CLKOS_ENABLE: String = if (config.osDiv != -1) "ENABLED" else "DISABLED"
    val CLKOS2_ENABLE: String = if (config.os2Div != -1) "ENABLED" else "DISABLED"
    val CLKOS3_ENABLE: String = if (config.os3Div != -1) "ENABLED" else "DISABLED"
    val CLKOP_CPHASE: Int = 0
    val CLKOS_CPHASE: Int = 0
    val CLKOS2_CPHASE: Int = 0
    val CLKOS3_CPHASE: Int = 0
    val CLKOP_FPHASE: Int = 0
    val CLKOS_FPHASE: Int = 0
    val CLKOS2_FPHASE: Int = 0
    val CLKOS3_FPHASE: Int = 0
    val FEEDBK_PATH: String = config.fbPath.toString
    val CLKOP_TRIM_POL: String = "RISING"
    val CLKOP_TRIM_DELAY: Int = 0
    val CLKOS_TRIM_POL: String = "RISING"
    val CLKOS_TRIM_DELAY: Int = 0
    val OUTDIVIDER_MUXA: String = "DIVA"
    val OUTDIVIDER_MUXB: String = "DIVB"
    val OUTDIVIDER_MUXC: String = "DIVC"
    val OUTDIVIDER_MUXD: String = "DIVD"
    val PLL_LOCK_MODE: Int = 0
    val PLL_LOCK_DELAY: Int = 200
    val STDBY_ENABLE: String = "DISABLED"
    val REFIN_RESET: String = "DISABLED"
    val SYNC_ENABLE: String = "DISABLED"
    val INT_LOCK_STICKY: String = "DISABLED"
    val DPHASE_SOURCE: String = "DISABLED"
    val PLLRST_ENA: String = "DISABLED"
    val INTFB_WAKE: String = "DISABLED"
  }
  val io = new Bundle {
    val CLKI = in Bool()
    val CLKFB = in Bool()
    val CLKINTFB = out Bool()
    val PHASESEL0 = in Bool()
    val PHASESEL1 = in Bool()
    val PHASEDIR = in Bool()
    val PHASESTEP = in Bool()
    val PHASELOADREG = in Bool()
    val STDBY = in Bool()
    val RST = in Bool()
    val ENCLKOP = in Bool()
    val ENCLKOS = in Bool()
    val ENCLKOS2 = in Bool()
    val ENCLKOS3 = in Bool()
    val PLLWAKESYNC = in Bool()
    val CLKOP = out Bool()
    val CLKOS = out Bool()
    val CLKOS2 = out Bool()
    val CLKOS3 = out Bool()
    val LOCK = out Bool()
    val INTLOCK = out Bool()
    val REFCLK = out Bool()
  }

  noIoPrefix()
}
