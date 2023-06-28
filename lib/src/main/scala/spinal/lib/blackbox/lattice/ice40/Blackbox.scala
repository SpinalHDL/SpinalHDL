package spinal.lib.blackbox.lattice.ice40

import spinal.core._

import scala.language.postfixOps

sealed trait FeedbackPath {
  override def toString: String = this.getClass.getSimpleName.dropRight(1)
}

object FeedbackPath {
  case object SIMPLE extends FeedbackPath
  case object DELAY extends FeedbackPath
  case object PHASE_AND_DELAY extends FeedbackPath
  case object EXTERNAL extends FeedbackPath
}

sealed trait PllOutSelect {
  override def toString: String = this.getClass.getSimpleName.dropRight(1)
}

case object PllOutSelect {
  case object GENCLK extends PllOutSelect
  case object GENCLK_HALF extends PllOutSelect
  case object SHIFTREG_90deg extends PllOutSelect
  case object SHIFTREG_0deg extends PllOutSelect
}

sealed trait ShiftregDivMode

case object ShiftregDivMode {
  case object DIV_4 extends ShiftregDivMode
  case object DIV_7 extends ShiftregDivMode
}

sealed trait AdjustmentMode {
  override def toString: String = this.getClass.getSimpleName.dropRight(1)
}

object AdjustmentMode {
  case object FIXED extends AdjustmentMode
  case object DYNAMIC extends AdjustmentMode
}

abstract class AbstractPllConfig {
  def applyTo(bb: BlackBox): Unit
  def withExtFeedback: Boolean
  def withDynamicDelay: Boolean
  def withLatchInputValue: Boolean
  def withLock: Boolean
}

@deprecated("Use SB_PLL40_CONFIG instead as it provides checking/calculation features")
case class SB_PLL40_PAD_CONFIG(
    var DIVR: Bits,
    var DIVF: Bits,
    var DIVQ: Bits,
    var FILTER_RANGE: Bits,
    var FEEDBACK_PATH: String,
    var DELAY_ADJUSTMENT_MODE_FEEDBACK: String,
    var FDA_FEEDBACK: Bits,
    var DELAY_ADJUSTMENT_MODE_RELATIVE: String,
    var FDA_RELATIVE: Bits,
    var SHIFTREG_DIV_MODE: Bits,
    var PLLOUT_SELECT: String,
    var ENABLE_ICEGATE: Bool,
    withLock: Boolean = false
) extends AbstractPllConfig {
  def applyTo(bb: BlackBox): Unit = {
    bb.addGeneric("DIVR", DIVR)
    bb.addGeneric("DIVF", DIVF)
    bb.addGeneric("DIVQ", DIVQ)
    bb.addGeneric("FILTER_RANGE", FILTER_RANGE)
    bb.addGeneric("FEEDBACK_PATH", FEEDBACK_PATH)
    bb.addGeneric("DELAY_ADJUSTMENT_MODE_FEEDBACK", DELAY_ADJUSTMENT_MODE_FEEDBACK)
    bb.addGeneric("FDA_FEEDBACK", FDA_FEEDBACK)
    bb.addGeneric("DELAY_ADJUSTMENT_MODE_RELATIVE", DELAY_ADJUSTMENT_MODE_RELATIVE)
    bb.addGeneric("FDA_RELATIVE", FDA_RELATIVE)
    bb.addGeneric("SHIFTREG_DIV_MODE", SHIFTREG_DIV_MODE)
    bb.addGeneric("PLLOUT_SELECT", PLLOUT_SELECT)
    bb.addGeneric("ENABLE_ICEGATE", ENABLE_ICEGATE)
  }

  override def withExtFeedback = FEEDBACK_PATH == "EXTERNAL"
  override def withDynamicDelay =
    DELAY_ADJUSTMENT_MODE_FEEDBACK == "DYNAMIC" || DELAY_ADJUSTMENT_MODE_RELATIVE == "DYNAMIC"
  override def withLatchInputValue = ENABLE_ICEGATE == True
}

case class SB_PLL40_CONFIG(
    DIVR: Int,
    DIVF: Int,
    DIVQ: Int,
    FILTER_RANGE: Int,
    FEEDBACK_PATH: FeedbackPath = FeedbackPath.SIMPLE,
    DELAY_ADJUSTMENT_MODE_FEEDBACK: AdjustmentMode = AdjustmentMode.FIXED,
    FDA_FEEDBACK: Int,
    DELAY_ADJUSTMENT_MODE_RELATIVE: AdjustmentMode = AdjustmentMode.FIXED,
    FDA_RELATIVE: Int,
    SHIFTREG_DIV_MODE: ShiftregDivMode = ShiftregDivMode.DIV_4,
    PLLOUT_SELECT: PllOutSelect = PllOutSelect.GENCLK,
    ENABLE_ICEGATE: Boolean = false,
    withLock: Boolean = false
) extends AbstractPllConfig {
  require(DIVR >= 0 && DIVR <= 15)
  require(DIVF >= 0 && DIVF <= 127)
  require(DIVQ >= 0 && DIVQ <= 7)
  require(FILTER_RANGE >= 0 && FILTER_RANGE <= 7)
  require(FDA_FEEDBACK >= 0 && FDA_FEEDBACK <= 15)
  require(FDA_RELATIVE >= 0 && FDA_RELATIVE <= 15)

  def applyTo(bb: BlackBox): Unit = {
    bb.addGeneric("DIVR", DIVR)
    bb.addGeneric("DIVF", DIVF)
    bb.addGeneric("DIVQ", DIVQ)
    bb.addGeneric("FILTER_RANGE", FILTER_RANGE)
    bb.addGeneric("FEEDBACK_PATH", FEEDBACK_PATH.toString)
    bb.addGeneric("DELAY_ADJUSTMENT_MODE_FEEDBACK", DELAY_ADJUSTMENT_MODE_FEEDBACK.toString)
    bb.addGeneric("FDA_FEEDBACK", FDA_FEEDBACK)
    bb.addGeneric("DELAY_ADJUSTMENT_MODE_RELATIVE", DELAY_ADJUSTMENT_MODE_RELATIVE.toString)
    bb.addGeneric("FDA_RELATIVE", FDA_RELATIVE)
    bb.addGeneric("SHIFTREG_DIV_MODE", if (SHIFTREG_DIV_MODE == ShiftregDivMode.DIV_4) 0 else 1)
    bb.addGeneric("PLLOUT_SELECT", PLLOUT_SELECT.toString)
    bb.addGeneric("ENABLE_ICEGATE", ENABLE_ICEGATE.toInt)
  }

  def fout(fin: HertzNumber) = {
    SB_PLL40_CONFIG.SingleClockSettings(fin, DIVR, DIVF, DIVQ, FEEDBACK_PATH).fout
  }

  override def withExtFeedback = FEEDBACK_PATH == FeedbackPath.EXTERNAL
  override def withDynamicDelay =
    Seq(DELAY_ADJUSTMENT_MODE_FEEDBACK, DELAY_ADJUSTMENT_MODE_RELATIVE).contains(
      AdjustmentMode.DYNAMIC
    )
  override def withLatchInputValue = ENABLE_ICEGATE
}

object SB_PLL40_CONFIG {
  // see iCE40 LP/HP Family Data Sheet, FPGA-DS-02029-4.0, p. 35
  val fin_min: HertzNumber = 10 MHz
  val fin_max: HertzNumber = 133 MHz
  val fvco_min: HertzNumber = 533 MHz
  val fvco_max: HertzNumber = 1066 MHz
  val fout_min: HertzNumber = 16 MHz
  val fout_max: HertzNumber = 275 MHz
  // not from spec, taken from icepll
  // https://github.com/YosysHQ/icestorm/blob/d20a5e9001f46262bf0cef220f1a6943946e421d/icepll/icepll.cc#LL110C43-L110C43
  val fdiv_min: HertzNumber = 10 MHz
  val fdiv_max: HertzNumber = 133 MHz

  case class SingleClockSettings(
      fin: HertzNumber,
      divr: Int,
      divf: Int,
      divq: Int,
      feedback: FeedbackPath,
      private val shiftreg_div: Int = 1
  ) {
    def fdiv: HertzNumber = fin / (divr + 1)

    // note that PLL uses 4x frequency internally in PHASE_AND_DELAY mode
    def fvco: HertzNumber = feedback match {
      case FeedbackPath.SIMPLE                        => fdiv * (divf + 1)
      case FeedbackPath.DELAY | FeedbackPath.EXTERNAL => fdiv * (divf + 1) * (1 << divq)
      case FeedbackPath.PHASE_AND_DELAY               => fdiv * (divf + 1) * (1 << divq) * shiftreg_div
    }

    def fout: HertzNumber = feedback match {
      case FeedbackPath.PHASE_AND_DELAY => fvco / (1 << divq) / shiftreg_div
      case _                            => fvco / (1 << divq)
    }

    def isValid: Boolean =
      fdiv >= fdiv_min && fdiv <= fdiv_max && fvco >= fvco_min && fvco <= fvco_max && fout >= fout_min && fout <= fout_max

    def report(): String = {
      val feedbackStr =
        if (feedback == FeedbackPath.SIMPLE) "SIMPLE" else "DELAY or PHASE_AND_DELAY"
      f"""
         |input freq:   $fin
         |output freq:  $fout
         |
         |feedback: $feedbackStr
         |PFD freq: $fdiv
         |VCO freq: $fvco
         |
         |DIVR: $divr
         |DIVF: $divf
         |DIVQ: $divq
         |""".stripMargin
    }
  }

  def calculate(
      fin: HertzNumber,
      fout_req: HertzNumber,
      feedback_req: Option[FeedbackPath],
      shiftreg_div: Int = 1
  ): Option[SingleClockSettings] = {
    require(fin >= fin_min && fin <= fin_max, s"PLL input must be in range [10.0, 133.0], not $fin")
    require(
      fout_req >= fout_min && fout_req <= fout_max,
      s"PLL output must be in range [16.0, 275.0], not $fout_req"
    )

    def better_match(
        a: Option[SingleClockSettings],
        b: Option[SingleClockSettings]
    ): Option[SingleClockSettings] = {
      (a, b) match {
        case (None, Some(_))                                                             => b
        case (Some(_), None)                                                             => a
        case (Some(aa), Some(bb)) if (aa.fout - fout_req).abs < (bb.fout - fout_req).abs => a
        case _                                                                           => b
      }
    }

    def best_div(feedback: FeedbackPath): Option[SingleClockSettings] = {
      import BigDecimal.RoundingMode.{FLOOR, CEILING}
      var best: Option[SingleClockSettings] = None

      def update_if_valid(divr: Int, divf: Int, divq: Int): Unit = {
        // icepll mentions that the PLL Usage guide lists the wrong limit for DIVF for simple feedback
        // https://github.com/YosysHQ/icestorm/blob/d20a5e9001f46262bf0cef220f1a6943946e421d/icepll/icepll.cc#L93
        if (divf < 0 || divf > 127 || (feedback != FeedbackPath.SIMPLE && divf > 63))
          return

        val settings = SingleClockSettings(fin, divr, divf, divq, feedback, shiftreg_div)
        if (!settings.isValid)
          return

        best = better_match(best, Some(settings))
      }

      if (feedback == FeedbackPath.SIMPLE) {
        for (divr <- 0 until 16) {
          for (divq <- 0 until 8) {
            val divf_exact = ((fout_req * (divr + 1) * (1 << divq)) / fin) - 1
            update_if_valid(divr, divf_exact.setScale(0, FLOOR).toInt, divq)
            update_if_valid(divr, divf_exact.setScale(0, CEILING).toInt, divq)
          }
        }
      } else {
        for (divr <- 0 until 16) {
          val divf_exact = ((fout_req * (divr + 1)) / fin) - 1
          for (divq <- 0 until 8) {
            update_if_valid(divr, divf_exact.setScale(0, FLOOR).toInt, divq)
            update_if_valid(divr, divf_exact.setScale(0, CEILING).toInt, divq)
          }
        }
      }
      best
    }

    feedback_req match {
      case Some(x) => best_div(x)
      case None    => better_match(best_div(FeedbackPath.SIMPLE), best_div(FeedbackPath.DELAY))
    }
  }

  def singleOutput(
      fin: HertzNumber,
      fout: HertzNumber,
      tolerance: Double = 0.01,
      FEEDBACK_PATH: Option[FeedbackPath] = Some(FeedbackPath.SIMPLE),
      FDA_FEEDBACK: Option[Int] = None,
      FDA_RELATIVE: Option[Int] = None,
      SHIFTREG_DIV_MODE: Option[ShiftregDivMode] = None,
      PLLOUT_SELECT: PllOutSelect = PllOutSelect.GENCLK,
      ENABLE_ICEGATE: Boolean = false,
      withLock: Boolean = false
  ) = {
    require(
      FEEDBACK_PATH.isEmpty || FEEDBACK_PATH.get != FeedbackPath.PHASE_AND_DELAY || PLLOUT_SELECT == PllOutSelect.SHIFTREG_0deg || PLLOUT_SELECT == PllOutSelect.SHIFTREG_90deg,
      "if feedback path is PHASE_AND_DELAY, output select must be SHIFTREG_Xdeg"
    )
    require(
      SHIFTREG_DIV_MODE.isEmpty || FEEDBACK_PATH
        .getOrElse(FeedbackPath.SIMPLE) == FeedbackPath.PHASE_AND_DELAY,
      "SHIFTREG_DIV_MODE can only be used in PHASE_AND_DELAY feedback mode"
    )
    require(
      (PLLOUT_SELECT != PllOutSelect.SHIFTREG_0deg && PLLOUT_SELECT != PllOutSelect.SHIFTREG_90deg) || (FEEDBACK_PATH.isDefined && FEEDBACK_PATH.get == FeedbackPath.PHASE_AND_DELAY),
      "SHIFTREG_Xdeg output selection can only be used with PHASE_AND_DELAY feedback mode"
    )

    val best = calculate(
      fin,
      fout,
      FEEDBACK_PATH,
      SHIFTREG_DIV_MODE match {
        case Some(ShiftregDivMode.DIV_4) => 4
        case Some(ShiftregDivMode.DIV_7) => 7
        case None =>
          4 // ignored if feedback mode is != PHASE_AND_DELAY, and we default to 4 if not set
      }
    )
    if (best.isEmpty)
      throw new Exception(s"Could not find any PLL configuration for fin=$fin fout=$fout")

    val solution = best.get
    if (((solution.fout - fout).abs / fout) > tolerance)
      throw new Exception(
        s"Could not find PLL configuration for fin=$fin fout=$fout within ${tolerance * 100}%\n" +
          s"  best match is ${((solution.fout - fout).abs / fout) * 100}% off:${best.get.report()}"
      )

    // not from spec, values taken from icepll
    // https://github.com/YosysHQ/icestorm/blob/d20a5e9001f46262bf0cef220f1a6943946e421d/icepll/icepll.cc#L316
    val filter_range = solution.fdiv match {
      case x if x < (17 MHz)  => 1
      case x if x < (26 MHz)  => 2
      case x if x < (44 MHz)  => 3
      case x if x < (66 MHz)  => 4
      case x if x < (101 MHz) => 5
      case _                  => 6
    }

    SB_PLL40_CONFIG(
      solution.divr,
      solution.divf,
      solution.divq,
      filter_range,
      solution.feedback,
      if (FDA_FEEDBACK.isDefined) AdjustmentMode.DYNAMIC else AdjustmentMode.FIXED,
      FDA_FEEDBACK.getOrElse(0),
      if (FDA_RELATIVE.isDefined) AdjustmentMode.DYNAMIC else AdjustmentMode.FIXED,
      FDA_RELATIVE.getOrElse(0),
      SHIFTREG_DIV_MODE.getOrElse(ShiftregDivMode.DIV_4),
      PLLOUT_SELECT,
      ENABLE_ICEGATE,
      withLock
    )
  }
}

abstract class ICE40_PLL(p: AbstractPllConfig) extends BlackBox {
  val RESETB = in port Bool() default True
  val BYPASS = in port Bool() default False
  val EXTFEEDBACK = p.withExtFeedback generate(in port Bool())
  val DYNAMICDELAY = p.withDynamicDelay generate(in port Bits(8 bit))
  val LATCHINPUTVALUE = p.withLatchInputValue generate(in port Bool())
  val LOCK = p.withLock generate(out port Bool())
  val PLLOUTGLOBAL = out port Bool()
  val PLLOUTCORE = out port Bool()

  def clockInput: Bool
}

object ICE40_PLL {

  /** Calculate & instantiate PLL and return ClockDomain
    *
    * If the source clock domain uses a synchronous reset, a reset bridge
    * will be instantiated to forward the reset to the resulting CD.
    *
    * @param reqFreq The frequency the new ClockDomain should run at
    * @param tolerance Allowed mismatch of ClockDomain freq to reqFreq (as a factor)
    * @param sourceCd ClockDomain to use as clock/reset source, defaults to current CD
    * @param usePad if true a SB_PLL40_PAD will be used, otherwise a SB_PLL40_CORE
    *
    * {{{
    * new ClockingArea(ICE40_PLL.makePLL(96 MHz)) {
    *   // your code running at 96MHz
    * }
    * }}}
    */
  def makePLL(
      reqFreq: HertzNumber,
      tolerance: Double = 0.02,
      sourceCd: ClockDomain = ClockDomain.current,
      usePad: Boolean = false
  ) = {
    require(
      sourceCd.frequency.isInstanceOf[FixedFrequency],
      "Source clock domain for PLL must have a fixed & known frequency"
    )
    require(
      !sourceCd.hasClockEnableSignal,
      "Source clock domain for PLL must not use a clock enable"
    )
    require(
      sourceCd.config.clockEdge == RISING || sourceCd.config.resetKind == BOOT,
      "Currently only rising edge clocks are supported when reset is used"
    )

    val config = SB_PLL40_CONFIG.singleOutput(
      sourceCd.frequency.getValue,
      reqFreq,
      tolerance,
      withLock = sourceCd.config.resetKind == SYNC
    )
    val pll = if (usePad) SB_PLL40_PAD(config) else SB_PLL40_CORE(config)
    pll.clockInput := sourceCd.readClockWire
    pll.BYPASS := False
    val bufferedClk = SB_GB(pll.PLLOUTGLOBAL)

    def resetBridge(clk: Bool, rst: Bool): Bool = sourceCd.config.resetActiveLevel match {
      case HIGH => SB_DFFS(SB_DFFS(False, clk, rst), bufferedClk, rst)
      case LOW  => SB_DFFR(SB_DFFR(True, clk, rst), bufferedClk, rst)
    }

    val resultFreq = FixedFrequency(config.fout(sourceCd.frequency.getValue))
    sourceCd.config.resetKind match {
      case BOOT =>
        sourceCd.copy(clock = bufferedClk, frequency = resultFreq)
      case SYNC | ASYNC =>
        pll.RESETB := !sourceCd.isResetActive
        val syncRst = resetBridge(bufferedClk, !pll.LOCK)
        sourceCd.copy(clock = bufferedClk, reset = syncRst, frequency = resultFreq)
    }
  }
}

case class SB_PLL40_PAD(p: AbstractPllConfig) extends ICE40_PLL(p) {
  val PACKAGEPIN = in port Bool()

  p.applyTo(this)

  override def clockInput: Bool = PACKAGEPIN
}

case class SB_PLL40_CORE(p: AbstractPllConfig) extends ICE40_PLL(p) {
  val REFERENCECLK = in Bool ()

  p.applyTo(this)

  override def clockInput: Bool = REFERENCECLK
}

object SB_GB {
  def apply(input: Bool): Bool = {
    val c = SB_GB().setCompositeName(input, "SB_GB", weak = true)
    c.USER_SIGNAL_TO_GLOBAL_BUFFER := input
    c.GLOBAL_BUFFER_OUTPUT
  }
}

case class SB_GB() extends BlackBox {
  val USER_SIGNAL_TO_GLOBAL_BUFFER = in Bool ()
  val GLOBAL_BUFFER_OUTPUT = out Bool ()
}

object SB_IO {
  def ddrRegistredOutput() = SB_IO("010000")
  def ddrRegistredInout() = SB_IO("110000")
}

case class SB_IO(pinType: String) extends BlackBox {
  addGeneric("PIN_TYPE", B(pinType))
  val PACKAGE_PIN = inout(Analog(Bool()))
  val CLOCK_ENABLE = in Bool () default False
  val INPUT_CLK = in Bool () default False
  val OUTPUT_CLK = in Bool () default False
  val OUTPUT_ENABLE = in Bool () default False
  val D_OUT_0 = in Bool () default False
  val D_OUT_1 = in Bool () default False
  val D_IN_0 = out Bool ()
  val D_IN_1 = out Bool ()
  setDefinitionName("SB_IO")
}

case class SB_SPRAM256KA() extends BlackBox {
  val DATAIN = in Bits (16 bits)
  val ADDRESS = in UInt (14 bits)
  val MASKWREN = in Bits (4 bits)
  val WREN = in Bool ()
  val CHIPSELECT = in Bool ()
  val CLOCK = in Bool ()
  val DATAOUT = out Bits (16 bits)
  val STANDBY = in Bool ()
  val SLEEP = in Bool ()
  val POWEROFF = in Bool ()
  mapCurrentClockDomain(CLOCK)

//  val ram = Mem(Bits(16 bits), 16*1024)
//  DATAOUT := ram.readWriteSync(
//    address = ADDRESS,
//    data = DATAIN,
//    enable = CHIPSELECT,
//    write = WREN,
//    mask = MASKWREN
//  )
}

case class SB_DFFR() extends BlackBox {
  val D = in port Bool()
  val C = in port Bool()
  val R = in port Bool()
  val Q = out port Bool()
}

object SB_DFFR {
  def apply(D: Bool, C: Bool, R: Bool): Bool = {
    val ff = SB_DFFR()
    ff.D := D
    ff.C := C
    ff.R := R
    ff.Q
  }
}

case class SB_DFFS() extends BlackBox {
  val D = in port Bool()
  val C = in port Bool()
  val S = in port Bool()
  val Q = out port Bool()
}

object SB_DFFS {
  def apply(D: Bool, C: Bool, S: Bool): Bool = {
    val ff = SB_DFFS()
    ff.D := D
    ff.C := C
    ff.S := S
    ff.Q
  }
}
