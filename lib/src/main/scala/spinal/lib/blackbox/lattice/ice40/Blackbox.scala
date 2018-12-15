package spinal.lib.blackbox.lattice.ice40

import spinal.core._

case class SB_PLL40_PAD_CONFIG( var DIVR : Bits,
                                var DIVF : Bits,
                                var DIVQ : Bits,
                                var FILTER_RANGE : Bits,
                                var FEEDBACK_PATH : String,
                                var DELAY_ADJUSTMENT_MODE_FEEDBACK : String,
                                var FDA_FEEDBACK : Bits,
                                var DELAY_ADJUSTMENT_MODE_RELATIVE : String,
                                var FDA_RELATIVE : Bits,
                                var SHIFTREG_DIV_MODE : Bits,
                                var PLLOUT_SELECT : String,
                                var ENABLE_ICEGATE : Bool
                              ){
  def applyTo(bb: BlackBox): Unit = {
    bb.addGeneric("DIVR", DIVR)
    bb.addGeneric("DIVF", DIVF)
    bb.addGeneric("DIVQ",DIVQ)
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
}

case class SB_PLL40_PAD(p : SB_PLL40_PAD_CONFIG) extends BlackBox{
  val PACKAGEPIN = in Bool()
  val PLLOUTCORE = out Bool()
  val PLLOUTGLOBAL = out Bool()
  val RESETB = in Bool()
  val BYPASS = in Bool()

  p.applyTo(this)
}

case class SB_PLL40_CORE(p : SB_PLL40_PAD_CONFIG) extends BlackBox{
  val REFERENCECLK = in Bool()
  val PLLOUTCORE = out Bool()
  val PLLOUTGLOBAL = out Bool()
  val RESETB = in Bool()
  val BYPASS = in Bool()

  p.applyTo(this)
}
