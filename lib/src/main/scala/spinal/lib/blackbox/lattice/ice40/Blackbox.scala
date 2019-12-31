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


object SB_GB{
  def apply(input : Bool) : Bool = {
    val c = SB_GB().setCompositeName(input, "SB_GB", true)
    c.USER_SIGNAL_TO_GLOBAL_BUFFER := input
    c.GLOBAL_BUFFER_OUTPUT
  }
}

case class SB_GB() extends BlackBox{
  val USER_SIGNAL_TO_GLOBAL_BUFFER = in Bool()
  val GLOBAL_BUFFER_OUTPUT = out Bool()
}



object SB_IO{
  def ddrRegistredOutput() = SB_IO("010000")
  def ddrRegistredInout() = SB_IO("110000")
}

case class SB_IO(pinType : String) extends BlackBox{
  addGeneric("PIN_TYPE", B(pinType))
  val PACKAGE_PIN = inout(Analog(Bool))
  val CLOCK_ENABLE = in Bool() default(False)
  val INPUT_CLK = in Bool() default(False)
  val OUTPUT_CLK = in Bool() default(False)
  val OUTPUT_ENABLE = in Bool() default(False)
  val D_OUT_0 = in Bool() default(False)
  val D_OUT_1 = in Bool() default(False)
  val D_IN_0 = out Bool()
  val D_IN_1 = out Bool()
  setDefinitionName("SB_IO")
}

case class SB_SPRAM256KA() extends BlackBox{
  val DATAIN = in Bits(16 bits)
  val ADDRESS = in UInt(14 bits)
  val MASKWREN = in Bits(4 bits)
  val WREN = in Bool()
  val CHIPSELECT = in Bool()
  val CLOCK = in Bool()
  val DATAOUT = out Bits(16 bits)
  val STANDBY = in Bool()
  val SLEEP = in Bool()
  val POWEROFF = in Bool()
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

