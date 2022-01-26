package spinal.lib.blackbox.xilinx.s7

import spinal.core._
import spinal.core.fiber.Handle


object BUFGCE{
  def on(that: Bool, clockEnable: Bool): Bool = {
    val bufgce = BUFGCE()
    bufgce.setCompositeName(that, "BUFGCE")
    bufgce.I := that
    bufgce.CE := clockEnable
    bufgce.O
  }
  def onReset(that : Handle[ClockDomain]): Handle[ClockDomain] = Handle {
    that.copy(reset = BUFG.on(that.reset))
  }}

case class BUFGCE() extends BlackBox{
  val I = in Bool()
  val CE = in Bool()
  val O = out Bool()
  O := CE.mux(True -> I, False -> False)
}

object BUFG{
  def on(that : Bool) : Bool = {
    val bufg = BUFG()
    bufg.setCompositeName(that, "BUFG")
    bufg.I := that
    bufg.O
  }

  def onReset(that : Handle[ClockDomain]): Handle[ClockDomain] = Handle {
    that.copy(reset = BUFG.on(that.reset))
  }
}

case class BUFG() extends BlackBox {
  val I = in Bool()
  val O = out Bool()

  O := I
}


object IBUF{
  def on(that : Bool) : Bool = {
    val bufg = BUFG()
    bufg.setCompositeName(that, "IBUF")
    bufg.I := that
    bufg.O
  }
}

case class IBUF() extends BlackBox {
  val I = in Bool()
  val O = out Bool()

  O := I
}



object BUFIO{
  def on(that : Bool) : Bool = {
    val bufio = BUFIO()
    bufio.setCompositeName(that, "BUFIO")
    bufio.I := that
    bufio.O
  }
}

case class BUFIO() extends BlackBox {
  val I = in Bool()
  val O = out Bool()
}
/*
  Basic example CLKOUT[n] = (CLKIN1 * clkOut_Mult) / clkOut[n]_Divide
  (source clk = 125 Mhz)
  val pll = new PLLE2_BASE(clkOut_Mult = 10,      // 1250 MHz
                            clkOut0_Divide = 50)  //  25 MHz
  pll.CLKFBIN := pll.CLKFBOUT // internal loop back
  pll.CLKIN1 := io.clk // input clock 125 MHz
  pll.RST := False
  pll.PWRDWN := False
  
  ClockDomain(pll.CLKOUT0, frequency = FixedFrequency(25 MHz))
*/
case class PLLE2_BASE(
  bandwidth : String = "OPTIMIZED",
  startUpWait : String = "FALSE",
  clkIn1_Period : Double = 0.0, // 0.938 to 52.631ns
  clkOut_Mult : Int = 2,
  clkOut0_Divide : Int = 1,
  clkOut0_Phase : Double = 0.0,
  clkOut0_DutyCycle : Double = 0.5,
  clkOut1_Divide : Int = 1,
  clkOut1_Phase : Double  = 0.0,
  clkOut1_DutyCycle : Double = 0.5,
  clkOut2_Divide : Int = 1,
  clkOut2_Phase : Double  = 0.0,
  clkOut2_DutyCycle : Double = 0.5,
  clkOut3_Divide : Int = 1,
  clkOut3_Phase : Double  = 0.0,
  clkOut3_DutyCycle : Double = 0.5,
  clkOut4_Divide : Int = 1,
  clkOut4_Phase : Double  = 0.0,
  clkOut4_DutyCycle : Double = 0.5,
  clkOut5_Divide : Int = 1,
  clkOut5_Phase : Double  = 0.0,
  clkOut5_DutyCycle : Double = 0.5
  ) extends BlackBox{

  addGeneric("BANDWIDTH", bandwidth )
  addGeneric("STARTUP_WAIT", startUpWait )
  addGeneric("CLKIN1_PERIOD", clkIn1_Period )
  addGeneric("CLKFBOUT_MULT", clkOut_Mult )

  addGeneric("CLKOUT0_DIVIDE", clkOut0_Divide )
  addGeneric("CLKOUT0_PHASE", clkOut0_Phase )
  addGeneric("CLKOUT0_DUTY_CYCLE", clkOut0_DutyCycle )

  addGeneric("CLKOUT1_DIVIDE", clkOut1_Divide )
  addGeneric("CLKOUT1_PHASE", clkOut1_Phase )
  addGeneric("CLKOUT1_DUTY_CYCLE", clkOut1_DutyCycle )

  addGeneric("CLKOUT2_DIVIDE", clkOut2_Divide )
  addGeneric("CLKOUT2_PHASE", clkOut2_Phase )
  addGeneric("CLKOUT2_DUTY_CYCLE", clkOut2_DutyCycle )

  addGeneric("CLKOUT3_DIVIDE", clkOut3_Divide )
  addGeneric("CLKOUT3_PHASE", clkOut3_Phase )
  addGeneric("CLKOUT3_DUTY_CYCLE", clkOut3_DutyCycle )

  addGeneric("CLKOUT4_DIVIDE", clkOut4_Divide )
  addGeneric("CLKOUT4_PHASE", clkOut3_Phase )
  addGeneric("CLKOUT4_DUTY_CYCLE", clkOut4_DutyCycle )

  addGeneric("CLKOUT5_DIVIDE", clkOut5_Divide )
  addGeneric("CLKOUT5_PHASE", clkOut5_Phase )
  addGeneric("CLKOUT5_DUTY_CYCLE", clkOut5_DutyCycle )

  val CLKIN1 = in Bool()
  val CLKFBIN = in Bool()
  val CLKFBOUT = out Bool()

  val RST = in Bool()
  val LOCKED = out Bool()

  val CLKOUT0  = out Bool()
  val CLKOUT1  = out Bool()
  val CLKOUT2  = out Bool()
  val CLKOUT3  = out Bool()
  val CLKOUT4  = out Bool()
  val CLKOUT5  = out Bool()
  val PWRDWN = in Bool()
}

// See UG472 (v1.14), 7 Series Clocking, p. 78 ff
case class MMCME2_BASE(
    CLKIN1_PERIOD: Double,
    BANDWIDTH: String = "OPTIMIZED",
    CLKFBOUT_MULT_F: Double = 5.0,
    CLKFBOUT_PHASE: Double = 0.0,
    CLKOUT0_DIVIDE_F: Double = 1.0,
    CLKOUT1_DIVIDE: Double = 1,
    CLKOUT2_DIVIDE: Double = 1,
    CLKOUT3_DIVIDE: Double = 1,
    CLKOUT4_DIVIDE: Double = 1,
    CLKOUT5_DIVIDE: Double = 1,
    CLKOUT0_DUTY_CYCLE: Double = 0.5,
    CLKOUT1_DUTY_CYCLE: Double = 0.5,
    CLKOUT2_DUTY_CYCLE: Double = 0.5,
    CLKOUT3_DUTY_CYCLE: Double = 0.5,
    CLKOUT4_DUTY_CYCLE: Double = 0.5,
    CLKOUT5_DUTY_CYCLE: Double = 0.5,
    CLKOUT0_PHASE: Double = 0.0,
    CLKOUT1_PHASE: Double = 0.0,
    CLKOUT2_PHASE: Double = 0.0,
    CLKOUT3_PHASE: Double = 0.0,
    CLKOUT4_PHASE: Double = 0.0,
    CLKOUT5_PHASE: Double = 0.0,
    CLKOUT4_CASCADE: String = "FALSE",
    DIVCLK_DIVIDE: Double = 1.0,
    REF_JITTER1: Double = 0.0,
    STARTUP_WAIT: String = "FALSE"
) extends BlackBox {
  assert(List("OPTIMIZED", "HIGH", "LOW").contains(BANDWIDTH), message = "Invalid BANDWIDTH")
  assert(CLKFBOUT_MULT_F >= 5.0 && CLKFBOUT_MULT_F <= 64.0, message = "Invalid CLKFBOUT_MULT_F")
  assert(CLKFBOUT_PHASE >= -360.0 && CLKFBOUT_PHASE <= 360.0)
  assert(CLKIN1_PERIOD >= 1.0 && CLKIN1_PERIOD <= 1000.0)
  assert(CLKOUT0_DIVIDE_F >= 1.0 && CLKOUT0_DIVIDE_F <= 128.0)
  assert(CLKOUT0_DUTY_CYCLE >= 0.001 && CLKOUT0_DUTY_CYCLE <= 0.999)
  assert(CLKOUT1_DUTY_CYCLE >= 0.001 && CLKOUT1_DUTY_CYCLE <= 0.999)
  assert(CLKOUT2_DUTY_CYCLE >= 0.001 && CLKOUT2_DUTY_CYCLE <= 0.999)
  assert(CLKOUT3_DUTY_CYCLE >= 0.001 && CLKOUT3_DUTY_CYCLE <= 0.999)
  assert(CLKOUT4_DUTY_CYCLE >= 0.001 && CLKOUT4_DUTY_CYCLE <= 0.999)
  assert(CLKOUT5_DUTY_CYCLE >= 0.001 && CLKOUT5_DUTY_CYCLE <= 0.999)
  assert(CLKOUT0_PHASE >= -360.0 && CLKOUT0_PHASE <= 360.0)
  assert(CLKOUT1_PHASE >= -360.0 && CLKOUT1_PHASE <= 360.0)
  assert(CLKOUT2_PHASE >= -360.0 && CLKOUT2_PHASE <= 360.0)
  assert(CLKOUT3_PHASE >= -360.0 && CLKOUT3_PHASE <= 360.0)
  assert(CLKOUT4_PHASE >= -360.0 && CLKOUT4_PHASE <= 360.0)
  assert(CLKOUT5_PHASE >= -360.0 && CLKOUT5_PHASE <= 360.0)
  assert(DIVCLK_DIVIDE >= 1 && DIVCLK_DIVIDE <= 128)
  assert(REF_JITTER1 >= 0.0 && REF_JITTER1 <= 0.999)

  addGeneric("BANDWIDTH", BANDWIDTH)
  addGeneric("CLKFBOUT_MULT_F", CLKFBOUT_MULT_F)
  addGeneric("CLKFBOUT_PHASE", CLKFBOUT_PHASE)
  addGeneric("CLKIN1_PERIOD", CLKIN1_PERIOD)
  addGeneric("CLKOUT0_DIVIDE_F", CLKOUT0_DIVIDE_F)
  addGeneric("CLKOUT1_DIVIDE", CLKOUT1_DIVIDE)
  addGeneric("CLKOUT2_DIVIDE", CLKOUT2_DIVIDE)
  addGeneric("CLKOUT3_DIVIDE", CLKOUT3_DIVIDE)
  addGeneric("CLKOUT4_DIVIDE", CLKOUT4_DIVIDE)
  addGeneric("CLKOUT5_DIVIDE", CLKOUT5_DIVIDE)
  addGeneric("CLKOUT0_DUTY_CYCLE", CLKOUT0_DUTY_CYCLE)
  addGeneric("CLKOUT1_DUTY_CYCLE", CLKOUT1_DUTY_CYCLE)
  addGeneric("CLKOUT2_DUTY_CYCLE", CLKOUT2_DUTY_CYCLE)
  addGeneric("CLKOUT3_DUTY_CYCLE", CLKOUT3_DUTY_CYCLE)
  addGeneric("CLKOUT4_DUTY_CYCLE", CLKOUT4_DUTY_CYCLE)
  addGeneric("CLKOUT5_DUTY_CYCLE", CLKOUT5_DUTY_CYCLE)
  addGeneric("CLKOUT0_PHASE", CLKOUT0_PHASE)
  addGeneric("CLKOUT1_PHASE", CLKOUT1_PHASE)
  addGeneric("CLKOUT2_PHASE", CLKOUT2_PHASE)
  addGeneric("CLKOUT3_PHASE", CLKOUT3_PHASE)
  addGeneric("CLKOUT4_PHASE", CLKOUT4_PHASE)
  addGeneric("CLKOUT5_PHASE", CLKOUT5_PHASE)
  addGeneric("CLKOUT4_CASCADE", CLKOUT4_CASCADE)
  addGeneric("DIVCLK_DIVIDE", DIVCLK_DIVIDE)
  addGeneric("REF_JITTER1", REF_JITTER1)
  addGeneric("STARTUP_WAIT", STARTUP_WAIT)

  val CLKIN1 = in Bool ()
  val CLKOUT0 = out Bool ()
  val CLKOUT0B = out Bool ()
  val CLKOUT1 = out Bool ()
  val CLKOUT1B = out Bool ()
  val CLKOUT2 = out Bool ()
  val CLKOUT2B = out Bool ()
  val CLKOUT3 = out Bool ()
  val CLKOUT3B = out Bool ()
  val CLKOUT4 = out Bool ()
  val CLKOUT5 = out Bool ()
  val CLKOUT6 = out Bool ()

  val CLKFBIN = in Bool ()
  val CLKFBOUT = out Bool ()
  val CLKFBOUTB = out Bool ()

  val LOCKED = out Bool ()
  val RST = in Bool ()
  val PWRDWN = in Bool ()
}

object IBUFG {
  def on(that: Bool): Bool = {
    val ibufg = IBUFG()
    ibufg.setCompositeName(that, "IBUFG")
    ibufg.I := that
    ibufg.O
  }
}

case class IBUFG() extends BlackBox {
  val I = in Bool()
  val O = out Bool()
}
