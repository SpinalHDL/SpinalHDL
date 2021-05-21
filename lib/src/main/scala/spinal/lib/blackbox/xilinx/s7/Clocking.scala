package spinal.lib.blackbox.xilinx.s7

import spinal.core._
import spinal.core.fiber.Handle

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

  val CLKIN1 = in Bool
  val CLKFBIN = in Bool
  val CLKFBOUT = out Bool

  val RST = in Bool
  val LOCKED = out Bool

  val CLKOUT0  = out Bool
  val CLKOUT1  = out Bool
  val CLKOUT2  = out Bool
  val CLKOUT3  = out Bool
  val CLKOUT4  = out Bool
  val CLKOUT5  = out Bool
  val PWRDWN = in Bool
}
