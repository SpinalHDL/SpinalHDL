package spinal.lib.blackbox.xilinx.s7
import spinal.core._
import spinal.lib.History

case class OSERDESE2(DATA_RATE_OQ : String = "DDR",
                     DATA_RATE_TQ : String = "DDR",
                     DATA_WIDTH : Int = 4,
                     SERDES_MODE : String = "MASTER",
                     TRISTATE_WIDTH : Int = 4
                     ) extends BlackBox {


  addGeneric("DATA_RATE_OQ", DATA_RATE_OQ )
  addGeneric("DATA_RATE_TQ", DATA_RATE_TQ )
  addGeneric("DATA_WIDTH", DATA_WIDTH )
  addGeneric("SERDES_MODE", SERDES_MODE )
  addGeneric("TRISTATE_WIDTH", TRISTATE_WIDTH )
  val CLK = in Bool()
  val CLKDIV = in Bool()
  val D1 = in Bool()
  val D2 = in.Bool()
  val D3 = in.Bool()
  val D4 = in.Bool()
  val D5 = in.Bool()
  val D6 = in.Bool()
  val D7 = in.Bool()
  val D8 = in.Bool()
  val T1 = in Bool()
  val T2 = in.Bool()
  val T3 = in.Bool()
  val T4 = in.Bool()
  val TCE = in Bool()
  val OCE = in Bool()
  val TBYTEIN = in Bool()  default(False)
  val RST = in Bool()
  val SHIFTIN1 = in Bool() default(False)
  val SHIFTIN2 = in Bool()  default(False)
  val OQ = out Bool()
  val OFB = out Bool()
  val TQ = out Bool()
  val TFB = out Bool()
  val TBYTEOUT = out Bool()
  val SHIFTOUT1 = out Bool()
  val SHIFTOUT2 = out Bool()

  def D(i : Int) = i match {
    case 0 => D1
    case 1 => D2
    case 2 => D3
    case 3 => D4
    case 4 => D5
    case 5 => D6
    case 6 => D7
    case 7 => D8
  }

  def T(i : Int) = i match {
    case 0 => T1
    case 1 => T2
    case 2 => T3
    case 3 => T4
  }

  val counter = ClockDomain(CLK, RST) (Reg(UInt(1 bits)) init(0))
  counter := counter + 1

  val sel = counter @@ CLK
  val dReg = ClockDomain(CLKDIV)(RegNext(Vec(D1, D2, D3, D4)))
  val tReg = ClockDomain(CLKDIV)(RegNext(Vec(T1, T2, T3, T4)))

  val dReg2 = ClockDomain(CLK)(dReg)
  val tReg2 = ClockDomain(CLK)(tReg)

  OQ := dReg2(sel)
  TQ := tReg2(sel)

  OFB := False
  TFB := False
  TBYTEOUT  := False
  SHIFTOUT1 := False
  SHIFTOUT2 := False
}



case class ODELAYE2(ODELAY_TYPE : String = "FIXED",
                    ODELAY_VALUE : Int = 0,
                    HIGH_PERFORMANCE_MODE : Boolean = false,
                    SIGNAL_PATTERN : String = "DATA",
                    REFCLK_FREQUENCY : Double = 200,
                    CINVCTRL_SEL : Boolean = false,
                    PIPE_SEL : Boolean = false,
                    DELAY_SRC : String = "ODATAIN") extends BlackBox{

  addGeneric("ODELAY_TYPE", ODELAY_TYPE)
  addGeneric("ODELAY_VALUE", ODELAY_VALUE)
  addGeneric("HIGH_PERFORMANCE_MODE", if(HIGH_PERFORMANCE_MODE) "TRUE" else "FALSE")
  addGeneric("SIGNAL_PATTERN", SIGNAL_PATTERN)
  addGeneric("REFCLK_FREQUENCY", REFCLK_FREQUENCY)
  addGeneric("CINVCTRL_SEL", if(CINVCTRL_SEL) "TRUE" else "FALSE")
  addGeneric("PIPE_SEL", if(PIPE_SEL) "TRUE" else "FALSE")
  addGeneric("DELAY_SRC", DELAY_SRC)


  val C = in Bool()
  val REGRST = in Bool()
  val LD = in Bool()
  val CE = in Bool()
  val INC = in Bool()
  val CINVCTRL = in Bool()
  val CLKIN = in Bool()
  val ODATAIN = in Bool()
  val LDPIPEEN = in Bool()
  val CNTVALUEOUT = in Bits(5 bits)
  val CNTVALUEIN = out Bits(5 bits)
  val DATAOUT = out Bool()
}


case class OBUFDS() extends BlackBox{
  val I = in Bool()
  val O, OB = out Bool()
  O := I
  OB := !I
}

case class IOBUFDS() extends BlackBox{
  val I, T = in Bool()
  val O = out Bool()
  val IO, IOB = inout(Analog(Bool))
}


case class IOBUF() extends BlackBox{
  val I, T = in Bool()
  val O = out Bool()
  val IO = inout(Analog(Bool))

  when(T){
    IO := I
  }
  O := IO
}




case class ISERDESE2(DATA_RATE : String = "DDR",
                     DATA_WIDTH : Int = 4,
                     INTERFACE_TYPE : String = "MEMORY",
                     IOBDELAY : String = "NONE"
                    ) extends BlackBox {


  addGeneric("DATA_RATE", DATA_RATE  )
  addGeneric("DATA_WIDTH", DATA_WIDTH  )
  addGeneric("INTERFACE_TYPE", INTERFACE_TYPE  )
  addGeneric("IOBDELAY", IOBDELAY  )


  val BITSLIP = in Bool()
  val CE1 = in Bool()
  val CE2 = in Bool()
  val CLK = in Bool()
  val CLKB = in Bool()
  val CLKDIV = in Bool()
  val CLKDIVP = in Bool()
  val D = in Bool()
  val DDLY = in Bool()
  val DYNCLKDIVSEL = in Bool()
  val DYNCLKSEL = in Bool()
  val OCLK = in Bool()
  val OCLKB = in Bool()
  val OFB = in Bool()
  val RST = in Bool()
  val SHIFTIN1 = in Bool()
  val SHIFTIN2 = in Bool()

  val O = out Bool()
  val Q1 = out Bool()
  val Q2 = out Bool()
  val Q3 = out Bool()
  val Q4 = out Bool()
  val Q5 = out Bool()
  val Q6 = out Bool()
  val Q7 = out Bool()
  val Q8 = out Bool()
  val SHIFTOUT1 = out Bool()
  val SHIFTOUT2 = out Bool()


  def Q(i : Int) = i match {
    case 0 => Q1
    case 1 => Q2
    case 2 => Q3
    case 3 => Q4
    case 4 => Q5
    case 5 => Q6
    case 6 => Q7
    case 7 => Q8
  }



  val rShift = ClockDomain(CLK)(History(DDLY, 1 to 4))
  val fShift = ClockDomain(CLK, config = ClockDomainConfig(clockEdge = FALLING))(History(DDLY, 1 to 4))
  val fastData = (0 to 3).flatMap(i => List(rShift(i), fShift(i)))
  val slowData = ClockDomain(CLKDIV)(RegNext(Vec(fastData)))
  for(i <- 0 to 7){
    Q(i) := slowData(7-i)
  }
}



case class IDELAYE2(HIGH_PERFORMANCE_MODE : Boolean = false,
                    IDELAY_TYPE : String = "FIXED",
                    IDELAY_VALUE : Int = 0,
                    PIPE_SEL : Boolean = false,
                    REFCLK_FREQUENCY : Double = 200.0,
                    SIGNAL_PATTERN : String = "DATA"
                   ) extends BlackBox{
  addGeneric("HIGH_PERFORMANCE_MODE", if(HIGH_PERFORMANCE_MODE) "TRUE" else "FALSE")
  addGeneric("IDELAY_TYPE", IDELAY_TYPE)
  addGeneric("IDELAY_VALUE", IDELAY_VALUE)
  addGeneric("PIPE_SEL", if(PIPE_SEL) "TRUE" else "FALSE")
  addGeneric("REFCLK_FREQUENCY", REFCLK_FREQUENCY)
  addGeneric("SIGNAL_PATTERN", SIGNAL_PATTERN)

  val CNTVALUEOUT = out Bits(5 bits)
  val DATAOUT = out Bool()

  val C = in Bool()
  val CE = in Bool()
  val CINVCTRL = in Bool()
  val CNTVALUEIN = in Bits(5 bits)
  val DATAIN = in Bool()
  val IDATAIN = in Bool()
  val INC = in Bool()
  val LD = in Bool()
  val LDPIPEEN = in Bool()
  val REGRST = in Bool()


  DATAOUT := IDATAIN
}


case class IDELAYCTRL() extends BlackBox {
  val REFCLK = in Bool()
  val RST = in Bool()
  val RDY = out Bool()

  RDY := True
}