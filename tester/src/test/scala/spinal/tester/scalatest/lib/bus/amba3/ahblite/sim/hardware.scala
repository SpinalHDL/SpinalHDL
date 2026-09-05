package spinal.lib.bus.amba3.ahblite.sim.hardware

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._

// Section 4.1.1 Default slave
class DefaultSlave(cfg: AhbLite3Config) extends Component {
  val io = new Bundle {
    val bus = slave(AhbLite3(cfg))
  }

  def OKAY = False
  def ERROR = True

  import io.bus._

  HRDATA := 0
  HREADYOUT := True

  when(HTRANS === AhbLite3.NONSEQ || HTRANS === AhbLite3.SEQ) {
    HRESP := ERROR
  } otherwise {
    HRESP := OKAY
  }
}

class Device(n: Int, aw: Int = 32, dw: Int = 32) extends Component {
  val cfg = AhbLite3Config(aw, dw)
  val io = new Bundle {
    val ahbMaster = slave(AhbLite3Master(cfg))
    val ahbSlaves = Vec(master(AhbLite3(cfg)), n)
  }

  // Section 4.1.1 Default slave
  val defaultSlave = new DefaultSlave(cfg)
  val ds = defaultSlave.io.bus

  // Interface accessors
  import io.ahbMaster._
  def slaves(i: UInt) = (i < n) ? io.ahbSlaves(i) | ds
  val allSlaves = io.ahbSlaves :+ ds

  // Figure 1-1 AHB-Lite block diagram
  for (s <- allSlaves) {
    // addr & control: master -> slaves
    s.HADDR := HADDR
    s.HWRITE := HWRITE
    s.HSIZE := HSIZE
    s.HBURST := HBURST
    s.HPROT := HPROT
    s.HTRANS := HTRANS
    s.HMASTLOCK := HMASTLOCK
    // data: master -> slaves
    s.HWDATA := HWDATA
  }

  // Figure 4-1 Slave select signals
  val decoder = new Area {
    // HADDR: master -> decoder
    val i = (HADDR >> 10) resize log2Up(n)

    // HSEL: decoder -> slaves
    allSlaves foreach { _.HSEL := False }
    slaves(i).HSEL := True
  }

  // Figure 4-2 Multiplexor interconnection
  val mux = new Area {
    // decoder -> mux
    val i = RegNextWhen(decoder.i, HREADY) init 0
    // master <- mux <- slaves
    HREADY := slaves(i).HREADYOUT
    HRESP := slaves(i).HRESP
    HRDATA := slaves(i).HRDATA
    // mux -> slaves
    allSlaves foreach { _.HREADY := HREADY }
  }
}

object GenerateDevice extends App {
  SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = LOW
    )
  ).generateSystemVerilog(new Device(10)).printPruned()
}

class Identity(aw: Int = 32, dw: Int = 32) extends Component {
  val cfg = AhbLite3Config(aw, dw)
  val io = new Bundle {
    val m = slave(AhbLite3(cfg))
    val s = master(AhbLite3(cfg))
  }
  io.m >> io.s
  // Trick to have a clock domain
  val fakeReg = Reg(Bits(0 bits))
}
