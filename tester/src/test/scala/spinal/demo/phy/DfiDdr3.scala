package spinal.demo.phy

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.memory.sdram.dfi._
import spinal.lib.memory.sdram.dfi.foundation.BmbAdapter
import spinal.lib.memory.sdram.dfi.interface._

class DDR3IO(config: DfiConfig) extends Bundle {

  import config._

  val ckP = out Bits (chipSelectNumber bits)
  val ckN = out Bits (chipSelectNumber bits)
  val cke = out Bits (chipSelectNumber bits)
  val resetN = out Bits (chipSelectNumber bits)
  val rasN = out Bits (chipSelectNumber bits)
  val casN = out Bits (chipSelectNumber bits)
  val weN = out Bits (chipSelectNumber bits)
  val csN = out Bits (chipSelectNumber bits)
  val ba = out Bits (bankWidth * chipSelectNumber bits)
  val addr = out Bits (addressWidth * chipSelectNumber bits)
  val odt = out Bits (chipSelectNumber bits)
  val dm = out Bits (config.sdram.bytePerWord * chipSelectNumber bits)
  val dqsP = inout(Analog(Bits(2 * chipSelectNumber bits)))
  val dqsN = inout(Analog(Bits(2 * chipSelectNumber bits)))
  val dq = inout(Analog(Bits(config.sdram.dataWidth * chipSelectNumber bits)))
}

case class BmbDfiDdr3(config: DfiConfig, dfiConfig: DfiConfig) extends Component {
  val tp: TaskParameter =
    TaskParameter(timingWidth = 5, refWidth = 23, cmdBufferSize = 64, dataBufferSize = 64, rspBufferSize = 64)
  val bmbp: BmbParameter = BmbParameter(
    addressWidth = config.sdram.byteAddressWidth + log2Up(config.chipSelectNumber),
    dataWidth = config.beatWidth,
    sourceWidth = 1,
    contextWidth = 2,
    lengthWidth = 6,
    alignment = BmbParameter.BurstAlignement.WORD
  )

  import config._

  val io = new Bundle {
    val clk1 = in Bool ()
    val clk2 = in Bool ()
    val clk3 = in Bool ()
    val clk4 = in Bool ()
    val bmb = slave(Bmb(bmbp))
    val ddr3 = new DDR3IO(config)
    val initDone = out Bool ()
  }
  val tpa = TaskParameterAggregate(tp, BmbAdapter.taskPortParameter(bmbp, config, tp), config)
  val clockArea = new ClockingArea(ClockDomain.current) {
    val dfiController = DfiController(tp, bmbp, dfiConfig)
    dfiController.io.bmb <> io.bmb
  }

  if (config.frequencyRatio == 1) {
    val ddr3Chips = for (i <- 0 until (chipSelectNumber)) yield new Area {
      val sel = i
      val phy = DfiPhyDdr3(tpa)
    }

    ddr3Chips.map(_.phy.io.dfi.read.rd(0).rddataValid).orR <> clockArea.dfiController.io.dfi.read.rd(0).rddataValid
    ddr3Chips.map(_.phy.io.dfi.read.rd(0).rddata).reduceBalancedTree(_ | _) <> clockArea.dfiController.io.dfi.read
      .rd(0)
      .rddata

    val rst = ~ClockDomain.readResetWire
    val adapter = for (ddr3Chip <- ddr3Chips) yield new Area {
      ddr3Chip.phy.io.clk.work <> io.clk1
      ddr3Chip.phy.io.clk.ddr <> io.clk2
      ddr3Chip.phy.io.clk.ddr90 <> io.clk3
      ddr3Chip.phy.io.clk.ref <> io.clk4
      ddr3Chip.phy.io.rst := rst

      ddr3Chip.phy.io.dfi.control.cke(0) <> clockArea.dfiController.io.dfi.control.cke(ddr3Chip.sel)
      ddr3Chip.phy.io.dfi.control.odt.clearAll()
      ddr3Chip.phy.io.dfi.control.resetN.setAll()
      ddr3Chip.phy.io.dfi.control.csN(0) <> clockArea.dfiController.io.dfi.control.csN(ddr3Chip.sel)
      ddr3Chip.phy.io.dfi.control.assignUnassignedByName(clockArea.dfiController.io.dfi.control)

      ddr3Chip.phy.io.dfi.write.wr.assignUnassignedByName(clockArea.dfiController.io.dfi.write.wr)

      ddr3Chip.phy.io.dfi.read.rden <> clockArea.dfiController.io.dfi.read.rden

      ddr3Chip.phy.io.ddr3.ckP.asBool <> io.ddr3.ckP(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.ckN.asBool <> io.ddr3.ckN(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.cke.asBool <> io.ddr3.cke(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.resetN.asBool <> io.ddr3.resetN(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.rasN.asBool <> io.ddr3.rasN(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.casN.asBool <> io.ddr3.casN(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.weN.asBool <> io.ddr3.weN(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.csN.asBool <> io.ddr3.csN(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.ba <> io.ddr3.ba(bankWidth * ddr3Chip.sel, bankWidth bits)
      ddr3Chip.phy.io.ddr3.addr <> io.ddr3.addr(addressWidth * ddr3Chip.sel, addressWidth bits)
      ddr3Chip.phy.io.ddr3.odt.asBool <> io.ddr3.odt(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.dm <> io.ddr3.dm(config.sdram.bytePerWord * ddr3Chip.sel, config.sdram.bytePerWord bits)
      ddr3Chip.phy.io.ddr3.dqsP <> io.ddr3.dqsP(2 * ddr3Chip.sel, 2 bits)
      ddr3Chip.phy.io.ddr3.dqsN <> io.ddr3.dqsN(2 * ddr3Chip.sel, 2 bits)
      ddr3Chip.phy.io.ddr3.dq <> io.ddr3.dq(config.sdram.dataWidth * ddr3Chip.sel, config.sdram.dataWidth bits)
      ddr3Chip.phy.io.initDone <> io.initDone
    }
  }

}

case class BmbCmdOp(bmbp: BmbParameter, config: DfiConfig) extends Component {

  val io = new Bundle {
    val bmb = master(Bmb(bmbp))
    val initDone = in Bool ()
  }
  val counter = Reg(UInt(io.bmb.cmd.length.getWidth + 1 bits)).init(0)
  val opcodeCount = RegInit(U(0, 2 bits))
  val idleTimer = RegInit(U(0, 4 bits))
  val start = RegInit(False).setWhen(io.initDone)

  def write(initdata: UInt, length: Int, address: Int) = {
    when(idleTimer === 0) {
      counter := U(length, log2Up(length) + 1 bits).resized
    }
    when(counter =/= 0) {
      counter := counter - 1
      io.bmb.cmd.valid.set()
      io.bmb.cmd.address := address
      io.bmb.cmd.length := U(length * config.bytePerBeat - 1).resized
      io.bmb.cmd.opcode := True.asBits
    }

    io.bmb.cmd.data := initdata.asBits.rotateLeft((U(length) - counter) >> 2).resized
    when(counter === 1) {
      io.bmb.cmd.last.set()
    }
    println("write command")
  }

  def read(length: Int, address: Int): Unit = {
    io.bmb.cmd.address := address
    io.bmb.cmd.length := U(length * config.bytePerBeat - 1).resized
    io.bmb.cmd.opcode := False.asBits
    io.bmb.cmd.last.set()
    io.bmb.cmd.valid.set()
  } //  val start = True

  io.bmb.cmd.valid.clear()
  io.bmb.cmd.last.clear()
  io.bmb.cmd.source.clearAll()
  io.bmb.cmd.opcode.setAll()
  io.bmb.cmd.address.clearAll()
  io.bmb.cmd.length.clearAll()
  io.bmb.cmd.data.clearAll()
  io.bmb.cmd.mask.clearAll()
  io.bmb.cmd.context.clearAll()
  io.bmb.rsp.ready := RegInit(False).setWhen(io.bmb.cmd.valid && io.bmb.cmd.opcode === 0).clearWhen(io.bmb.rsp.last)

  when(idleTimer =/= 0) {
    idleTimer := idleTimer - 1
  }
  when(idleTimer === 1 || io.bmb.cmd.opcode === 0) {
    when(opcodeCount < opcodeCount.maxValue) {
      opcodeCount := opcodeCount + 1
    }
  }
  when(io.bmb.cmd.last) {
    idleTimer := 10
  }
  when(start) {
    switch(opcodeCount) {
      is(0) {
        write(U"32'h9FD24589", 32, 128)
      }
      is(1) {
        write(U"32'h75BC2457", 32, 1024)
      }
      is(2) {
        read(32, 128)
      }
    }
  }

}

case class DfiDdr3() extends Component {
  val sdramtime = SdramTiming(
    generation = 3,
    RFC = 260,
    RAS = 38,
    RP = 15,
    RCD = 15,
    WTR = 8,
    WTP = 0,
    RTP = 8,
    RRD = 6,
    REF = 64000,
    FAW = 35
  )
  val sdram = SdramConfig(
    SdramGeneration.MYDDR,
    bankWidth = 3,
    columnWidth = 10,
    rowWidth = 15,
    dataWidth = 16,
    ddrMHZ = 100,
    ddrWrLat = 4,
    ddrRdLat = 4,
    sdramtime = sdramtime
  )
  val timeConfig = DfiTimeConfig(
    tPhyWrLat = sdram.tPhyWrlat,
    tPhyWrData = 0,
    tPhyWrCsGap = 3,
    tRddataEn = sdram.tRddataEn,
    tPhyRdlat = 4,
    tPhyRdCsGap = 3,
    tPhyRdCslat = 0,
    tPhyWrCsLat = 0
  )
  val dfiConfig: DfiConfig = DfiConfig(
    frequencyRatio = 1,
    transferPerBurst = 8,
    addressWidth = Math.max(sdram.columnWidth, sdram.rowWidth),
    chipSelectNumber = 1,
    bankWidth = sdram.bankWidth,
    bgWidth = 0,
    cidWidth = 0,
    dataSlice = 1,
    cmdPhase = 0,
    signalConfig = new DDRSignalConfig(),
    timeConfig = timeConfig,
    sdram = sdram
  )
  val phyConfig: DfiConfig = DfiConfig(
    frequencyRatio = 1,
    transferPerBurst = 8,
    addressWidth = Math.max(sdram.columnWidth, sdram.rowWidth),
    chipSelectNumber = 1,
    bankWidth = sdram.bankWidth,
    bgWidth = 0,
    cidWidth = 0,
    dataSlice = 1,
    cmdPhase = 0,
    signalConfig = {
      val signalConfig = new DDRSignalConfig() {
        override def useOdt: Boolean = true
        override def useResetN: Boolean = true
        override def useRddataDnv = true
      }
      signalConfig
    },
    timeConfig = timeConfig,
    sdram = sdram
  )
  val bmbp: BmbParameter = BmbParameter(
    addressWidth = sdram.byteAddressWidth + log2Up(phyConfig.chipSelectNumber),
    dataWidth = phyConfig.beatWidth,
    sourceWidth = 1,
    contextWidth = 2,
    lengthWidth = 6,
    alignment = BmbParameter.BurstAlignement.WORD
    )

  val io = new Bundle {
    val clk = in Bool ()
    val rstN = in Bool ()
    val ddr3 = new DDR3IO(phyConfig)
  }
  noIoPrefix()
  val bmbClockDomainCfg = ClockDomainConfig(resetActiveLevel = LOW)
  val myClockDomain = ClockDomain.internal("work", bmbClockDomainCfg)

  val pll = PllClk()
  pll.io.clk.in1 <> io.clk
  pll.io.reset <> ~io.rstN
  val rstN = io.rstN & pll.io.locked
  myClockDomain.clock := pll.io.clk.out1
  myClockDomain.reset := rstN

  val topClockingArea = new ClockingArea(myClockDomain) {
    val bmbCmdOp = BmbCmdOp(bmbp, phyConfig)

    val bmbddr = BmbDfiDdr3(phyConfig, dfiConfig)
    bmbddr.io.clk1 <> pll.io.clk.out1
    bmbddr.io.clk2 <> pll.io.clk.out2
    bmbddr.io.clk3 <> pll.io.clk.out3
    bmbddr.io.clk4 <> pll.io.clk.out4
    bmbddr.io.bmb <> bmbCmdOp.io.bmb
    bmbddr.io.ddr3 <> io.ddr3
    bmbddr.io.initDone <> bmbCmdOp.io.initDone
  }
}

object BmbDfiDdr3 extends App {
  val sdramtime = SdramTiming(
    generation = 3,
    RFC = 260,
    RAS = 38,
    RP = 15,
    RCD = 15,
    WTR = 8,
    WTP = 0,
    RTP = 8,
    RRD = 6,
    REF = 64000,
    FAW = 35
  )
  val sdram = SdramConfig(
    SdramGeneration.MYDDR,
    bankWidth = 3,
    columnWidth = 10,
    rowWidth = 15,
    dataWidth = 16,
    ddrMHZ = 100,
    ddrWrLat = 4,
    ddrRdLat = 4,
    sdramtime = sdramtime
  )
  val timeConfig = DfiTimeConfig(
    tPhyWrLat = sdram.tPhyWrlat,
    tPhyWrData = 0,
    tPhyWrCsGap = 3,
    tRddataEn = sdram.tRddataEn,
    tPhyRdlat = 4,
    tPhyRdCsGap = 3,
    tPhyRdCslat = 0,
    tPhyWrCsLat = 0
  )
  val dfiConfig: DfiConfig = DfiConfig(
    frequencyRatio = 1,
    transferPerBurst = 8,
    addressWidth = Math.max(sdram.columnWidth, sdram.rowWidth),
    chipSelectNumber = 1,
    bankWidth = sdram.bankWidth,
    bgWidth = 0,
    cidWidth = 0,
    dataSlice = 1,
    cmdPhase = 0,
    signalConfig = new DDRSignalConfig(),
    timeConfig = timeConfig,
    sdram = sdram
  )
  val phyConfig: DfiConfig = DfiConfig(
    frequencyRatio = 1,
    transferPerBurst = 8,
    addressWidth = Math.max(sdram.columnWidth, sdram.rowWidth),
    chipSelectNumber = 1,
    bankWidth = sdram.bankWidth,
    bgWidth = 0,
    cidWidth = 0,
    dataSlice = 1,
    cmdPhase = 0,
    signalConfig = {
      val signalConfig = new DDRSignalConfig() {
        override def useOdt: Boolean = true
        override def useResetN: Boolean = true
        override def useRddataDnv = true
      }
      signalConfig
    },
    timeConfig = timeConfig,
    sdram = sdram
  )
  val ver = SpinalConfig().generateVerilog(BmbDfiDdr3(phyConfig, dfiConfig))
  ver.mergeRTLSource("ddr3_dfi_phy")
}

object BmbCmdOp extends App {
  val sdramtime = SdramTiming(
    generation = 3,
    RFC = 260,
    RAS = 38,
    RP = 15,
    RCD = 15,
    WTR = 8,
    WTP = 0,
    RTP = 8,
    RRD = 6,
    REF = 64000,
    FAW = 35
  )
  val sdram = SdramConfig(
    SdramGeneration.MYDDR,
    bankWidth = 3,
    columnWidth = 10,
    rowWidth = 15,
    dataWidth = 16,
    ddrMHZ = 100,
    ddrWrLat = 4,
    ddrRdLat = 4,
    sdramtime = sdramtime
  )
  val timeConfig = DfiTimeConfig(
    tPhyWrLat = sdram.tPhyWrlat,
    tPhyWrData = 0,
    tPhyWrCsGap = 3,
    tRddataEn = sdram.tRddataEn,
    tPhyRdlat = 4,
    tPhyRdCsGap = 3,
    tPhyRdCslat = 0,
    tPhyWrCsLat = 0
  )
  val config: DfiConfig = DfiConfig(
    frequencyRatio = 1,
    transferPerBurst = 8,
    addressWidth = Math.max(sdram.columnWidth, sdram.rowWidth),
    chipSelectNumber = 1,
    bankWidth = sdram.bankWidth,
    bgWidth = 0,
    cidWidth = 0,
    dataSlice = 1,
    cmdPhase = 0,
    signalConfig = new DDRSignalConfig(),
    timeConfig = timeConfig,
    sdram = sdram
  )
  val bmbp: BmbParameter = BmbParameter(
    addressWidth = sdram.byteAddressWidth + log2Up(config.chipSelectNumber),
    dataWidth = config.beatWidth,
    sourceWidth = 1,
    contextWidth = 2,
    lengthWidth = 6,
    alignment = BmbParameter.BurstAlignement.WORD
  )

  SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW))
    .generateVerilog(BmbCmdOp(bmbp, config))

}

object DfiDdr3 extends App {
  val ver = SpinalConfig().generateVerilog(DfiDdr3())
  ver.mergeRTLSource("ddr3_dfi_phy")
}
