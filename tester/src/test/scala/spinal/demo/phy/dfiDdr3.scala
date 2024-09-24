package spinal.demo.phy

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.memory.sdram.dfi._
import spinal.lib.memory.sdram.dfi.foundation.BmbAdapter
import spinal.lib.memory.sdram.dfi.interface._

class DDR3IO(config: DfiConfig) extends Bundle {

  import config._

  val ck_p_o = out Bits (chipSelectNumber bits)
  val ck_n_o = out Bits (chipSelectNumber bits)
  val cke_o = out Bits (chipSelectNumber bits)
  val reset_n_o = out Bits (chipSelectNumber bits)
  val ras_n_o = out Bits (chipSelectNumber bits)
  val cas_n_o = out Bits (chipSelectNumber bits)
  val we_n_o = out Bits (chipSelectNumber bits)
  val cs_n_o = out Bits (chipSelectNumber bits)
  val ba_o = out Bits (bankWidth * chipSelectNumber bits)
  val addr_o = out Bits (addressWidth * chipSelectNumber bits)
  val odt_o = out Bits (chipSelectNumber bits)
  val dm_o = out Bits (config.sdram.bytePerWord * chipSelectNumber bits)
  val dqs_p_io = inout(Analog(Bits(2 * chipSelectNumber bits)))
  val dqs_n_io = inout(Analog(Bits(2 * chipSelectNumber bits)))
  val dq_io = inout(Analog(Bits(config.sdram.dataWidth * chipSelectNumber bits)))
}

case class bmb_dfi_ddr3(config: DfiConfig, configDfi: DfiConfig) extends Component {
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
    val dfiController = DfiController(tp, bmbp, configDfi)
    dfiController.io.bmb <> io.bmb
  }

  if (config.frequencyRatio == 1) {
    val ddr3Chips = for (i <- 0 until (chipSelectNumber)) yield new Area {
      val sel = i
      val phy = dfi_phy_ddr3(tpa)
    }

    ddr3Chips.map(_.phy.io.dfi.read.rd(0).rddataValid).orR <> clockArea.dfiController.io.dfi.read.rd(0).rddataValid
    ddr3Chips.map(_.phy.io.dfi.read.rd(0).rddata).reduceBalancedTree(_ | _) <> clockArea.dfiController.io.dfi.read
      .rd(0)
      .rddata

    val rst = ~ClockDomain.readResetWire
    val adapter = for (ddr3Chip <- ddr3Chips) yield new Area {
      ddr3Chip.phy.io.clk.i <> io.clk1
      ddr3Chip.phy.io.clk.ddr_i <> io.clk2
      ddr3Chip.phy.io.clk.ddr90_i <> io.clk3
      ddr3Chip.phy.io.clk.ref_i <> io.clk4
      ddr3Chip.phy.io.rst_i := rst

      ddr3Chip.phy.io.dfi.control.cke(0) <> clockArea.dfiController.io.dfi.control.cke(ddr3Chip.sel)
      ddr3Chip.phy.io.dfi.control.odt.clearAll()
      ddr3Chip.phy.io.dfi.control.resetN.setAll()
      ddr3Chip.phy.io.dfi.control.csN(0) <> clockArea.dfiController.io.dfi.control.csN(ddr3Chip.sel)
      ddr3Chip.phy.io.dfi.control.assignUnassignedByName(clockArea.dfiController.io.dfi.control)

      ddr3Chip.phy.io.dfi.write.wr.assignUnassignedByName(clockArea.dfiController.io.dfi.write.wr)

      ddr3Chip.phy.io.dfi.read.rden <> clockArea.dfiController.io.dfi.read.rden

      ddr3Chip.phy.io.ddr3.ck_p_o <> io.ddr3.ck_p_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.ck_n_o <> io.ddr3.ck_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.cke_o <> io.ddr3.cke_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.reset_n_o <> io.ddr3.reset_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.ras_n_o <> io.ddr3.ras_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.cas_n_o <> io.ddr3.cas_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.we_n_o <> io.ddr3.we_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.cs_n_o <> io.ddr3.cs_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.ba_o <> io.ddr3.ba_o(bankWidth * ddr3Chip.sel, bankWidth bits)
      ddr3Chip.phy.io.ddr3.addr_o <> io.ddr3.addr_o(addressWidth * ddr3Chip.sel, addressWidth bits)
      ddr3Chip.phy.io.ddr3.odt_o <> io.ddr3.odt_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.dm_o <> io.ddr3.dm_o(config.sdram.bytePerWord * ddr3Chip.sel, config.sdram.bytePerWord bits)
      ddr3Chip.phy.io.ddr3.dqs_p_io <> io.ddr3.dqs_p_io(2 * ddr3Chip.sel, 2 bits)
      ddr3Chip.phy.io.ddr3.dqs_n_io <> io.ddr3.dqs_n_io(2 * ddr3Chip.sel, 2 bits)
      ddr3Chip.phy.io.ddr3.dq_io <> io.ddr3.dq_io(config.sdram.dataWidth * ddr3Chip.sel, config.sdram.dataWidth bits)
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

case class dfi_ddr3() extends Component {
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
  val configDfi: DfiConfig = DfiConfig(
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
    addressWidth = sdram.byteAddressWidth + log2Up(config.chipSelectNumber),
    dataWidth = config.beatWidth,
    sourceWidth = 1,
    contextWidth = 2,
    lengthWidth = 6,
    alignment = BmbParameter.BurstAlignement.WORD
  )

  val io = new Bundle {
    val clk = in Bool ()
    val rst_n = in Bool ()
    val ddr3 = new DDR3IO(config)
  }
  noIoPrefix()
  val bmbClockDomainCfg = ClockDomainConfig(resetActiveLevel = LOW)
  val myClockDomain = ClockDomain.internal("work", bmbClockDomainCfg)

  val pll = pllClk()
  pll.io.clk.in1 <> io.clk
  pll.io.reset <> ~io.rst_n
  val rst_n = io.rst_n & pll.io.locked
  myClockDomain.clock := pll.io.clk.out1
  myClockDomain.reset := rst_n

  val topClockingArea = new ClockingArea(myClockDomain) {
    val bmb_cmd = BmbCmdOp(bmbp, config)

    val bmbddr = bmb_dfi_ddr3(config, configDfi)
    bmbddr.io.clk1 <> pll.io.clk.out1
    bmbddr.io.clk2 <> pll.io.clk.out2
    bmbddr.io.clk3 <> pll.io.clk.out3
    bmbddr.io.clk4 <> pll.io.clk.out4
    bmbddr.io.bmb <> bmb_cmd.io.bmb
    bmbddr.io.ddr3 <> io.ddr3
    bmbddr.io.initDone <> bmb_cmd.io.initDone
  }
}

object bmb_dfi_ddr3 extends App {
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
  val configDfi: DfiConfig = DfiConfig(
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
  val ver = SpinalConfig().generateVerilog(bmb_dfi_ddr3(config, configDfi))
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

object dfi_ddr3 extends App {
  val ver = SpinalConfig().generateVerilog(dfi_ddr3())
  ver.mergeRTLSource("ddr3_dfi_phy")
}
