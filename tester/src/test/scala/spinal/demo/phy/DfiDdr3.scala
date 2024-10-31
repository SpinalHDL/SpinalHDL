package spinal.demo.phy

import spinal.core._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.fsm._
import spinal.lib.memory.sdram.dfi._
import spinal.lib.memory.sdram.dfi.function.BmbAdapter
import spinal.lib.memory.sdram.dfi.interface._
import spinal.lib.{MuxOH, _}

class DDR3IO(ddrIoDfiConfig: DfiConfig) extends Bundle {

  import ddrIoDfiConfig._

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
  val dm = out Bits (ddrIoDfiConfig.sdram.bytePerWord * chipSelectNumber bits)
  val dqsP = inout(Analog(Bits(2 * chipSelectNumber bits)))
  val dqsN = inout(Analog(Bits(2 * chipSelectNumber bits)))
  val dq = inout(Analog(Bits(ddrIoDfiConfig.sdram.dataWidth * chipSelectNumber bits)))
}

case class BmbDfiDdr3(bmbp: BmbParameter, ddrIoDfiConfig: DfiConfig, dfiConfig: DfiConfig) extends Component {
  val task: TaskParameter =
    TaskParameter(timingWidth = 5, refWidth = 23, cmdBufferSize = 64, dataBufferSize = 64, rspBufferSize = 64)

  import ddrIoDfiConfig._
  val phyDfiConfig: DfiConfig = DfiConfig(
    frequencyRatio = 1,
    chipSelectNumber = 1,
    bgWidth = ddrIoDfiConfig.bgWidth,
    cidWidth = ddrIoDfiConfig.cidWidth,
    dataSlice = ddrIoDfiConfig.dataSlice,
    cmdPhase = ddrIoDfiConfig.cmdPhase,
    signalConfig = ddrIoDfiConfig.signalConfig,
    timeConfig = ddrIoDfiConfig.timeConfig,
    sdram = ddrIoDfiConfig.sdram
  )

  val io = new Bundle {
    val clk1 = in Bool ()
    val clk2 = in Bool ()
    val clk3 = in Bool ()
    val clk4 = in Bool ()
    val bmb = slave(Bmb(bmbp))
    val ddr3 = new DDR3IO(ddrIoDfiConfig)
    val initDone = out Bool ()
  }
  val taskConfig = BmbAdapter.taskConfig(bmbp, ddrIoDfiConfig, task)
  val clockArea = new ClockingArea(ClockDomain.current) {
    val dfiController = DfiController(bmbp, task, dfiConfig)
    dfiController.io.bmb <> io.bmb.pipelined(cmdValid = true, rspValid = true, cmdReady = true, rspReady = true)
  }

  if (frequencyRatio == 1) {
    val ddr3Chips = for (i <- 0 until (chipSelectNumber)) yield new Area {
      val sel = i
      val phy = DfiPhyDdr3(taskConfig, phyDfiConfig).setName(s"ddr3_dfi_phy_${i}")
    }

    ddr3Chips.map(_.phy.io.dfi.read.rd(0).rddataValid).orR <> clockArea.dfiController.io.dfi.read.rd(0).rddataValid
    MuxOH(
      ddr3Chips.map(_.phy.io.dfi.read.rd(0).rddataValid),
      ddr3Chips.map(_.phy.io.dfi.read.rd(0).rddata)
    ) <> clockArea.dfiController.io.dfi.read.rd(0).rddata
    ddr3Chips.map(_.phy.io.initDone).andR <> io.initDone

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
      ddr3Chip.phy.io.ddr3.dm <> io.ddr3
        .dm(ddrIoDfiConfig.sdram.bytePerWord * ddr3Chip.sel, ddrIoDfiConfig.sdram.bytePerWord bits)
      ddr3Chip.phy.io.ddr3.dqsP <> io.ddr3.dqsP(2 * ddr3Chip.sel, 2 bits)
      ddr3Chip.phy.io.ddr3.dqsN <> io.ddr3.dqsN(2 * ddr3Chip.sel, 2 bits)
      ddr3Chip.phy.io.ddr3.dq <> io.ddr3
        .dq(ddrIoDfiConfig.sdram.dataWidth * ddr3Chip.sel, ddrIoDfiConfig.sdram.dataWidth bits)
    }
  }

}

case class BmbCmdOp(bmbp: BmbParameter, ddrIoDfiConfig: DfiConfig) extends Component {

  val io = new Bundle {
    val bmb = master(Bmb(bmbp))
    val initDone = in Bool ()
  }
  val counter = Reg(UInt(io.bmb.cmd.length.getWidth + 1 bits)).init(0)
  val idleTimer = RegInit(U(0, 9 bits))
  val start = RegInit(False).setWhen(io.initDone)
  val writeData = Reg(cloneOf(io.bmb.cmd.data).asUInt).init(0)
  when(io.bmb.cmd.fire) {
    writeData := writeData + 1
  }
  io.bmb.cmd.valid.clear()
  io.bmb.cmd.last.clear()
  io.bmb.cmd.source.clearAll()
  io.bmb.cmd.opcode.setAll()
  io.bmb.cmd.address.clearAll()
  io.bmb.cmd.length.clearAll()
  io.bmb.cmd.data.clearAll()
  io.bmb.cmd.mask.clearAll()
  io.bmb.cmd.context.clearAll()
  io.bmb.rsp.ready := RegInit(False).clearWhen(io.bmb.rsp.lastFire).setWhen(io.bmb.cmd.valid && io.bmb.cmd.opcode === 0)

  when(io.bmb.cmd.lastFire & io.bmb.cmd.isWrite | io.bmb.rsp.lastFire) {
    idleTimer := 500
  } otherwise {
    when(idleTimer =/= 0) {
      idleTimer := idleTimer - 1
    }
  }

  when(start) {
    val fsm = new StateMachine {
      def write(state: State, nextState: State, initdata: Bits, length: Int, address: Int) = {
        state.whenIsActive {
          when(idleTimer === 1) {
            goto(nextState)
          }
          when(counter =/= 0) {
            when(io.bmb.cmd.ready){
              counter := counter - 1
              io.bmb.cmd.valid.set()
              io.bmb.cmd.address := address
              io.bmb.cmd.length := U(length * ddrIoDfiConfig.bytePerBeat - 1).resized
              io.bmb.cmd.opcode := True.asBits
            }otherwise{
              io.bmb.cmd.valid.clear()
            }
          }
          io.bmb.cmd.data := writeData.asBits
          when(counter === 1) {
            io.bmb.cmd.last.set()
          }
//          println("write command")
        }
        state.onEntry {
          counter := U(length, log2Up(length) + 1 bits).resized
          writeData := initdata.asUInt
        }
      }

      def read(state: State, nextState: State, length: Int, address: Int): Unit = {
        state.whenIsActive {
          when(idleTimer === 1) {
            goto(nextState)
          }
          state.onEntry {
            io.bmb.cmd.address := address
            io.bmb.cmd.length := U(length * ddrIoDfiConfig.bytePerBeat - 1).resized
            io.bmb.cmd.opcode := False.asBits
            io.bmb.cmd.last.set()
            io.bmb.cmd.valid.set()
          }
        }
      }
      val idle = new State with EntryPoint
      val task1 = new State
      val task2 = new State
      val task3 = new State
      val task4 = new State
      val task5 = new State
      val task6 = new State
      val task7 = new State
      val task8 = new State
      val end = new State

      idle.whenIsActive(when(start) { goto(task1) })
      write(task1, task2, B"32'h00000001", 256, 128) // min.length is 4
      write(task2, task3, B"32'h77359400", 256, 131584)
      read(task3, task4, 256, 128)
      read(task4, task5, 256, 131584)
      write(task5, task6, B"32'h00000001", 256, 1024)
      write(task6, task7, B"32'h77359400", 256, 308639568)
      read(task7, task8, 256, 1024)
      read(task8, task7, 256, 308639568)
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
    ddrMHZ = 200,
    ddrWrLat = 6,
    ddrRdLat = 6,
    sdramtime = sdramtime
  )
  val timeConfig = DfiTimeConfig(
    tPhyWrLat = sdram.tPhyWrlat,
    tPhyWrData = 0,
    tPhyWrCsGap = 3,
    tRddataEn = sdram.tRddataEn,
    tPhyRdlat = 5,
    tPhyRdCsGap = 3,
    tPhyRdCslat = 0,
    tPhyWrCsLat = 0
  )
  val dfiConfig: DfiConfig = DfiConfig(
    frequencyRatio = 1,
    chipSelectNumber = 1,
    bgWidth = 0,
    cidWidth = 0,
    dataSlice = 1,
    cmdPhase = 0,
    signalConfig = new DDRSignalConfig(),
    timeConfig = timeConfig,
    sdram = sdram
  )
  val ddrIoDfiConfig: DfiConfig = DfiConfig(
    frequencyRatio = dfiConfig.frequencyRatio,
    chipSelectNumber = dfiConfig.chipSelectNumber,
    bgWidth = dfiConfig.bgWidth,
    cidWidth = dfiConfig.cidWidth,
    dataSlice = dfiConfig.dataSlice,
    cmdPhase = dfiConfig.cmdPhase,
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
    addressWidth = sdram.byteAddressWidth + log2Up(ddrIoDfiConfig.chipSelectNumber),
    dataWidth = ddrIoDfiConfig.beatWidth,
    sourceWidth = 0,
    contextWidth = 0,
    lengthWidth = 10,
    alignment = BmbParameter.BurstAlignement.BYTE
  )

  val io = new Bundle {
    val clk = in Bool ()
    val rstN = in Bool ()
    val ddr3 = new DDR3IO(ddrIoDfiConfig)
    val key = in Bool ()
  }
  noIoPrefix()
  val bmbClockDomainCfg = ClockDomainConfig(resetActiveLevel = LOW)
  val myClockDomain = ClockDomain.internal("work", bmbClockDomainCfg)

  val pll = pll_clk()
  pll.io.clk.in1 <> io.clk
  pll.io.resetn <> io.rstN
  val rstN = io.rstN & pll.io.locked
  myClockDomain.clock := pll.io.clk.out1
  myClockDomain.reset := rstN

  val topClockingArea = new ClockingArea(myClockDomain) {
    val start = RegInit(False).setWhen(!io.key)
    val bmbCmdOp = BmbCmdOp(bmbp, ddrIoDfiConfig)

    val bmbddr = BmbDfiDdr3(bmbp, ddrIoDfiConfig, dfiConfig)
    bmbddr.io.clk1 <> pll.io.clk.out1
    bmbddr.io.clk2 <> pll.io.clk.out2
    bmbddr.io.clk3 <> pll.io.clk.out3
    bmbddr.io.clk4 <> pll.io.clk.out4
    bmbddr.io.bmb <> bmbCmdOp.io.bmb
    bmbddr.io.ddr3 <> io.ddr3

    val initDone = bmbddr.io.initDone & start
    bmbCmdOp.io.initDone := initDone
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
    chipSelectNumber = 1,
    bgWidth = 0,
    cidWidth = 0,
    dataSlice = 1,
    cmdPhase = 0,
    signalConfig = new DDRSignalConfig(),
    timeConfig = timeConfig,
    sdram = sdram
  )
  val ddrIoDfiConfig: DfiConfig = DfiConfig(
    frequencyRatio = dfiConfig.frequencyRatio,
    chipSelectNumber = dfiConfig.chipSelectNumber,
    bgWidth = dfiConfig.bgWidth,
    cidWidth = dfiConfig.cidWidth,
    dataSlice = dfiConfig.dataSlice,
    cmdPhase = dfiConfig.cmdPhase,
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
    addressWidth = ddrIoDfiConfig.sdram.byteAddressWidth + log2Up(ddrIoDfiConfig.chipSelectNumber),
    dataWidth = ddrIoDfiConfig.beatWidth,
    sourceWidth = 1,
    contextWidth = 2,
    lengthWidth = 6,
    alignment = BmbParameter.BurstAlignement.WORD
  )
  val ver = SpinalConfig().generateVerilog(BmbDfiDdr3(bmbp, ddrIoDfiConfig, dfiConfig))
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
  val ddrIoDfiConfig: DfiConfig = DfiConfig(
    frequencyRatio = 1,
    chipSelectNumber = 1,
    bgWidth = 0,
    cidWidth = 0,
    dataSlice = 1,
    cmdPhase = 0,
    signalConfig = new DDRSignalConfig(),
    timeConfig = timeConfig,
    sdram = sdram
  )
  val bmbp: BmbParameter = BmbParameter(
    addressWidth = sdram.byteAddressWidth + log2Up(ddrIoDfiConfig.chipSelectNumber),
    dataWidth = ddrIoDfiConfig.beatWidth,
    sourceWidth = 1,
    contextWidth = 2,
    lengthWidth = 6,
    alignment = BmbParameter.BurstAlignement.WORD
  )

  SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW))
    .generateVerilog(BmbCmdOp(bmbp, ddrIoDfiConfig))

}

object DfiDdr3 extends App {
  val ver = SpinalConfig().generateVerilog(DfiDdr3())
  ver.mergeRTLSource("ddr3_dfi_phy")
}
