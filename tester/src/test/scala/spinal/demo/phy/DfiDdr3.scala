package spinal.demo.phy

import spinal.core._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.fsm._
import spinal.lib.memory.sdram.dfi._
import spinal.lib.{MuxOH, _}

class DDR3IO(ddrIoDfiConfig: DfiConfig) extends Bundle {

  import ddrIoDfiConfig._

  val ckP = out Bits (chipSelectNumber bits)
  val ckN = out Bits (chipSelectNumber bits)
  val cke = out Bits (chipSelectNumber bits)
  val resetN = out Bits (1 bits)
  val rasN = out Bits (1 bits)
  val casN = out Bits (1 bits)
  val weN = out Bits (1 bits)
  val csN = out Bits (chipSelectNumber bits)
  val ba = out Bits (bankWidth bits)
  val addr = out Bits (addressWidth bits)
  val odt = out Bits (chipSelectNumber bits)
  val dm = out Bits (ddrIoDfiConfig.sdram.bytePerWord bits)
  val dqsP = inout(Analog(Bits(4 * chipSelectNumber bits)))
  val dqsN = inout(Analog(Bits(4 * chipSelectNumber bits)))
  val dq = inout(Analog(Bits(ddrIoDfiConfig.sdram.dataWidth bits)))
}

case class BmbDfiDdr3(bmbp: BmbParameter, ddrIoDfiConfig: DfiConfig, dfiConfig: DfiConfig) extends Component {
  val task: TaskParameter =
    TaskParameter(bytePerTaskMax = 128, timingWidth = 5, refWidth = 23, cmdBufferSize = 64, dataBufferSize = 64, rspBufferSize = 64)

  val phyDfiConfig: DfiConfig = DfiConfig(
    chipSelectNumber = 1,
    dataSlice = ddrIoDfiConfig.dataSlice,
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
    val dfiController = DfiController(bmbp, task, dfiConfig, RowBankColumn)
    dfiController.io.bmb <> io.bmb.pipelined(cmdValid = true, rspValid = true, cmdReady = true, rspReady = true)
  }

  if (ddrIoDfiConfig.frequencyRatio == 1) {
    val phy = DfiPhyDdr3(taskConfig, ddrIoDfiConfig).setName(s"ddr3_dfi_phy")
    val rst = ~ClockDomain.readResetWire
    phy.io.clk.work <> io.clk1
    phy.io.clk.ddr <> io.clk2
    phy.io.clk.ddr90 <> io.clk3
    phy.io.clk.ref <> io.clk4
    phy.io.rst := rst
    phy.io.initDone <> io.initDone

    phy.io.dfi.control.cke <> clockArea.dfiController.io.dfi.control.cke
    phy.io.dfi.control.csN <> clockArea.dfiController.io.dfi.control.csN
    phy.io.dfi.control.odt.clearAll()
    phy.io.dfi.control.resetN.setAll()
    phy.io.dfi.control.assignUnassignedByName(clockArea.dfiController.io.dfi.control)

    phy.io.dfi.write.wr.assignUnassignedByName(clockArea.dfiController.io.dfi.write.wr)

    phy.io.dfi.read.rden <> clockArea.dfiController.io.dfi.read.rden
    phy.io.dfi.read.rd(0).rddataValid <> clockArea.dfiController.io.dfi.read.rd(0).rddataValid
    phy.io.dfi.read.rd(0).rddata <> clockArea.dfiController.io.dfi.read.rd(0).rddata


    phy.io.ddr3.ckP <> io.ddr3.ckP
    phy.io.ddr3.ckN <> io.ddr3.ckN
    phy.io.ddr3.cke <> io.ddr3.cke
    phy.io.ddr3.resetN <> io.ddr3.resetN
    phy.io.ddr3.rasN <> io.ddr3.rasN
    phy.io.ddr3.casN <> io.ddr3.casN
    phy.io.ddr3.weN <> io.ddr3.weN
    phy.io.ddr3.csN <> io.ddr3.csN
    phy.io.ddr3.ba <> io.ddr3.ba
    phy.io.ddr3.addr <> io.ddr3.addr
    phy.io.ddr3.odt <> io.ddr3.odt
    phy.io.ddr3.dm <> io.ddr3.dm
    phy.io.ddr3.dqsP <> io.ddr3.dqsP
    phy.io.ddr3.dqsN <> io.ddr3.dqsN
    phy.io.ddr3.dq <> io.ddr3.dq

  }

}

case class BmbCmdOp(bmbp: BmbParameter, ddrIoDfiConfig: DfiConfig) extends Component {

  val io = new Bundle {
    val bmb = master(Bmb(bmbp))
    val initDone = in Bool ()
  }
  val counter = Reg(UInt(io.bmb.cmd.length.getWidth + 1 bits)).init(0)
  val idleTimer = RegInit(U(0, 10 bits))
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
  io.bmb.rsp.ready := True

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
            when(io.bmb.cmd.ready) {
              counter := counter - 1
              io.bmb.cmd.valid.set()
              io.bmb.cmd.address := address
              io.bmb.cmd.length := U(length * ddrIoDfiConfig.bytePerBeat - 1).resized
              io.bmb.cmd.opcode := True.asBits
            } otherwise {
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
          writeData := initdata.asUInt.resized
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
      write(task1, task2, B"32'h00000001", 64, 128) // min.length is 4
      write(task2, task3, B"32'h77359400", 64, 131584)
      read(task3, task4, 256, 128)
      read(task4, task1, 256, 131584)
      write(task5, task6, B"32'h00000001", 64, 1024)
      write(task6, task7, B"32'h77359400", 64, 37470400)
      read(task7, task8, 256, 1024)
      read(task8, task7, 256, 37470400)
    }
  }

}

case class DfiDdr3() extends Component {
  val sdramtime = SdramTiming(
    generation = 3,
    RFC = 260,
    RAS = 35,
    RP = 15,
    RCD = 15,
    WTR = 8,
    WTP = 0,
    RTP = 8,
    RRD = 8,
    REF = 64000,
    FAW = 35
  )
  val sdram = SdramConfig(
    SdramGeneration.DDR3,
    bgWidth = 0,
    cidWidth = 0,
    bankWidth = 3,
    columnWidth = 10,
    rowWidth = 16,
    dataWidth = 64,
    ddrMHZ = 100,
    ddrWrLat = 6,
    ddrRdLat = 6,
    sdramtime = sdramtime
  )
  val timeConfig = DfiTimeConfig(
    frequencyRatio = 1,
    cmdPhase = 0,
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
    chipSelectNumber = 2,
    dataSlice = 1,
    signalConfig = new DDR3SignalConfig(DfiFunctionConfig(), false) {
      override val useWrdataCsN = false
      override val useRddataCsN = false
    },
    timeConfig = timeConfig,
    sdram = sdram
  )
  val ddrIoDfiConfig: DfiConfig = DfiConfig(
    chipSelectNumber = dfiConfig.chipSelectNumber,
    dataSlice = dfiConfig.dataSlice,
    signalConfig = {
      val signalConfig = new DDR3SignalConfig(DfiFunctionConfig(), false) {
        override val useWrdataCsN = false
        override val useRddataCsN = false
        override val useOdt: Boolean = true
        override val useResetN: Boolean = true
        override val useRddataDnv = true
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
    val clk_p = in Bool ()
    val clk_n = in Bool ()
    val rstN = in Bool ()
    val ddr3 = new DDR3IO(ddrIoDfiConfig)
//    val key = in Bool ()
    val initComplete = out Bool()
  }
  noIoPrefix()
  io.flatten.foreach(bt => {
    bt.setName(bt.getName().replace("P", "_p").replace("N", "_n"))
  })
  val bmbClockDomainCfg = ClockDomainConfig(resetActiveLevel = LOW)
  val myClockDomain = ClockDomain.internal("work", bmbClockDomainCfg)
  case class IBUFDS() extends BlackBox {
    val io = new Bundle{
      val I = in Bool()
      val IB = in Bool()
      val O = out Bool()
    }
    noIoPrefix()
  }

  val clk = Bool()
  val ibufds = IBUFDS()
  ibufds.io.I := io.clk_p
  ibufds.io.IB := io.clk_n
  clk := ibufds.io.O

  val pll = pll_clk()
  pll.io.clk.in1 <> clk
  pll.io.resetn <> io.rstN
  val rstN = io.rstN & pll.io.locked
  myClockDomain.clock := pll.io.clk.out1
  myClockDomain.reset := rstN

  val topClockingArea = new ClockingArea(myClockDomain) {
//    val start = RegInit(False).setWhen(!io.key)
    val start = True
    val bmbCmdOp = BmbCmdOp(bmbp, ddrIoDfiConfig)

    val bmbddr = BmbDfiDdr3(bmbp, ddrIoDfiConfig, dfiConfig)
    bmbddr.io.clk1 <> pll.io.clk.out1
    bmbddr.io.clk2 <> pll.io.clk.out2
    bmbddr.io.clk3 <> pll.io.clk.out3
    bmbddr.io.clk4 <> pll.io.clk.out4
    bmbddr.io.bmb <> bmbCmdOp.io.bmb
    bmbddr.io.ddr3 <> io.ddr3

    io.initComplete := bmbddr.io.initDone.pull()

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
    SdramGeneration.DDR3,
    bgWidth = 0,
    cidWidth = 0,
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
    frequencyRatio = 1,
    cmdPhase = 0,
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
    chipSelectNumber = 1,
    dataSlice = 1,
    signalConfig = new DDR3SignalConfig(DfiFunctionConfig(), false) {
      override val useWrdataCsN = false
      override val useRddataCsN = false
    },
    timeConfig = timeConfig,
    sdram = sdram
  )
  val ddrIoDfiConfig: DfiConfig = DfiConfig(
    chipSelectNumber = dfiConfig.chipSelectNumber,
    dataSlice = dfiConfig.dataSlice,
    signalConfig = {
      val signalConfig = new DDR3SignalConfig(DfiFunctionConfig(), false) {
        override val useWrdataCsN = false
        override val useRddataCsN = false
        override val useOdt: Boolean = true
        override val useResetN: Boolean = true
        override val useRddataDnv = true
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
    SdramGeneration.DDR3,
    bgWidth = 0,
    cidWidth = 0,
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
    frequencyRatio = 1,
    cmdPhase = 0,
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
    chipSelectNumber = 1,
    dataSlice = 1,
    signalConfig = new DDR3SignalConfig(DfiFunctionConfig(), false) {
      override val useWrdataCsN = false
      override val useRddataCsN = false
    },
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
