package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._

case class CAAlignment(config: DfiConfig) extends Component {
  val io = new Bundle {
    val cmd = Vec(slave(Flow(DfiCmd(config))), config.frequencyRatio)
    val address = Vec(slave(Flow(DfiAddr(config))), config.frequencyRatio)
    val cke = in Vec (Bits(config.chipSelectNumber bits), config.frequencyRatio)
    val output = master(DfiControlInterface(config))
  }
  // cke,reserN,odt
  // Most DRAMs define CKE as low at reset; some devices, such as LPDDR1, LPDDR2 and LPDDR3, define CKE as high at
  // reset. The default value should adhere to the DRAM definition.
  io.output.cke := io.cke.asBits
  // In general, the dfi_reset_n signal is defined as low at reset; however, in some cases it may be necessary
  // to hold dfi_reset_n high during initialization.
  if (config.useResetN) io.output.resetN.setAll()
  // The MC generates dfi_odt (dfi_odt_pN in frequency ratio systems) based on the DRAM burst length including
  // CRC data. With CRC enabled, the MC may need to extend ODT.
  if (config.useOdt) io.output.odt.clearAll()

  // cmd
  if (config.useAckN) io.output.actN := Bits(widthOf(io.output.actN) bits).setAll()
  io.output.csN := Bits(widthOf(io.output.csN) bits).setAll()
  if (config.useRasN) io.output.rasN := Bits(widthOf(io.output.rasN) bits).setAll()
  if (config.useCasN) io.output.casN := Bits(widthOf(io.output.casN) bits).setAll()
  if (config.useWeN) io.output.weN := Bits(widthOf(io.output.weN) bits).setAll()
  if (config.useCid) io.output.cid := Bits(widthOf(io.output.cid) bits).clearAll()

  // address
  // With CA parity enabled the idle bus value must be deterministic (the parity bit is the XOR
  // of every covered pin, including NOP phases), so bg loses its dontCare default.
  if (config.useBg) io.output.bg := (if (config.useCaParity) B(0, widthOf(io.output.bg) bits)
                                     else Bits(widthOf(io.output.bg) bits).assignDontCare())
  if (config.useBank) io.output.bank := Bits(widthOf(io.output.bank) bits).clearAll()
  io.output.address := Bits(widthOf(io.output.address) bits).clearAll()

  def csN = io.output.csN.subdivideIn(config.frequencyRatio slices)
  def casN = io.output.casN.subdivideIn(config.frequencyRatio slices)
  def rasN = io.output.rasN.subdivideIn(config.frequencyRatio slices)
  def weN = io.output.weN.subdivideIn(config.frequencyRatio slices)

  for (i <- 0 until (config.frequencyRatio)) {
    when(io.cmd(i).valid) {
      if (config.useAckN) io.output.actN.subdivideIn(config.frequencyRatio slices)(i) := io.cmd(i).actN.asBits
      if (config.useCid) io.output.cid.subdivideIn(config.frequencyRatio slices)(i) := io.cmd(i).cid.asBits
      csN(i) := io.cmd(i).csN.asBits
      if (config.useCasN) casN(i) := io.cmd(i).casN.asBits
      if (config.useRasN) rasN(i) := io.cmd(i).rasN.asBits
      if (config.useWeN) weN(i) := io.cmd(i).weN.asBits
    }
    when(io.address(i).valid) {
      if (config.useBg) io.output.bg(i * config.bankGroupWidth, config.bankGroupWidth bits) := io.address(i).bg
      if (config.useBank) io.output.bank(i * config.bankWidth, config.bankWidth bits) := io.address(i).bank
      io.output.address(i * config.addressWidth, config.addressWidth bits) := io.address(i).address
    }
  }

  // DDR4 CA parity (JESD79-4B §4.17): even parity = XOR of every covered CA pin of that phase,
  // computed from the values actually driven on the bus this cycle (command phases use the
  // command values, NOP phases the idle defaults — idle XORs to 0 because cid/address/bank/bg
  // idle at 0 and act_n/ras/cas/we idle at 1). Coverage set: ACT_n, RAS_n/A16, CAS_n/A15,
  // WE_n/A14, A13..A0, BA, BG. CS_n/CKE/ODT are deliberately NOT covered (spec). Same-cycle as
  // the command (tcapar_lat = 0).
  if (config.useCaParity) {
    for (i <- 0 until config.frequencyRatio) {
      val covered =
        (if (config.useAckN) List(io.output.actN(i, 1 bits)) else Nil) ++
          (if (config.useRasN) List(io.output.rasN(i * config.controlWidth, config.controlWidth bits)) else Nil) ++
          (if (config.useCasN) List(io.output.casN(i * config.controlWidth, config.controlWidth bits)) else Nil) ++
          (if (config.useWeN) List(io.output.weN(i * config.controlWidth, config.controlWidth bits)) else Nil) ++
          (if (config.useCid) List(io.output.cid(i * config.chipIdWidth, config.chipIdWidth bits)) else Nil) ++
          (if (config.useBg) List(io.output.bg(i * config.bankGroupWidth, config.bankGroupWidth bits)) else Nil) ++
          (if (config.useBank) List(io.output.bank(i * config.bankWidth, config.bankWidth bits)) else Nil) ++
          List(io.output.address(i * config.addressWidth, config.addressWidth bits))
      io.output.parity(i) := covered.map(_.xorR).reduce(_ ^ _)
    }
  }
}

case class RdAlignment(config: DfiConfig) extends Component {
  val io = new Bundle {
    val phaseClear = in Bool ()
    val inputEn = in Vec (Bool(), config.frequencyRatio)
    val input = Vec(master(Stream(Fragment(DfiRdData(config)))), config.frequencyRatio)
    val inputCs = config.useRddataCsN generate Vec(slave(Flow(DfiReadCs(config))), config.frequencyRatio)
    val output = master(DfiReadInterface(config))
  }
  val dfiRdCs = config.useRddataCsN generate Vec(out(DfiRdCs(config)), config.frequencyRatio)
  val dfiRd = Vec(DfiRd(config), config.frequencyRatio)
  io.output.rden := io.inputEn
  if (config.useRddataCsN) io.output.rdCs := dfiRdCs
  dfiRd := io.output.rd

  val rdDataTemp = Vec(Stream(Fragment(DfiRdData(config))), config.frequencyRatio)
  rdDataTemp.foreach(_.last.clear())
  for (i <- 0 until (config.frequencyRatio)) {
    rdDataTemp(i).valid := dfiRd(i).rddataValid
    rdDataTemp(i).rdData.assignDontCare()
  }

  val curPhase = Reg(UInt(log2Up(config.frequencyRatio) bits)) init (0)
  val rdDataPhase = Reg(UInt(log2Up(config.frequencyRatio) + 1 bits)) init (0)
  rdDataPhase.clearAll()
  when(io.phaseClear) {
    curPhase := U(0)
  }

  for (i <- 0 until (config.frequencyRatio)) {
    val muxed = (i + curPhase + config.frequencyRatio - rdDataPhase)
      .resize(log2Up(config.frequencyRatio))
      .muxListDc(dfiRd.zipWithIndex.map(t => (t._2, t._1)))
    // Driven every cycle (not only on fire): an unassigned comb signal would trip the
    // no-latch elaboration check, and the FIFO only samples the payload on fire anyway.
    if (config.useRddataDbiN) rdDataTemp(i).rdDataDbiN := muxed.rddataDbiN
    when(rdDataTemp(i).fire) {
      // DDR4 read DBI (JESD79-4B §4.11): DRAM drove the byte inverted with rddata_dbi_n=0
      // (active low) for lanes with more zeros than ones; restore those bytes here.
      if (config.useRdDbi) {
        val restoredBytes = for (j <- 0 until config.dbiWidth) yield {
          val b = muxed.rddata(j * 8, 8 bits)
          Mux(!muxed.rddataDbiN(j), ~b, b)
        }
        rdDataTemp(i).rdData := restoredBytes.reverse.reduce(_ ## _)
      } else {
        rdDataTemp(i).rdData := muxed.rddata
      }
      curPhase := (i + 1) % config.frequencyRatio
      rdDataPhase := (rdDataPhase + i + 1 + config.frequencyRatio - curPhase)
        .resize(log2Up(config.frequencyRatio))
        .resized
    }
  }
  val rdDataFifos = for (i <- 0 until (config.frequencyRatio)) yield new Area {
    val rdDataFifo = new StreamFifo(Fragment(DfiRdData(config)), config.beatCount + 2)
    rdDataFifo.io.flush.clear()
    rdDataFifo.io.flush.setWhen(io.phaseClear)
  }
  val readyForPop = Mux(rdDataFifos.map(_.rdDataFifo.io.occupancy =/= 0).andR, io.input.map(_.ready).orR, False)

  for (i <- 0 until (config.frequencyRatio)) {
    rdDataFifos(i).rdDataFifo.io.push << rdDataTemp(i)
    io.input(i).valid := rdDataFifos.map(_.rdDataFifo.io.pop.valid).andR
    io.input(i).payload := rdDataFifos(i).rdDataFifo.io.pop.payload
    rdDataFifos(i).rdDataFifo.io.pop.ready := RegNext(readyForPop)
  }

  if (config.useRddataCsN) {
    for (i <- 0 until (config.frequencyRatio)) {
      dfiRdCs(i).rddataCsN.setAll()
      when(io.inputCs(i).valid) {
        dfiRdCs(i).rddataCsN := io.inputCs(i).rdCs
      }
    }
  }
}

case class WrAlignment(config: DfiConfig) extends Component {
  val io = new Bundle {
    val inputCs = config.useWrdataCsN generate Vec(slave(Flow(DfiWrCs(config))), config.frequencyRatio)
    val input = Vec(slave(Flow(DfiWrData(config))), config.frequencyRatio)
    val output = master(DfiWriteInterface(config))
  }
  val frequencyRatio = config.frequencyRatio
  val timeConfig = config.timeConfig

  for (i <- 0 until (frequencyRatio)) {
    io.output.wr(i).wrdataEn := io.input(i).valid
  }

  val delay = Reg(U(timeConfig.tPhyWrData / frequencyRatio) << 1).init(0)

  when(io.input.map(_.valid).orR.rise()) {
    delay := timeConfig.tPhyWrData / frequencyRatio
  }

  val wrdataHistory = Vec(Vec(DfiWrData(config), timeConfig.tPhyWrData / frequencyRatio + 2), frequencyRatio)
  for (i <- 0 until (frequencyRatio)) {
    wrdataHistory(i) := History(io.input(i).payload, 0 to timeConfig.tPhyWrData / frequencyRatio + 1)
    io.output.wr(i).wrdata.assignDontCare()
    io.output.wr(i).wrdataMask.assignDontCare()
    if (config.useWrDbi) io.output.wr(i).wrdataDbiN.assignDontCare()
  }

  for (phase <- 0 until (frequencyRatio)) {
    val muxed = (if (phase < timeConfig.tPhyWrData % frequencyRatio) delay + 1 else delay)
      .muxListDc(
        wrdataHistory
          .shuffle(t => (t + timeConfig.tPhyWrData % frequencyRatio) % frequencyRatio)(phase)
          .zipWithIndex
          .map(t => (t._2, t._1))
      )
    io.output.wr(phase).wrdata := muxed.wrData
    io.output.wr(phase).wrdataMask := muxed.wrDataMask
    if (config.useWrDbi) io.output.wr(phase).wrdataDbiN := muxed.wrDataDbiN
  }

  if (config.useWrdataCsN) {
    for (i <- 0 until (frequencyRatio)) {
      io.output.wr(i).wrdataCsN.setAll()
      when(io.inputCs(i).valid) {
        io.output.wr(i).wrdataCsN := io.inputCs(i).wrCs
      }
    }
  }
}
