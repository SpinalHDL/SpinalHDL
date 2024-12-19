package spinal.lib.memory.sdram.dfi.function

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.dfi.interface._

case class CAAlignment(config: DfiConfig) extends Component {
  import config._
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
  if (useResetN) io.output.resetN.setAll()
  // The MC generates dfi_odt (dfi_odt_pN in frequency ratio systems) based on the DRAM burst length including
  // CRC data. With CRC enabled, the MC may need to extend ODT.
  if (useOdt) io.output.odt.clearAll()

  // cmd
  if (useAckN) io.output.actN := Bits(widthOf(io.output.actN) bits).setAll()
  io.output.csN := Bits(widthOf(io.output.csN) bits).setAll()
  io.output.rasN := Bits(widthOf(io.output.rasN) bits).setAll()
  io.output.casN := Bits(widthOf(io.output.casN) bits).setAll()
  io.output.weN := Bits(widthOf(io.output.weN) bits).setAll()
  if (useCid) io.output.cid := Bits(widthOf(io.output.cid) bits).clearAll()

  // address
  if (config.useBg) io.output.bg := Bits(widthOf(io.output.bg) bits).assignDontCare()
  io.output.bank := Bits(widthOf(io.output.bank) bits).clearAll()
  io.output.address := Bits(widthOf(io.output.address) bits).clearAll()

  def csN = io.output.csN.subdivideIn(frequencyRatio slices)
  def casN = io.output.casN.subdivideIn(frequencyRatio slices)
  def rasN = io.output.rasN.subdivideIn(frequencyRatio slices)
  def weN = io.output.weN.subdivideIn(frequencyRatio slices)

  for (i <- 0 until (frequencyRatio)) {
    when(io.cmd(i).valid) {
      if (useAckN) io.output.actN.subdivideIn(frequencyRatio slices)(i) := io.cmd(i).actN.asBits
      if (useCid) io.output.cid.subdivideIn(frequencyRatio slices)(i) := io.cmd(i).cid.asBits
      csN(i) := io.cmd(i).csN.asBits
      casN(i) := io.cmd(i).casN.asBits
      rasN(i) := io.cmd(i).rasN.asBits
      weN(i) := io.cmd(i).weN.asBits
    }
    when(io.address(i).valid) {
      if (config.useBg) io.output.bg(i * bankGroupWidth, bankGroupWidth bits) := io.address(i).bg
      io.output.bank(i * bankWidth, bankWidth bits) := io.address(i).bank
      io.output.address(i * addressWidth, addressWidth bits) := io.address(i).address
    }
  }
}

case class RdAlignment(config: DfiConfig) extends Component {
  import config._
  val io = new Bundle {
    val phaseClear = in Bool ()
    val dfiRd = in Vec (DfiRd(config), config.frequencyRatio)
    val dfiRdCs = useRddataCsN generate Vec(out(DfiRdCs(config)), config.frequencyRatio)
    val idfiRd = Vec(master(Stream(Fragment(DfiRdData(config)))), config.frequencyRatio)
    val idfiRdCs = useRddataCsN generate Vec(slave(Flow(DfiReadCs(config))), config.frequencyRatio)
  }

  val rdDataTemp = Vec(Stream(Fragment(DfiRdData(config))), config.frequencyRatio)
  rdDataTemp.foreach(_.last.clear())
  for (i <- 0 until (frequencyRatio)) {
    rdDataTemp(i).valid := io.dfiRd(i).rddataValid
    rdDataTemp(i).rdData.assignDontCare()
  }

  val curPhase = Reg(UInt(log2Up(frequencyRatio) bits)) init (0)
  val rdDataPhase = Reg(UInt(log2Up(frequencyRatio) + 1 bits)) init (0)
  rdDataPhase.clearAll()
  when(io.phaseClear) {
    curPhase := U(0)
  }

  for (i <- 0 until (frequencyRatio)) {
    when(rdDataTemp(i).fire) {
      rdDataTemp(i).rdData := (i + curPhase + frequencyRatio - rdDataPhase)
        .resize(log2Up(frequencyRatio))
        .muxListDc(io.dfiRd.zipWithIndex.map(t => (t._2, t._1)))
        .rddata
      curPhase := (i + 1) % frequencyRatio
      rdDataPhase := (rdDataPhase + i + 1 + frequencyRatio - curPhase).resize(log2Up(frequencyRatio)).resized
    }
  }
  val rdDataFifos = for (i <- 0 until (frequencyRatio)) yield new Area {
    val rdDataFifo = new StreamFifo(Fragment(DfiRdData(config)), config.beatCount + 2)
    rdDataFifo.io.flush.clear()
    rdDataFifo.io.flush.setWhen(io.phaseClear)
  }
  val readyForPop = Mux(rdDataFifos.map(_.rdDataFifo.io.occupancy =/= 0).andR, io.idfiRd.map(_.ready).orR, False)

  for (i <- 0 until (frequencyRatio)) {
    rdDataFifos(i).rdDataFifo.io.push << rdDataTemp(i)
    io.idfiRd(i).valid := rdDataFifos.map(_.rdDataFifo.io.pop.valid).andR
    io.idfiRd(i).payload := rdDataFifos(i).rdDataFifo.io.pop.payload
    rdDataFifos(i).rdDataFifo.io.pop.ready := RegNext(readyForPop)
  }

  if (useRddataCsN) {
    for (i <- 0 until (frequencyRatio)) {
      io.dfiRdCs(i).rddataCsN.setAll()
      when(io.idfiRdCs(i).valid) {
        io.dfiRdCs(i).rddataCsN := io.idfiRdCs(i).rdCs
      }
    }
  }
}

case class WrAlignment(config: DfiConfig) extends Component {
  import config._
  val io = new Bundle {
    val idfiWrCs = useWrdataCsN generate Vec(slave(Flow(DfiWrCs(config))), config.frequencyRatio)
    val idfiWrData = Vec(slave(Flow(DfiWrData(config))), config.frequencyRatio)
    val dfiWr = master(DfiWriteInterface(config))
  }
  for (i <- 0 until (frequencyRatio)) {
    io.dfiWr.wr(i).wrdataEn := io.idfiWrData(i).valid
  }

  val delay = Reg(U(timeConfig.tPhyWrData / frequencyRatio) << 1).init(0)

  when(io.idfiWrData.map(_.valid).orR.rise()) {
    delay := timeConfig.tPhyWrData / frequencyRatio
  }

  val wrdatahistary = Vec(Vec(DfiWrData(config), timeConfig.tPhyWrData / frequencyRatio + 2), frequencyRatio)
  for (i <- 0 until (frequencyRatio)) {
    wrdatahistary(i) := History(io.idfiWrData(i).payload, 0 to timeConfig.tPhyWrData / frequencyRatio + 1)
    io.dfiWr.wr(i).wrdata.assignDontCare()
    io.dfiWr.wr(i).wrdataMask.assignDontCare()
  }

  for (phase <- 0 until (frequencyRatio)) {
    io.dfiWr.wr(phase).wrdata := (if (phase < timeConfig.tPhyWrData % frequencyRatio) delay + 1 else delay)
      .muxListDc(
        wrdatahistary
          .shuffle(t => (t + timeConfig.tPhyWrData % frequencyRatio) % frequencyRatio)(phase)
          .zipWithIndex
          .map(t => (t._2, t._1))
      )
      .wrData
    io.dfiWr.wr(phase).wrdataMask := (if (phase < timeConfig.tPhyWrData % frequencyRatio) delay + 1 else delay)
      .muxListDc(
        wrdatahistary
          .shuffle(t => (t + timeConfig.tPhyWrData % frequencyRatio) % frequencyRatio)(phase)
          .zipWithIndex
          .map(t => (t._2, t._1))
      )
      .wrDataMask
  }

  if (useWrdataCsN) {
    for (i <- 0 until (frequencyRatio)) {
      io.dfiWr.wr(i).wrdataCsN.setAll()
      when(io.idfiWrCs(i).valid) {
        io.dfiWr.wr(i).wrdataCsN := io.idfiWrCs(i).wrCs
      }
    }
  }
}
