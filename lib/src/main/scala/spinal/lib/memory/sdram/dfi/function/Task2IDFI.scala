package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._

case class CmdTxd(taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Component {
  val io = new Bundle {
    val task = slave(OpTasks(taskConfig, dfiConfig))
    val cmd = Vec(master(Flow(DfiCmd(dfiConfig))), dfiConfig.frequencyRatio)
    val address = Vec(master(Flow(DfiAddr(dfiConfig))), dfiConfig.frequencyRatio)
  }
  def cmdphase(i: Int) = io.cmd(i)
  def addrphase(i: Int) = io.address(i)

  // Rank one-hot decode (active-low), REPLACED by MakeTask's init broadcast mask during the
  // init window: initCsN is all-ones at runtime (andR -> exact legacy single-rank latch cs),
  // and the elaboration-time mask during init — csN(r) = mask(r) ? 0 : 1, so the runtime cs
  // latch no longer decides which rank receives MRS/ZQCL/PRE_ALL/REF; every rank in the mask
  // does, in the same cycle.
  val csSel = Mux(
    io.task.initCsN.andR,
    ~(B(1) << io.task.address.cs).resize(dfiConfig.chipSelectNumber),
    io.task.initCsN
  )

  // ---- Legacy (DDR3 and below): fixed RAS/CAS/WE encoding, unchanged ----
  def ACTIVE: Bits = (csSel ## B"b011").setName("ACTIVE")
  def WRITE: Bits = (csSel ## B"b100").setName("WRITE")
  def READ: Bits = (csSel ## B"b101").setName("READ")
  def PRECHARGE: Bits = csSel ## B"b010"
  def REFRESH: Bits = (csSel ## B"b001").setName("REFRESH")

  def AUTO_PRECHARGE_BIT = 10 // Disable auto precharge (auto close of row)
  def ALL_BANKS_BIT = 10 // Precharge all banks
  def COLUMNRange = 0 until (dfiConfig.sdram.columnWidth)
  def BANK: Bits = io.task.address.bank.asBits
  def ROW: Bits = io.task.address.row.asBits
  def COLUMN: Bits = io.task.address.column.asBits

  def active = io.task.active
  def write = io.task.write
  def read = io.task.read
  def precharge = io.task.precharge
  def prechargeAll = io.task.prechargeAll
  def refresh = io.task.refresh

  io.cmd.foreach(_.valid.clear())
  io.cmd.foreach(_.payload.setAll())
  io.address.foreach(_.valid.clear())
  io.address.foreach(_.address.clearAll())
  io.address.foreach(_.bank.clearAll())
  if (dfiConfig.useBg) io.address.foreach(_.bg.clearAll())

  if (dfiConfig.sdram.generation == SdramGeneration.DDR4) {
    // ---- DDR4: JESD79-4B Table 17 on the DFI control bus ----
    // Existing payload convention (verified against legacy encoders): in the 3-bit control nibble,
    // bit0=weN, bit1=casN, bit2=rasN. DDR4 WR/RD/PRE/REF share the DDR3 RAS/CAS/WE encodings;
    // ACT is flagged by actN=0, and the RAS_n/CAS_n/WE_n pins become A16/A15/A14 carrying the
    // top row bits: A14=RA[13] -> weN, A15=RA[14] -> casN, A16=RA[15] -> rasN.
    // Flat bank index split at the exit: bg = bank[bankWidth-1 : dfiBankWidth], ba = bank[dfiBankWidth-1 : 0]
    val bgBits = io.task.address.bank(dfiConfig.dfiBankWidth, dfiConfig.bankGroupWidth bits).asBits
    val baBits = io.task.address.bank(0, dfiConfig.dfiBankWidth bits).asBits
    // Top 3 row bits (RA[15:13]) for the ACT A16/A15/A14 pins
    val rowHi = io.task.address.row >> (dfiConfig.sdram.rowWidth - 3)

    // cid stays 0: rank selection uses csN (one chip-select per rank); cid is an HBM-style
    // alternative the PHY ignores in this topology. (payload.setAll() above would otherwise
    // leave cid=1 on every command.)
    when(active || write || read || precharge || prechargeAll || refresh || io.task.mrs || io.task.zq) {
      cmdphase(dfiConfig.timeConfig.cmdPhase).cid := B(0, dfiConfig.chipIdWidth bits)
    }

    when(active) {
      cmdphase(dfiConfig.timeConfig.cmdPhase).valid.set()
      if (dfiConfig.useAckN) {
        cmdphase(dfiConfig.timeConfig.cmdPhase).actN := False
        cmdphase(dfiConfig.timeConfig.cmdPhase).weN := rowHi(0) // A14 = RA13
        cmdphase(dfiConfig.timeConfig.cmdPhase).casN := rowHi(1) // A15 = RA14
        cmdphase(dfiConfig.timeConfig.cmdPhase).rasN := rowHi(2) // A16 = RA15
      }
      cmdphase(dfiConfig.timeConfig.cmdPhase).csN := csSel.asBits
      addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
      addrphase(dfiConfig.timeConfig.cmdPhase).bg := bgBits
      addrphase(dfiConfig.timeConfig.cmdPhase).bank := baBits.resized
      addrphase(dfiConfig.timeConfig.cmdPhase).address := ROW.resized
    }
    when(write) {
      cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).csN := csSel.asBits
      if (dfiConfig.useAckN) cmdphase(dfiConfig.timeConfig.cmdPhase).actN := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).rasN := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).casN := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).weN := False // WR (Table 17): RAS=H CAS=H WE=L
      addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
      addrphase(dfiConfig.timeConfig.cmdPhase).bg := bgBits
      addrphase(dfiConfig.timeConfig.cmdPhase).bank := baBits.resized
      addrphase(dfiConfig.timeConfig.cmdPhase).address(COLUMNRange) := COLUMN
      addrphase(dfiConfig.timeConfig.cmdPhase).address(AUTO_PRECHARGE_BIT).clear() // WR (no AP); WRA variant not generated by station
    }
    when(read) {
      cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).csN := csSel.asBits
      if (dfiConfig.useAckN) cmdphase(dfiConfig.timeConfig.cmdPhase).actN := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).rasN := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).casN := False
      cmdphase(dfiConfig.timeConfig.cmdPhase).weN := True // RD: RAS=H CAS=L WE=H
      addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
      addrphase(dfiConfig.timeConfig.cmdPhase).bg := bgBits
      addrphase(dfiConfig.timeConfig.cmdPhase).bank := baBits.resized
      addrphase(dfiConfig.timeConfig.cmdPhase).address(COLUMNRange) := COLUMN
      addrphase(dfiConfig.timeConfig.cmdPhase).address(AUTO_PRECHARGE_BIT).clear()
    }
    when(precharge) {
      cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).csN := csSel.asBits
      if (dfiConfig.useAckN) cmdphase(dfiConfig.timeConfig.cmdPhase).actN := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).rasN := False
      cmdphase(dfiConfig.timeConfig.cmdPhase).casN := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).weN := False // PRE: RAS=L CAS=H WE=L
      addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
      addrphase(dfiConfig.timeConfig.cmdPhase).bg := bgBits
      addrphase(dfiConfig.timeConfig.cmdPhase).bank := baBits.resized
      addrphase(dfiConfig.timeConfig.cmdPhase).address(ALL_BANKS_BIT).clear() // A10=L: single bank
    }
    when(prechargeAll) {
      cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).csN := csSel.asBits
      if (dfiConfig.useAckN) cmdphase(dfiConfig.timeConfig.cmdPhase).actN := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).rasN := False
      cmdphase(dfiConfig.timeConfig.cmdPhase).casN := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).weN := False // PREA shares PRE encoding, A10=H
      addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
      addrphase(dfiConfig.timeConfig.cmdPhase).address(ALL_BANKS_BIT).set()
    }
    when(refresh) {
      cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).csN := csSel.asBits
      if (dfiConfig.useAckN) cmdphase(dfiConfig.timeConfig.cmdPhase).actN := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).rasN := False
      cmdphase(dfiConfig.timeConfig.cmdPhase).casN := False
      cmdphase(dfiConfig.timeConfig.cmdPhase).weN := True // REF: RAS=L CAS=L WE=H
      addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
    }
    when(io.task.mrs) {
      // MRS: ACT_n=H, RAS_n=L, CAS_n=L, WE_n=L. MR select = {BG0, BA1:BA0} (flat bank[2:0]),
      // OP code on A15:A0 (carried in the row field by the init sequencer).
      cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).csN := csSel.asBits
      if (dfiConfig.useAckN) cmdphase(dfiConfig.timeConfig.cmdPhase).actN := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).rasN := False
      cmdphase(dfiConfig.timeConfig.cmdPhase).casN := False
      cmdphase(dfiConfig.timeConfig.cmdPhase).weN := False
      addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
      addrphase(dfiConfig.timeConfig.cmdPhase).bg := bgBits
      addrphase(dfiConfig.timeConfig.cmdPhase).bank := baBits.resized
      addrphase(dfiConfig.timeConfig.cmdPhase).address := ROW.resized
    }
    when(io.task.zq) {
      // ZQCL (Table 17): ACT_n=H RAS_n=H CAS_n=H WE_n=H, A10=H (long).
      // Same nibble as NOP; A10=H is what makes it ZQCL (ZQCS is A10=L).
      // Agent decodes identically: zqcl = actN && ras && cas && weN && a10.
      cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).csN := csSel.asBits
      if (dfiConfig.useAckN) cmdphase(dfiConfig.timeConfig.cmdPhase).actN := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).rasN := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).casN := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).weN := True
      addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
      addrphase(dfiConfig.timeConfig.cmdPhase).address(ALL_BANKS_BIT).set()
    }
  } else {
    // ---- Legacy path: byte-for-byte the pre-DDR4 behavior ----
    when(active) {
      cmdphase(dfiConfig.timeConfig.cmdPhase).valid.set()
      cmdphase(dfiConfig.timeConfig.cmdPhase).payload.assignFromBits(ACTIVE)
      addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
      addrphase(dfiConfig.timeConfig.cmdPhase).bank := BANK.resized
      addrphase(dfiConfig.timeConfig.cmdPhase).address := ROW.resized
    }
    when(write) {
      cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).payload.assignFromBits(WRITE)
      addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
      addrphase(dfiConfig.timeConfig.cmdPhase).bank := BANK.resized
      addrphase(dfiConfig.timeConfig.cmdPhase).address(COLUMNRange) := COLUMN
      addrphase(dfiConfig.timeConfig.cmdPhase).address(AUTO_PRECHARGE_BIT).clear()
    }
    when(read) {
      cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).payload.assignFromBits(READ)
      addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
      addrphase(dfiConfig.timeConfig.cmdPhase).bank := BANK.resized
      addrphase(dfiConfig.timeConfig.cmdPhase).address(COLUMNRange) := COLUMN
      addrphase(dfiConfig.timeConfig.cmdPhase).address(AUTO_PRECHARGE_BIT).clear()
    }
    when(precharge) {
      cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).payload.assignFromBits(PRECHARGE.setName("PRECHARGE"))
      addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
      addrphase(dfiConfig.timeConfig.cmdPhase).bank := BANK.resized
      addrphase(dfiConfig.timeConfig.cmdPhase).address(ALL_BANKS_BIT).clear()
    }
    when(prechargeAll) {
      cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).payload.assignFromBits(PRECHARGE.setName("PRECHARGEALL"))
      addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
      addrphase(dfiConfig.timeConfig.cmdPhase).address(ALL_BANKS_BIT).set()
    }
    when(refresh) {
      cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).payload.assignFromBits(REFRESH)
      addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
    }
    // Init ops (driven only while MakeTask's init engine plays its step table; always False
    // for init-less configs, so init-less behavior is bit-identical to dev).
    // MRS/EMRS: cmd {ras_n,cas_n,we_n} = 000 (Micron ddr3.v cmd table); MR/EMRS select on
    // the bank field (BA2:0 = mrIndex, EMRS = BA1/2/3), OP code on the row/address field.
    when(io.task.mrs) {
      cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).payload.assignFromBits(
        (csSel ## B"b000").setName("MRS")
      )
      addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
      addrphase(dfiConfig.timeConfig.cmdPhase).bank := BANK.resized
      addrphase(dfiConfig.timeConfig.cmdPhase).address := ROW.resized
    }
    // ZQCL: cmd = 110 (ras_n=H, cas_n=H, we_n=L) + A10=H (long / all-bank calibration).
    // DDR2 has no ZQ; its presets simply contain no ZQ step.
    when(io.task.zq) {
      cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
      cmdphase(dfiConfig.timeConfig.cmdPhase).payload.assignFromBits(
        (csSel ## B"b110").setName("ZQCL")
      )
      addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
      addrphase(dfiConfig.timeConfig.cmdPhase).address(ALL_BANKS_BIT).set()
    }
  }
}

case class RdDataRxd(taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Component {

  val frequencyRatio = dfiConfig.frequencyRatio
  val timeConfig = dfiConfig.timeConfig
  val io = new Bundle {
    val task = slave(OpTasks(taskConfig, dfiConfig))
    val input = Vec(slave(Stream(Fragment(DfiRdData(dfiConfig)))), frequencyRatio)
    val rden = out Vec (Bool(), frequencyRatio)
    val outputCs = dfiConfig.useRddataCsN generate Vec(master(Flow(DfiReadCs(dfiConfig))), frequencyRatio)
    val output = master(Flow(Fragment(TaskRsp(taskConfig, dfiConfig))))
  }
  val rspPipeline = new Area {
    val input = Flow(Fragment(Context()))
    assert(timeConfig.tPhyRdlat + timeConfig.tRddataEn >= 1)
    val cmd = input.toStream.queueLowLatency(
      1 << log2Up((timeConfig.tPhyRdlat + timeConfig.tRddataEn + dfiConfig.beatCount - 1) / dfiConfig.beatCount + 1),
      latency = 1
    )
    val rden = input.valid & ~input.write
    val rdensHistory =
      Vec(Vec(Bool(), (dfiConfig.timeConfig.cmdPhase + timeConfig.tRddataEn) / frequencyRatio + 2), frequencyRatio)
    rdensHistory.foreach(
      _ := History(rden, 0 to (dfiConfig.timeConfig.cmdPhase + timeConfig.tRddataEn) / frequencyRatio + 1, init = False)
    )

    val beatCounter = Counter(dfiConfig.beatCount, io.input.map(_.valid).orR)

    val delayCyc = timeConfig.tRddataEn / frequencyRatio
    val nextPhase = (dfiConfig.timeConfig.cmdPhase + timeConfig.tRddataEn) % frequencyRatio

    for (i <- 0 until (frequencyRatio)) {
      if (i >= nextPhase) {
        io.rden(i) := History(rdensHistory(nextPhase)(delayCyc), 0 until (dfiConfig.beatCount), init = False).orR
      } else {
        io.rden(i) := History(rdensHistory(nextPhase)(delayCyc + 1), 0 until (dfiConfig.beatCount), init = False).orR
      }
    }

    // dfi_rddata_cs_n 与 dfi_rddata_valid 对齐(不实现 tPhyRdCslat 独立延迟):
    // 在读命令拍捕获 rank 选择并保持,直到下一个读命令覆盖。MakeTask 的 RTW 定时器保证
    // 前一命令的 rddata 全部返回后才会发出下一命令,故保持窗口不会跨越命令边界。
    val csHold = Reg(Bits(dfiConfig.chipSelectNumber bits)) init (Bits(dfiConfig.chipSelectNumber bits).setAll())
    when(io.task.read) {
      csHold := ~(B(1) << io.task.address.cs).resize(dfiConfig.chipSelectNumber)
    }
    if (dfiConfig.useRddataCsN) {
      for (i <- 0 until (frequencyRatio)) {
        io.outputCs(i).valid := io.input(i).valid
        io.outputCs(i).rdCs := csHold
      }
    }

    val output = Flow(Fragment(PipelineRsp()))
    output.valid.clear()
    output.valid.setWhen(cmd.valid && cmd.write && cmd.last || io.input.map(_.valid).orR)
    output.context := cmd.context
    output.last := cmd.write || beatCounter.willOverflowIfInc && cmd.last
    cmd.ready := cmd.write || beatCounter.willOverflow

    for ((outputData, phase) <- (output.data.subdivideIn(frequencyRatio slices).reverse, io.input).zipped) {
      outputData := B(phase.rdData)
    }
  }
  val rspPop = rspPipeline.output.stage()
  val ready = Vec(Reg(Bool()), frequencyRatio)

  rspPipeline.input.valid := False
  rspPipeline.input.write.clear()
  rspPipeline.input.last := io.task.last
  rspPipeline.input.context := io.task.context

  case class PipelineRsp() extends Bundle {
    val data = Bits(dfiConfig.beatWidth bits)
    val context = Bits(taskConfig.contextWidth bits)
  }
  io.output.valid := rspPop.valid
  io.output.last := rspPop.last
  if (io.output.taskConfig.canRead) io.output.data := rspPop.data
  io.output.context := rspPop.context.resized

  rspPipeline.input.valid.setWhen(io.task.read | io.task.write)
  rspPipeline.input.write.setWhen(io.task.write).clearWhen(io.task.read)

  case class Context() extends Bundle {
    val context = Bits(taskConfig.contextWidth bits)
    val write = Bool()
  }
  ready.foreach(_.init(False))
  for (i <- 0 until (frequencyRatio)) {
    ready(i).setWhen(io.task.read).clearWhen(io.task.write)
  }
  for ((outport, iready) <- (io.input, ready).zipped) {
    outport.ready := iready
  }
}

case class WrDataTxd(taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Component {

  val frequencyRatio = dfiConfig.frequencyRatio
  val timeConfig = dfiConfig.timeConfig
  val io = new Bundle {
    val write = in Bool ()
    // 写命令的 rank 选择(低有效),与 write 标志同拍;无写命令时全 1(不选中任何 rank)
    val cs = in Bits (dfiConfig.chipSelectNumber bits)
    val input = slave(Stream(TaskWriteData(dfiConfig)))
    val output = Vec(master(Flow(DfiWrData(dfiConfig))), frequencyRatio)
    val outputCs = dfiConfig.useWrdataCsN generate Vec(master(Flow(DfiWrCs(dfiConfig))), frequencyRatio)
  }
  val delayCyc = timeConfig.tPhyWrLat / frequencyRatio
  val nextPhase = (dfiConfig.timeConfig.cmdPhase + timeConfig.tPhyWrLat) % frequencyRatio
  val writeHistory = History(io.write, 0 until dfiConfig.beatCount, init = False)
  val write = writeHistory.orR
  val wrens = Vec(Bool(), frequencyRatio)
  val wrensHistory =
    Vec(Vec(Bool(), (dfiConfig.timeConfig.cmdPhase + timeConfig.tPhyWrLat) / frequencyRatio + 2), frequencyRatio)
  // dfi_wrdata_cs_n 与 dfi_wrdata_en 对齐(本控制器不实现 tPhyWrCsLat 独立延迟):
  // rank 选择随写使能链同步延迟,空闲时保持全 1
  val csHistory =
    Vec(Vec(Bits(dfiConfig.chipSelectNumber bits), (dfiConfig.timeConfig.cmdPhase + timeConfig.tPhyWrLat) / frequencyRatio + 2), frequencyRatio)
  for (i <- 0 until (frequencyRatio)) {
    csHistory(i) := History(
      io.cs,
      0 to (dfiConfig.timeConfig.cmdPhase + timeConfig.tPhyWrLat) / frequencyRatio + 1,
      init = Bits(dfiConfig.chipSelectNumber bits).setAll()
    )
  }
  def wrdataPhase(i: Int) = io.output(i)
  // DDR4 DBI (JESD79-4B §4.11): per byte lane, more zeros than ones (ones < 4) -> transmit the
  // inverted byte with DBI_n=0 (active low); otherwise the byte passes as-is with DBI_n=1. The
  // DRAM re-inverts bytes whose DBI_n is low. The encoded data and DBI_n take exactly the same
  // subdivide/reverse/shuffle (+RegNext) path as wrData so all three stay phase-aligned.
  val dbiEncoded = if (dfiConfig.useWrDbi) new Area {
    val data = Bits(dfiConfig.beatWidth bits)
    val dbiN = Bits(dfiConfig.beatWidth / 8 bits)
    for (j <- 0 until dfiConfig.beatWidth / 8) {
      val b = io.input.payload.data(j * 8, 8 bits)
      val invert = CountOne(b.asBools) < 4
      data(j * 8, 8 bits) := Mux(invert, ~b, b)
      dbiN(j) := !invert
    }
  } else null
  val wrSrcData = if (dfiConfig.useWrDbi) dbiEncoded.data else io.input.payload.data
  val wrSrcDbiN = if (dfiConfig.useWrDbi) dbiEncoded.dbiN else null
  for (i <- 0 until (frequencyRatio)) {
    wrensHistory(i) := History(
      wrens(i),
      0 to (dfiConfig.timeConfig.cmdPhase + timeConfig.tPhyWrLat) / frequencyRatio + 1,
      init = False
    )
  }
  wrens.foreach(_.clear())
  wrens.foreach(_.setWhen(write))
  io.input.ready.clear()
  io.input.ready.setWhen(wrensHistory(nextPhase)(delayCyc))
  assert(!(!io.input.valid && io.input.ready), "SDRAM write data stream starved !", ERROR)
  for (i <- 0 until (frequencyRatio)) {
    if (i >= nextPhase) {
      wrdataPhase(i).valid := wrensHistory(nextPhase)(delayCyc)
      wrdataPhase(i).wrData := Vec(wrSrcData.subdivideIn(frequencyRatio slices).reverse).shuffle(t =>
        (t + nextPhase) % frequencyRatio
      )(i)
      wrdataPhase(i).wrDataMask := Vec(io.input.payload.mask.subdivideIn(frequencyRatio slices).reverse).shuffle(t =>
        (t + nextPhase) % frequencyRatio
      )(i)
      if (dfiConfig.useWrDbi)
        wrdataPhase(i).wrDataDbiN := Vec(wrSrcDbiN.subdivideIn(frequencyRatio slices).reverse).shuffle(t =>
          (t + nextPhase) % frequencyRatio
        )(i)
    } else {
      wrdataPhase(i).valid := wrensHistory(nextPhase)(delayCyc + 1)
      wrdataPhase(i).wrData := RegNext(
        Vec(wrSrcData.subdivideIn(frequencyRatio slices).reverse).shuffle(t =>
          (t + nextPhase) % frequencyRatio
        )(i)
      )
      wrdataPhase(i).wrDataMask := RegNext(
        Vec(io.input.payload.mask.subdivideIn(frequencyRatio slices).reverse).shuffle(t =>
          (t + nextPhase) % frequencyRatio
        )(i)
      )
      if (dfiConfig.useWrDbi)
        wrdataPhase(i).wrDataDbiN := RegNext(
          Vec(wrSrcDbiN.subdivideIn(frequencyRatio slices).reverse).shuffle(t =>
            (t + nextPhase) % frequencyRatio
          )(i)
        )
    }
  }
  if (dfiConfig.useWrdataCsN) {
    for (i <- 0 until (frequencyRatio)) {
      if (i >= nextPhase) {
        io.outputCs(i).valid := wrensHistory(nextPhase)(delayCyc)
        io.outputCs(i).wrCs := csHistory(nextPhase)(delayCyc)
      } else {
        io.outputCs(i).valid := wrensHistory(nextPhase)(delayCyc + 1)
        io.outputCs(i).wrCs := csHistory(nextPhase)(delayCyc + 1)
      }
    }
  }
}
