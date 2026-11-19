package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.core.sim._
import spinal.lib.sim.SparseMemory

import scala.collection.mutable
class DfiMemoryAgent(ctrl: DfiControlInterface, wr: DfiWriteInterface, rd: DfiReadInterface, clockDomain: ClockDomain) {
  val memory = SparseMemory()

  assert(
    ctrl.config == wr.config && rd.config == wr.config,
    "The config of DfiMemoryAgent is different."
  )

  val busConfig = ctrl.config
  val csCount = busConfig.chipSelectNumber
  val phaseCount = busConfig.frequencyRatio
  val cmdPhase = busConfig.timeConfig.cmdPhase
  // Phase where the DUT asserts dfi_rddata_en / dfi_wrdata_en for a command issued at cmdPhase,
  // matching RdDataRxd.nextPhase / WrDataTxd.nextPhase. The latency checks below assert on these
  // phases rather than on the (fixed) phase of the countdown iteration, which is only correct for
  // cmdPhase == 0.
  val rdNextPhase = (cmdPhase + busConfig.timeConfig.tRddataEn) % phaseCount
  val wrNextPhase = (cmdPhase + busConfig.timeConfig.tPhyWrLat) % phaseCount
  val oneTaskDataNumber = busConfig.transferPerBurst / busConfig.dataRate
  val oneTaskByteNumber = busConfig.bytePerBurst
  val bankWidth = busConfig.sdram.bankWidth
  val rowWidth = busConfig.sdram.rowWidth
  val columnWidth = busConfig.sdram.columnWidth
  val isDdr4 = busConfig.sdram.generation == SdramGeneration.DDR4
  val bgWidth = if (isDdr4) busConfig.sdram.bgWidth else 0
  val dfiBankWidth = busConfig.dfiBankWidth
  // DDR4 mode-register file: MR0..MR4 per rank (op code as written by MRS), plus flags.
  val modeRegisters = mutable.Map[Int, mutable.Map[Int, Long]]() // rank -> (mrIndex -> op)
  var zqclSeen = false
  var initChecked = false

  val ckeProxy = ctrl.cke.simProxy()
  val csNProxy = ctrl.csN.simProxy()
  val rasNProxy = ctrl.rasN.simProxy()
  val casNProxy = ctrl.casN.simProxy()
  val weNProxy = ctrl.weN.simProxy()
  val actNProxy = if (isDdr4) ctrl.actN.simProxy() else null
  val bgProxy = if (isDdr4) ctrl.bg.simProxy() else null

  val rowAddrQueue = mutable.Queue[Long]()
  val bankQueue = mutable.Queue[Long]()
  val wrEnQueue = mutable.Queue[Boolean]()
  val wrAddrQueue = mutable.Queue[Long]()
  val wrByteQueue = mutable.Queue[Byte]()
  val wrLatQueues = mutable.Queue[Int]()
  val rdEnQueue = mutable.Queue[(Boolean, Int)]()
  val rdDataQueue = mutable.Queue[BigInt]()
  val rProcess = Array.fill(phaseCount)(mutable.Queue[(BigInt) => Unit]())
  val rdLatQueues = mutable.Queue[Int]()
  // dfi_wrdata_cs_n / dfi_rddata_cs_n 断言用的命令级 rank 队列(仅 useWrdataCsN/useRddataCsN 时使用)
  val wrCsQueue = mutable.Queue[Int]()
  val rdCsQueue = mutable.Queue[Int]()

  var rowAddr: Long = 0
  var columnAddr: Long = 0
  var bank: Long = 0
  var byteAddr: Long = 0
  var writeValid: Boolean = false
  var oneTakeDataCounter: Int = 0
  var rdDataByte: Int = 0
  var rdData: BigInt = 0
  var rdByteAddress: Long = 0
  var rdValidPhase: Int = 0
  var curRdCs: Int = 0
  var rdElementsSinceCmd: Int = 0
  // dfi_wrdata_cs_n 匹配的超时计数(命令级,数据窗口内应命中)
  var wrCsVerifyTimeout = 0
  // dfi_rddata_cs_n 输出的拍级对齐依赖 RdAlignment 的 FIFO 缓冲延迟(不透明),故改为轮询:
  // 每命令入队 1 个期望 cs(命令级粒度,与 csHold 的保持语义一致),在 rdCs 输出窗口内命中即出队。
  val rdCsExpectQueue = mutable.Queue[Int]()
  var rdCsVerifyTimeout = 0

  def this(bus: Dfi, clockDomain: ClockDomain) {
    this(bus.control, bus.write, bus.read, clockDomain);
  }

  def getByteAsInt(address: Long) = getByte(address).toInt & 0xff

  def getByte(address: Long) = memory.read(address)

  /** Extract the partIndex-th field of bitNumber bits from bigInt and return its bits as a Boolean array.
    * Replacement for the previous string-grouping version, which asserted isPow2(bitLength) (crashing on
    * values such as 0b101) and hard-coded a 2-bit return (silently dropping chips beyond the first two
    * when chipSelectNumber > 2).
    */
  def selectBit(bigInt: BigInt, partIndex: Int, bitNumber: Int) = {
    val group = (bigInt >> (partIndex * bitNumber)) & ((BigInt(1) << bitNumber) - 1)
    Array.tabulate(bitNumber)(i => group.testBit(i))
  }

  def writeDataRxd(wrEn: collection.immutable.IndexedSeq[Boolean], wrData: collection.immutable.IndexedSeq[Long]) = {
    // dfi_wrdata_cs_n 断言: wrdataCsN 与 wrdata_en 同拍输出(经 WrDataTxd 延迟链),
    // 故在 wrdataEn 有效拍匹配命令级期望 cs(每命令入队 1 个),命中即出队。
    if (busConfig.signalConfig.useWrdataCsN && wrCsQueue.nonEmpty) {
      val expCs = wrCsQueue.head
      val matched = (0 until phaseCount).exists { i =>
        wrEn(i) && {
          val csn = wr.wr(i).wrdataCsN.toInt
          csn != (1 << busConfig.chipSelectNumber) - 1 && (csn & (1 << expCs)) == 0
        }
      }
      if (matched) {
        wrCsQueue.dequeue()
        wrCsVerifyTimeout = 0
      } else {
        wrCsVerifyTimeout += 1
        assert(wrCsVerifyTimeout < 1000, s"wrdataCsN output never matched rank $expCs")
      }
    }
    for (phase <- 0 until phaseCount) {
      wrEnQueue.enqueue(wrEn(phase))
      if (wrEnQueue.length == busConfig.timeConfig.tPhyWrData + 1) {
        writeValid = wrEnQueue.dequeue()
      }
      if (writeValid) {
        for (j <- 0.until(busConfig.phyIoWidth / 8).reverse) {
          wrByteQueue.enqueue((wrData(phase) >> j * 8).toByte)
        }
        if (oneTakeDataCounter == oneTaskDataNumber) {
          for (i <- 0 until (busConfig.bytePerBurst)) {
            val wrAddr = wrAddrQueue.dequeue()
            val wrByte = wrByteQueue.dequeue()
            writeNotification(wrAddr, wrByte)
            setByte(wrAddr, wrByte)
          }
          oneTakeDataCounter = 0
        } else {
          oneTakeDataCounter = oneTakeDataCounter + 1
        }
      }
      if(wrLatQueues.nonEmpty) {
        // tPhyWrLat is expressed in DFI clock cycles: the per-phase iteration below decrements once per
        // DFI cycle (frequencyRatio iterations per MC cycle), which matches WrDataTxd's
        // delayCyc = tPhyWrLat / frequencyRatio (MC cycles).
        for(i <- 0 until(wrLatQueues.length)){
          val wrLat = wrLatQueues.dequeue()
          if(wrLat == 0){
            assert(wrEn(wrNextPhase), "The parameter tPhyWrLat is not satisfied ")
          }else{
            wrLatQueues.enqueue(wrLat - 1)
          }
        }
      }
    }
  }

  def writeNotification(address: Long, value: Byte) = {} // memory.write(address, value)

  def setByte(address: Long, value: Byte) = memory.write(address, value)

  def readDataTxd(rdEn: collection.immutable.IndexedSeq[Boolean]) = {
    // dfi_rddata_cs_n 轮询断言: rdCs 输出(经 RdAlignment FIFO 延迟)非全 1 时,
    // 必须与当前期望命令的 rank 一致(低有效)。命中即出队。
    if (busConfig.signalConfig.useRddataCsN && rdCsExpectQueue.nonEmpty) {
      val expCs = rdCsExpectQueue.head
      val matched = (0 until phaseCount).exists { i =>
        val csn = rd.rdCs(i).rddataCsN.toInt
        csn != (1 << busConfig.chipSelectNumber) - 1 && (csn & (1 << expCs)) == 0
      }
      if (matched) {
        rdCsExpectQueue.dequeue()
        rdCsVerifyTimeout = 0
      } else {
        rdCsVerifyTimeout += 1
        // rdCs 输出经 RdAlignment FIFO 延迟,窗口放宽到 1000 拍(命令级等待)
        assert(rdCsVerifyTimeout < 1000, s"rddataCsN output never matched rank $expCs")
      }
    }
    rd.rd.foreach(_.rddataValid #= false)
    for ((en, phase) <- rdEn.zipWithIndex) {
      if (rProcess(phase).nonEmpty && rdDataQueue.nonEmpty) {
        rdEnQueue.dequeue()
        rProcess(phase).dequeue().apply(rdDataQueue.dequeue())
        // dfi_rddata_cs_n 断言用的命令级 rank 队列: 每命令首个数据元素取队首。
        // 命令数据窗口 = beatCount*phaseCount 个元素,窗口起点取队首。
        if (busConfig.signalConfig.useRddataCsN) {
          if (rdElementsSinceCmd == 0 && rdCsQueue.nonEmpty) {
            curRdCs = rdCsQueue.dequeue()
            rdCsExpectQueue.enqueue(curRdCs)
          }
          rdElementsSinceCmd += 1
          if (rdElementsSinceCmd == busConfig.beatCount * phaseCount) rdElementsSinceCmd = 0
        }
      }
      if (en) {
        rdEnQueue.enqueue((true, phase))
        for ((process, i) <- rProcess.zipWithIndex) {
          if (i == rdValidPhase) {
            process.enqueue { (bigInt: BigInt) =>
              rd.rd(i).rddataValid #= true
              rd.rd(i).rddata #= bigInt
            }
          }
        }
        rdValidPhase = (rdValidPhase + 1) % phaseCount
      }
      if(rdLatQueues.nonEmpty) {
        // tRddataEn is expressed in DFI clock cycles; see the same convention in writeDataRxd.
        for(i <- 0 until(rdLatQueues.length)){
          val rdLat = rdLatQueues.dequeue()
          if(rdLat == 0){
            assert(rdEn(rdNextPhase), "The parameter tRddataEn is not satisfied ")
          }else{
            rdLatQueues.enqueue(rdLat - 1)
          }
        }
      }
    }
  }

  // Dual-path verification: monotonic cycle stamp for the exported command log.
  var agentCycle = 0L
  clockDomain.onSamplings { agentCycle += 1 }

  clockDomain.onSamplings {
    val cke = selectBit(ckeProxy.toBigInt.asInstanceOf[BigInt], cmdPhase, csCount)
    val csN = selectBit(csNProxy.toBigInt.asInstanceOf[BigInt], cmdPhase, csCount)
    val ras = rasNProxy.toBigInt.asInstanceOf[BigInt].testBit(cmdPhase)
    val cas = casNProxy.toBigInt.asInstanceOf[BigInt].testBit(cmdPhase)
    val weN = weNProxy.toBigInt.asInstanceOf[BigInt].testBit(cmdPhase)
    // The command at phase cmdPhase only owns the [cmdPhase*width, (cmdPhase+1)*width) slice of the
    // phase-concatenated address/bank buses; reading the whole bus would decode a shifted address for
    // cmdPhase != 0.
    val cmdAddr = (ctrl.address.toLong >> (cmdPhase * busConfig.addressWidth)) & ((1L << busConfig.addressWidth) - 1)
    val cmdBank = (ctrl.bank.toLong >> (cmdPhase * busConfig.bankWidth)) & ((1L << busConfig.bankWidth) - 1)
    val wrEn = wr.wr.map(_.wrdataEn.toBoolean).toIndexedSeq
    val wrData = wr.wr.map(_.wrdata.toLong).toIndexedSeq
    val rdEn = rd.rden.map(_.toBoolean).toIndexedSeq

    if (isDdr4) {
      // ---- DDR4 decode (JESD79-4B Table 17, values are active-low pin states) ----
      val actN = actNProxy.toBigInt.asInstanceOf[BigInt].testBit(cmdPhase)
      val cmdBg = (bgProxy.toBigInt.asInstanceOf[BigInt] >> (cmdPhase * bgWidth)) & ((1L << bgWidth) - 1)
      val a10 = (cmdAddr >> 10) & 1L
      // Flat bank index as the DUT's address map produced it: {bg, ba}
      def flatBank(ba: Long): Long = ((cmdBg << dfiBankWidth) | (ba & ((1L << dfiBankWidth) - 1))).toLong

      for ((enPerChip, idPerChip) <- cke.zip(csN).map(t => t._1 && !t._2).zipWithIndex) {
        // All ras/cas/weN variables are the actual (active-low) pin states from Table 17.
        val mrs = enPerChip && actN && !ras && !cas && !weN
        val act = enPerChip && !actN
        val wrCmd = enPerChip && actN && ras && cas && !weN
        val rdCmd = enPerChip && actN && ras && !cas && weN
        val preCmd = enPerChip && actN && !ras && cas && !weN
        val refCmd = enPerChip && actN && !ras && !cas && weN
        val zqcl = enPerChip && actN && ras && cas && weN && a10 == 1L

        if (mrs) {
          // MR select = {BG0, BA1:BA0}; the address field carries the OP code
          val mrIndex = (((cmdBg >> 0) & 1L) << 2) | (cmdBank & 3L)
          if (mrIndex != 7) { // 7 = RCW1, reserved
            val rankMr = modeRegisters.getOrElseUpdate(idPerChip, mutable.Map())
            rankMr(mrIndex.toInt) = cmdAddr
          }
        }
        if (zqcl) zqclSeen = true

        // Dual-path verification: export every decoded DDR4 command so an independent
        // decoder (Python, straight from JESD79-4B Table 17) can be diffed against it.
        def hex(v: Long) = java.lang.Long.toHexString(v)
        if (mrs) {
          val mr = (((cmdBg >> 0) & 1L) << 2) | (cmdBank & 3L)
          println(s"[AGENTCMD] $agentCycle MRS $mr ${hex(cmdAddr)}")
        }
        if (act) println(s"[AGENTCMD] $agentCycle ACT ${flatBank(cmdBank)} ${hex(cmdAddr)}")
        if (wrCmd) println(s"[AGENTCMD] $agentCycle WR ${flatBank(cmdBank)} ${hex(cmdAddr)}")
        if (rdCmd) println(s"[AGENTCMD] $agentCycle RD ${flatBank(cmdBank)} ${hex(cmdAddr)}")
        if (preCmd) {
          // A10=H is PREA (all banks): the bank field is don't-care, export -1 so the
          // independent decoder can match without comparing garbage bank bits.
          if (a10 == 1L) println(s"[AGENTCMD] $agentCycle PRE -1 -1")
          else println(s"[AGENTCMD] $agentCycle PRE ${flatBank(cmdBank)} ${hex(cmdAddr)}")
        }
        if (refCmd) println(s"[AGENTCMD] $agentCycle REF -1 -1")
        if (zqcl) println(s"[AGENTCMD] $agentCycle ZQCL -1 -1")

        // Traffic before the init sequence (MRS file + ZQCL) is a controller bug: a DDR4 SDRAM
        // ignores commands until its mode registers and ZQ calibration are set up.
        if ((act || wrCmd || rdCmd) && !initChecked) {
          assert(
            modeRegisters.contains(idPerChip) && zqclSeen,
            s"DDR4 rank $idPerChip saw ACT/WR/RD before MRS+ZQCL init completed"
          )
          initChecked = true
        }

        if (act) {
          rowAddrQueue.enqueue(cmdAddr)
          bankQueue.enqueue(flatBank(cmdBank))
        }
        if (rowAddrQueue.nonEmpty) rowAddr = rowAddrQueue.dequeue()
        if (bankQueue.nonEmpty) bank = bankQueue.dequeue()

        if (wrCmd) {
          columnAddr = cmdAddr & ((1L << columnWidth) - 1)
          byteAddr =
            ((idPerChip << busConfig.sdram.wordAddressWidth) + (bank << (columnWidth + rowWidth)) + (rowAddr << columnWidth) + columnAddr) << log2Up(
              busConfig.sdram.bytePerWord
            )
          for (beat <- 0 until busConfig.beatCount) {
            for (i <- (0 until busConfig.bytePerBeat).reverse) {
              wrAddrQueue.enqueue(byteAddr + beat * busConfig.bytePerBeat + i)
            }
          }
          wrLatQueues.enqueue(busConfig.timeConfig.tPhyWrLat)
          if (busConfig.signalConfig.useWrdataCsN) wrCsQueue.enqueue(idPerChip)
        }
        if (rdCmd) {
          columnAddr = cmdAddr & ((1L << columnWidth) - 1)
          byteAddr =
            ((idPerChip << busConfig.sdram.wordAddressWidth) + (bank << (columnWidth + rowWidth)) + (rowAddr << columnWidth) + columnAddr) << log2Up(
              busConfig.sdram.bytePerWord
            )
          for (beat <- 0 until busConfig.beatCount) {
            for (phase <- 0 until phaseCount reverse) {
              for (j <- 0 until busConfig.phyIoWidth / 8) {
                rdByteAddress = byteAddr + beat * busConfig.bytePerBeat + phase * busConfig.phyIoWidth / 8 + j
                rdDataByte = getByteAsInt(rdByteAddress)
                rdData |= BigInt(rdDataByte) << (j * 8)
              }
              rdDataQueue.enqueue(rdData)
              rdData = 0
            }
          }
          rdLatQueues.enqueue(busConfig.timeConfig.tRddataEn)
          if (busConfig.signalConfig.useRddataCsN) rdCsQueue.enqueue(idPerChip)
        }
        // preCmd/refCmd need no memory-side action (refresh/precharge don't move data);
        // the testbench observes them on the bus directly.
      }
    } else {
      // ---- Legacy decode: unchanged ----
      for ((enPerChip, idPerChip) <- cke.zip(csN).map(t => t._1 && !t._2).zipWithIndex) {
        // cmd and address
        val active = enPerChip & !ras & cas & weN
        val write = enPerChip & ras & !cas & !weN
        val read = enPerChip & ras & !cas & weN
        if (active) {
          rowAddrQueue.enqueue(cmdAddr)
          bankQueue.enqueue(cmdBank)
        }
        if (rowAddrQueue.nonEmpty) {
          rowAddr = rowAddrQueue.dequeue()
        }
        if (bankQueue.nonEmpty) {
          bank = bankQueue.dequeue()
        }
        // write cmd
        if (write) {
          columnAddr = cmdAddr & (1 << columnWidth) - 1
          byteAddr =
            ((idPerChip << busConfig.sdram.wordAddressWidth) + (bank << (columnWidth + rowWidth)) + (rowAddr << columnWidth) + columnAddr) << log2Up(
              busConfig.sdram.bytePerWord
            )
          for (beat <- 0 until (busConfig.beatCount)) {
            for (i <- (0 until (busConfig.bytePerBeat)).reverse) {
              wrAddrQueue.enqueue(byteAddr + beat * busConfig.bytePerBeat + i)
            }
          }
          wrLatQueues.enqueue(busConfig.timeConfig.tPhyWrLat)
          if (busConfig.signalConfig.useWrdataCsN) wrCsQueue.enqueue(idPerChip)
        }

        // read cmd
        if (read) {
          columnAddr = cmdAddr & (1 << columnWidth) - 1
          byteAddr =
            ((idPerChip << busConfig.sdram.wordAddressWidth) + (bank << (columnWidth + rowWidth)) + (rowAddr << columnWidth) + columnAddr) << log2Up(
              busConfig.sdram.bytePerWord
            )
          for (beat <- (0 until (busConfig.beatCount))) {
            for (phase <- 0.until(phaseCount).reverse) {
              for (j <- 0 until (busConfig.phyIoWidth / 8)) {
                rdByteAddress = byteAddr + beat * busConfig.bytePerBeat + phase * busConfig.phyIoWidth / 8 + j
                rdDataByte = getByteAsInt(rdByteAddress)
                rdData |= BigInt(rdDataByte) << (j * 8)
              }
              rdDataQueue.enqueue(rdData)
              rdData = 0
            }
          }
          rdLatQueues.enqueue(busConfig.timeConfig.tRddataEn)
          if (busConfig.signalConfig.useRddataCsN) rdCsQueue.enqueue(idPerChip)
        }
      }
    }
    writeDataRxd(wrEn, wrData)
    readDataTxd(rdEn)
  }
}
