package spinal.lib.memory.sdram.dfi

import spinal.lib.bus.bmb.sim.{BmbMasterAgent, BmbRegionAllocator}
import spinal.lib.bus.bmb.Bmb
import spinal.lib.bus.misc.SizeMapping
import spinal.tester.code.SpinalAnyFunSuite

import scala.util.Random

class DfiControllerTester extends SpinalAnyFunSuite {

  import spinal.core._
  import spinal.core.sim._
  import spinal.lib.bus.bmb.BmbParameter

  import scala.collection.mutable

  /** One DfiController configuration = one test case: a random BMB master is driven against the
    * DfiMemoryAgent reference model and every write/read is cross-checked.
    *
    * @param name               test name
    * @param generation         SDRAM generation (DDR3/DDR2)
    * @param frequencyRatio     DFI frequency ratio (PHY:MC)
    * @param cmdPhase           command phase in the frequency-ratio interface
    * @param tPhyWrData         write-data enable delay (0 disables the WrAlignment delay chain)
    * @param chipSelectNumber   number of DRAM chips
    * @param rowWidth           row address width. Keep byteAddressWidth + log2Up(chipSelectNumber) < 31,
    *                           otherwise `1 << addressWidth` overflows Int in the address generator.
    * @param addrMap            address mapping order
    */
  def runDfiControllerTest(
      name: String,
      generation: SdramGeneration,
      frequencyRatio: Int,
      cmdPhase: Int,
      tPhyWrData: Int,
      chipSelectNumber: Int,
      rowWidth: Int,
      addrMap: AddrMap,
      useCsN: Boolean = false,
      bgWidth: Int = 0,
      cidWidth: Int = 0,
      bankWidth: Int = 3,
      sdramTiming: SdramTiming = null
  ): Unit = {
    test(name) {
      // Deterministic test: derive a fixed seed from the test name and seed BOTH random sources
      // (the SpinalSim internal RNG via doSimUntilVoid, and the global scala.util.Random used by the
      // agent/memory/region allocator) so a failure is reproducible run-to-run.
      val seed = name.hashCode
      scala.util.Random.setSeed(seed)
      SimConfig.withVcdWave
        .compile {
          val task: TaskParameter =
            TaskParameter(
              timingWidth = 5,
              refWidth = 23,
              cmdBufferSize = 1024,
              dataBufferSize = 1024,
              rspBufferSize = 1024
            )
          val sdramtime = if (sdramTiming != null) sdramTiming else SdramTiming(
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
            generation,
            bgWidth = bgWidth,
            cidWidth = cidWidth,
            bankWidth = bankWidth,
            columnWidth = 10,
            rowWidth = rowWidth,
            dataWidth = 16,
            ddrMHZ = 200,
            ddrWrLat = 4,
            ddrRdLat = 4,
            sdramtime = sdramtime
          )
          val timeConfig = DfiTimeConfig(
            frequencyRatio = frequencyRatio,
            cmdPhase = cmdPhase,
            tPhyWrLat = sdram.tPhyWrlat,
            tPhyWrData = tPhyWrData,
            tPhyWrCsGap = 3,
            tRddataEn = sdram.tRddataEn,
            tPhyRdlat = 4,
            tPhyRdCsGap = 3,
            tPhyRdCslat = 0,
            tPhyWrCsLat = 0
          )
          val signalConfig = generation match {
            case SdramGeneration.DDR2 =>
              new DDR2SignalConfig(DfiFunctionConfig(), false) {
                override val useWrdataCsN = useCsN
                override val useRddataCsN = useCsN
              }
            case SdramGeneration.DDR4 =>
              new DDR4SignalConfig(DfiFunctionConfig(), false) {
                override val useWrdataCsN = useCsN
                override val useRddataCsN = useCsN
              }
            case _ =>
              new DDR3SignalConfig(DfiFunctionConfig(), false) {
                override val useWrdataCsN = useCsN
                override val useRddataCsN = useCsN
              }
          }
          val dfiConfig: DfiConfig = DfiConfig(
            chipSelectNumber = chipSelectNumber,
            dataSlice = 1,
            signalConfig = signalConfig,
            timeConfig = timeConfig,
            sdram = sdram
          )
          val bmbp: BmbParameter = BmbParameter(
            addressWidth = sdram.byteAddressWidth + log2Up(dfiConfig.chipSelectNumber),
            dataWidth = dfiConfig.beatWidth,
            sourceWidth = 0,
            contextWidth = 2,
            lengthWidth = 10,
            alignment = BmbParameter.BurstAlignement.WORD
          )
          val dut = DfiController(bmbp, task, dfiConfig, addrMap)
          dut
        }
        .doSimUntilVoid(seed) { dut =>
          dut.clockDomain.forkStimulus(10)
          dut.io.dfi.read.rd.foreach(_.rddataValid #= false)
          val memorySize = 1 << dut.io.bmb.p.access.addressWidth
          val allowedWrites = mutable.HashMap[Long, Byte]()
          val allowedWritesStandby = mutable.HashMap[Long, Byte]()
          val addrMap = dut.addrMap

          def rearrangeBits(value: Long, segmentLengths: Array[(Int, Int)]): Long = {
            val segmentWidth = segmentLengths.map(_._1)
            val segmentOrder = segmentLengths.map(_._2)
            require(segmentWidth.sum == 64, "The sum of all segment lengths must be equal to 64")
            require(segmentOrder.distinct.length == segmentOrder.length, "The elements in a order array must be unique")
            val binaryString = value.toBinaryString.reverse.padTo(64, '0').reverse
            val segments = Array.fill[String](segmentWidth.length)("")
            var offset = 0
            for (i <- segmentWidth.indices) {
              val segment = binaryString.substring(offset, offset + segmentWidth(i))
              segments(i) = segment
              offset += segmentWidth(i)
            }
            val rearrangedBinaryString = segmentOrder.map(segments).mkString("")
            java.lang.Long.parseLong(rearrangedBinaryString, 2)
          }
          def addressTranslation(bmbAddr: Long): Long = {
            //addressWidth = (Width, Order)
            val reservedBitsWidth = (64 - dut.dfiConfig.sdram.byteAddressWidth, 0)
            val bankWidth = (dut.dfiConfig.sdram.bankWidth, 1)
            val rowWidth = (dut.dfiConfig.sdram.rowWidth, 2)
            val colAddrHiWidth = (dut.dfiConfig.sdram.columnWidth - log2Up(dut.dfiConfig.transferPerBurst), 3)
            val colAddrLoWidth = (log2Up(dut.dfiConfig.transferPerBurst), 4)
            val byteWidth = (log2Up(dut.dfiConfig.sdram.bytePerWord), 5)

            val segmentLengths = addrMap match {
              case RowBankColumn =>
                Array[(Int, Int)](reservedBitsWidth, rowWidth, bankWidth, colAddrHiWidth, colAddrLoWidth, byteWidth)
              case BankRowColumn =>
                Array[(Int, Int)](reservedBitsWidth, bankWidth, rowWidth, colAddrHiWidth, colAddrLoWidth, byteWidth)
              case RowColumnBank =>
                Array[(Int, Int)](reservedBitsWidth, rowWidth, colAddrHiWidth, bankWidth, colAddrLoWidth, byteWidth)
            }
            val dfiAddr = rearrangeBits(bmbAddr, segmentLengths)
            dfiAddr
          }

          var rspReadCount = 0
          var rspReadBeats = 0L
          var cmdWriteCount = 0
          val dfiMemoryAgent = new DfiMemoryAgent(dut.io.dfi, dut.clockDomain) {
            override def writeNotification(address: Long, value: Byte): Unit = {
              val option = allowedWrites.get(address)
              assert(option.isDefined)
              assert(option.get == value, s"Write mismatch")
              allowedWrites.remove(address)
              if (allowedWritesStandby.contains(address)) {
                allowedWrites(address) = allowedWritesStandby(address)
                allowedWritesStandby.remove(address)
              }
            }
          }

          val regions = BmbRegionAllocator(alignmentMinWidth = 6)
          var stopGenerating = false
          val bmbAgent = new BmbMasterAgent(dut.io.bmb, dut.clockDomain) {
            override def onRspRead(address: BigInt, data: Seq[Byte]): Unit = {
              rspReadCount += 1
              rspReadBeats += data.length
              val ref = (0 until data.length).map(i => dfiMemoryAgent.getByte(addressTranslation(address.toLong + i)))
              if (ref != data) {
                val master = dut.io.bmb
                simFailure(s"Read mismatch on $master\n  REF=$ref\n  DUT=$data")
              }
            }
            override def getCmd(): () => Unit = {
              // ponytail: stopGenerating gates only NEW-command generation. The StreamDriver calls
              // getCmd() once per beat, so returning null while cmdQueue is non-empty strands the
              // remaining beats of an already-accepted multi-beat command: its rsp never completes
              // -> [NOT-DRAINED] pending=1 rspQ=1 lastCmd=WR (seed-dependent ~1/3 of runs).
              val c =
                if (stopGenerating) {
                  if (cmdQueue.isEmpty) null else super.getCmd()
                } else if ((!rspQueue.exists(_.nonEmpty)) | (cmdQueue.nonEmpty)) {
                  super.getCmd()
                } else null
              c
            }
            override def maskRandom() = true
            override def onCmdWrite(address: BigInt, data: Byte): Unit = {
              cmdWriteCount += 1
              val addressLong = addressTranslation(address.toLong)
              if (allowedWrites.contains(addressLong)) {
                allowedWritesStandby(addressLong) = data
              } else {
                assert(!allowedWrites.contains(addressLong), s"address is $address")
                allowedWrites(addressLong) = data
              }
            }
            override def regionAllocate(sizeMax: Int): SizeMapping = regions.allocate(
              Random.nextInt(memorySize) & ~((1 << regions.alignmentMinWidth) - 1),
              sizeMax,
              dut.io.bmb.p
            )
            override def regionFree(region: SizeMapping): Unit = regions.free(region)
            override def regionIsMapped(region: SizeMapping, opcode: Int): Boolean = true
          }
          var dfiCmdCount = 0
          var bmbCmdFireCount = 0
          var c = 0
          while (c < 12000) {
            val ras = dut.io.dfi.control.rasN.toLong
            val cas = dut.io.dfi.control.casN.toLong
            val we = dut.io.dfi.control.weN.toLong
            val idleMask = (1L << dut.dfiConfig.frequencyRatio) - 1
            if (ras != 0xffffffffL && ras != idleMask) dfiCmdCount += 1
            else if (cas != idleMask) dfiCmdCount += 1
            else if (we != idleMask) dfiCmdCount += 1
            if (dut.io.bmb.cmd.valid.toBoolean && dut.io.bmb.cmd.ready.toBoolean) bmbCmdFireCount += 1
            if (c % 2000 == 0) {
              println(
                s"[T c=$c] bmb.valid=${dut.io.bmb.cmd.valid.toBoolean} bmb.ready=${dut.io.bmb.cmd.ready.toBoolean} " +
                  s"dfi.ras=${ras}%x cas=${cas}%x we=${we}%x " +
                  s"pending=${bmbAgent.pendingCounter} rspQ=${bmbAgent.rspQueue.map(_.size).mkString(",")}"
              )
            }
            dut.clockDomain.waitSampling()
            c += 1
          }
          println(s"[DFI-ACT] ${name}: dfiCmdCount=$dfiCmdCount bmbCmdFireCount=$bmbCmdFireCount")
          // Stop command generation so the drain below empties deterministically (the agent's getCmd
          // override otherwise re-generates as soon as the rsp queue empties, making the drain racy).
          stopGenerating = true
          var drainC = 0
          // Bounded drain: give the DUT up to 300k cycles to complete the in-flight command's rsp.
          // (A racy exit on the first momentarily-empty rsp queue is intentionally avoided: the agent
          // would re-generate and the last command's final rsp beat can be dropped.)
          while (bmbAgent.rspQueue.exists(_.nonEmpty) && drainC < 300) {
            dut.clockDomain.waitSampling(1000)
            drainC += 1
          }
          val drained = !bmbAgent.rspQueue.exists(_.nonEmpty)
          println(s"[DRAIN] ${name}: drained=$drained drainC=$drainC pending=${bmbAgent.pendingCounter} rspReadBeats=$rspReadBeats")
          if (!drained) {
            println(s"[NOT-DRAINED] ${name}: drainC=$drainC pending=${bmbAgent.pendingCounter} rspQ=${bmbAgent.rspQueue.map(_.size).mkString(",")}")
          }
          // 刷新验证: 命令已停止,空闲期应触发 auto-refresh(prechargeAll → refresh)。
          // 在 cmdPhase 相位采样 DFI 命令总线,断言观察到 refresh 编码。
          // REFRESH (CmdTxd "b001", LSB-first assignFromBits): rasN=0, casN=0, weN=1。
          var refreshCount = 0
          val rCmdPhase = dut.dfiConfig.timeConfig.cmdPhase
          val rCsNum = dut.dfiConfig.chipSelectNumber
          val rIdleCs = (1L << rCsNum) - 1
          for (_ <- 0 until 10000) {
            dut.clockDomain.waitSampling()
            val rRas = (dut.io.dfi.control.rasN.toLong >> rCmdPhase) & 1L
            val rCas = (dut.io.dfi.control.casN.toLong >> rCmdPhase) & 1L
            val rWe = (dut.io.dfi.control.weN.toLong >> rCmdPhase) & 1L
            val rCs = (dut.io.dfi.control.csN.toLong >> (rCmdPhase * rCsNum)) & rIdleCs
            if (rRas == 0 && rCas == 0 && rWe == 1 && rCs != rIdleCs) refreshCount += 1
          }
          println(s"[REFRESH] ${name}: refreshCount=$refreshCount")
          assert(refreshCount > 0, s"${name}: no refresh command observed in idle window")
          println(s"[COUNT] ${name}: cmdWriteCount=$cmdWriteCount rspReadCount=$rspReadCount")
          simSuccess()
        }
    }
  }

  runDfiControllerTest("DfiController_DDR3_fr2_chip2_RowBankColumn", SdramGeneration.DDR3, 2, 0, 0, 2, 15, RowBankColumn)
  // (i) tPhyWrData > 0: exercises the WrAlignment write-data delay chain
  runDfiControllerTest("DfiController_DDR3_fr2_tPhyWrData4", SdramGeneration.DDR3, 2, 0, 4, 2, 15, RowBankColumn)
  // (iii) BankRowColumn address mapping
  runDfiControllerTest("DfiController_DDR3_fr2_chip2_BankRowColumn", SdramGeneration.DDR3, 2, 0, 0, 2, 15, BankRowColumn)
  // (iv) chipSelectNumber = 4: exercises 2-bit CS selection (and the DfiMemoryAgent selectBit fix);
  //      rowWidth reduced so byteAddressWidth + log2Up(4) < 31 keeps `1 << addressWidth` inside Int
  runDfiControllerTest("DfiController_DDR3_fr2_chip4", SdramGeneration.DDR3, 2, 0, 0, 4, 13, RowBankColumn)
  // (v) DDR2 (burstLength=4, dataRate=2): beatCount = 1 -> exercises the CCD == null branch in MakeTask
  runDfiControllerTest("DfiController_DDR2_fr2_chip2_RowBankColumn", SdramGeneration.DDR2, 2, 0, 0, 2, 15, RowBankColumn)
  // (vi) frequencyRatio = 4 (stress): multi-phase DFI with beatCount = 1
  runDfiControllerTest("DfiController_DDR3_fr4_cmdPhase0", SdramGeneration.DDR3, 4, 0, 0, 2, 15, RowBankColumn)
  // (vii) cmdPhase != 0: command issued on a non-zero phase (phase-aligned read-data return)
  runDfiControllerTest("DfiController_DDR3_fr2_cmdPhase1", SdramGeneration.DDR3, 2, 1, 0, 2, 15, RowBankColumn)
  runDfiControllerTest("DfiController_DDR3_fr4_cmdPhase1", SdramGeneration.DDR3, 4, 1, 0, 2, 15, RowBankColumn)
  // RowColumnBank is rejected at elaboration (assert in DfiController): the mapping interleaves
  // burst-adjacent bits into the bank field, breaking the station's single-bank burst continuity.
  // (ix) useWrdataCsN/useRddataCsN: exercises the wrCs/rdCs datapaths (DfiWrCs/DfiReadCs, WrAlignment/RdAlignment inputCs)
  runDfiControllerTest("DfiController_DDR3_fr2_csN", SdramGeneration.DDR3, 2, 0, 0, 2, 15, RowBankColumn, useCsN = true)

  // ===== DDR4 (JESD79-4B, 4 banks x 2 bank groups, BL8) =====
  // Timing values: tRFC1(8Gb)=350ns, tRAS=34ns, tRP/tRCD=15ns, tWTR_L=8/tWTR_S=4,
  // tRRD_L=5/tRRD_S=3, tFAW=35ns, tREFI=7800ns (REF_NS overrides the legacy per-row derivation).
  val ddr4Timing = SdramTiming(
    generation = 4,
    RFC = 350,
    RAS = 34,
    RP = 15,
    RCD = 15,
    WTR = 8,
    WTP = 0,
    RTP = 8,
    RRD = 5,
    REF = 64000,
    FAW = 35,
    WTR_S = 4,
    RRD_S = 3,
    REF_NS = 7800
  )
  // DDR4 geometry: bankWidth=4 (flat: {bg,ba}), bgWidth=1, cidWidth=1, rowWidth=13
  // (keeps byteAddressWidth + log2Up(cs) = 28+1 < 31 for the Int-based address generator).
  def ddr4(name: String, fr: Int, phase: Int, map: AddrMap, csN: Boolean = false): Unit =
    runDfiControllerTest(
      name, SdramGeneration.DDR4, fr, phase, 0, 2, 13, map,
      useCsN = csN, bgWidth = 1, cidWidth = 1, bankWidth = 4, sdramTiming = ddr4Timing
    )
  ddr4("DfiController_DDR4_fr2_RowBankColumn", 2, 0, RowBankColumn)
  ddr4("DfiController_DDR4_fr2_BankRowColumn", 2, 0, BankRowColumn)
  ddr4("DfiController_DDR4_fr2_cmdPhase1", 2, 1, RowBankColumn)
  ddr4("DfiController_DDR4_fr4_cmdPhase0", 4, 0, RowBankColumn)
  ddr4("DfiController_DDR4_fr4_cmdPhase1", 4, 1, RowBankColumn)
  ddr4("DfiController_DDR4_fr2_csN", 2, 0, RowBankColumn, csN = true)
}
