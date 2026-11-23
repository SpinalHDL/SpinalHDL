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
      sdramTiming: SdramTiming = null,
      mr5: Int = 0x0000,
      mr4: Int = 0x0000,
      refreshMinCount: Int = 1,
      idleCycles: Int = 10000,
      caParity: Boolean = false,
      injectParityError: Boolean = false,
      initSteps: List[SdramInitStep] = Nil,
      useCtrlMsg: Boolean = false,
      ctrlMsgAckDelayCycles: Int = 0,
      bootTraining: Boolean = false,
      useLegacyWrLvl: Boolean = false,
      wrLvlDoneDelayCycles: Int = 0,
      tWrlvlResp: Int = 16
  ): Unit = {
    test(name) {
      val ctrlMsgEnabled = useCtrlMsg
      val bootEnabled = useCtrlMsg && bootTraining
      val legacyWrLvlEnabled = useLegacyWrLvl
      assert(!useCtrlMsg || generation == SdramGeneration.DDR4, "ctrlmsg training cases are DDR4-only")
      assert(
        !useLegacyWrLvl || generation == SdramGeneration.DDR2 || generation == SdramGeneration.DDR3,
        "legacy write-leveling cases are DDR2/DDR3-only"
      )
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
            tPhyWrCsLat = 0,
            tWrlvlResp = tWrlvlResp
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
                // DDR4 CA parity needs dfi_parity (MC out) + dfi_alert_n (DRAM in) present
                override val useParityIn = caParity
                override val useAlertN = caParity
                // DFI 6.0.1 Table 12 ctrlmsg channel: four wires + arbiter + trainBusy gates
                override val useCtrlMsg = ctrlMsgEnabled
              }
            case _ =>
              new DDR3SignalConfig(DfiFunctionConfig(), false) {
                override val useWrdataCsN = useCsN
                override val useRddataCsN = useCsN
                // P3 legacy write-leveling Stream handshake (dfi_wrlvl_en / dfi_wrlvl_resp)
                override val useLegacyWrLvl = legacyWrLvlEnabled
              }
          }
          val dfiConfig: DfiConfig = DfiConfig(
            chipSelectNumber = chipSelectNumber,
            dataSlice = 1,
            signalConfig = signalConfig,
            timeConfig = timeConfig,
            sdram = sdram,
            ddr4ModeRegs =
              if (generation == SdramGeneration.DDR4) Ddr4ModeRegs(mr4 = mr4, mr5 = mr5) else Ddr4ModeRegs(),
            initSteps = if (initSteps.isEmpty) None else Some(initSteps),
            bootTraining = bootTraining
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
          if (caParity) dut.caParityError.simPublic() // expose the sticky alert latch to the sim
          dut
        }
        .doSimUntilVoid(seed) { dut =>
          dut.clockDomain.forkStimulus(10)
          dut.io.dfi.read.rd.foreach(_.rddataValid #= false)
          // ALERT_n is DRAM-driven (DUT input): idle high until the agent pulls it low. Drive the
          // idle level immediately so the DUT never samples an undriven (0) input as an alert.
          if (caParity) dut.io.dfi.status.alertN #= (1 << dut.dfiConfig.frequencyRatio) - 1
          // ctrlmsg trainReq is testbench-driven (DUT input): an undriven Flow valid reads as
          // a random value in the simulator and would spuriously fire the arbiter every few
          // cycles. Idle it low for the whole run; fireTrainReq pulses it exactly once.
          if (ctrlMsgEnabled) {
            dut.io.trainReq.valid #= false
            dut.io.trainReq.msgCode #= 0
            dut.io.trainReq.rank #= 0
            // ctrlmsgAck is a DUT input the PHY agent only drives from its onSamplings callback,
            // which applies writes one delta-cycle AFTER the rising-edge eval — so at the very
            // first edges the DUT would sample Verilator's randReset(2) random t0 value (a t0
            // ack=1 makes the arbiter latch acked inside the resp window and lets a late real
            // ACK complete the handshake — the ctrlmsg_timeout full-suite flake). Drive the
            // idle level here, before the first edge, same rule as trainReq/alertN above.
            dut.io.dfiCtrlMsg.ctrlmsgAck #= false
          }
          if (legacyWrLvlEnabled) {
            // Same t0 rules as the ctrlmsg block above: the trigger Flow and the agent-driven
            // wrlvlResp are DUT inputs — an undriven valid reads random, and the agent only
            // drives resp from onSamplings (one delta-cycle late), so pin both idle here,
            // before the first edge.
            dut.io.legacyWrLvlTrigger.valid #= false
            dut.io.dfiWrLvl.wrlvlResp #= 0
          }
          // P2 boot-training enable: fired from t0 (BootTrainer latches it on the first
          // sampling edge), dropped once the FSM has certainly seen it. The orchestrator
          // then starts by itself at initDone — no further testbench stimulus.
          if (bootEnabled) {
            dut.io.bootTrainEnable.valid #= true
            dut.clockDomain.waitSampling(4)
            dut.io.bootTrainEnable.valid #= false
          }
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
          val dfiMemoryAgent = new DfiMemoryAgent(
            dut.io.dfi,
            dut.clockDomain,
            if (ctrlMsgEnabled) dut.io.dfiCtrlMsg else null,
            if (legacyWrLvlEnabled) dut.io.dfiWrLvl else null
          ) {
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
          // Error injection (case B): corrupt the parity bit as seen at the DRAM on the first
          // idle cycle at/after cycle 5000. The command bus stays intact, so no command is
          // dropped and data flow is unaffected; the agent must still raise ALERT_n and the DUT
          // must latch caParityError (asserted after the run below).
          if (injectParityError) dfiMemoryAgent.parityCorruptCycle = 5000
          // DFI 6.0.1 ctrlmsg: PHY ack latency knob (0 = within tctrlmsg_resp; a value beyond
          // the DUT's resp window exercises the DUT timeout path).
          dfiMemoryAgent.ctrlMsgAckDelayCycles = ctrlMsgAckDelayCycles
          // P3 legacy write-leveling: PHY wrLvlDone latency knob (bounded by tWrlvlResp = 16).
          dfiMemoryAgent.wrLvlDoneDelayCycles = wrLvlDoneDelayCycles

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
          // ---- DFI 6.0.1 ctrlmsg training stimulus (useCtrlMsg configs only) ----
          var trainDoneSeen = false
          var trainReqFired = false
          var trafficRspReadAtResume = 0L
          // ---- P3 legacy write-leveling stimulus (useLegacyWrLvl configs only) ----
          var wrLvlDoneSeen = false
          var wrLvlTriggerFired = false
          var wrLvlDoneAtCycle = -1
          var wrLvlRspReadAtResume = 0L
          // ---- P2 boot-training orchestrator observation (bootTraining configs only) ----
          val bootMsgSeq = mutable.ArrayBuffer[Long]() // msg code at each dfi_ctrlmsg_req rise
          var bootDonePulses = 0
          var bootPrevReq = false
          var bootHalted = false
          var bootHaltedAtCycle = -1
          var postHaltCmdCount = 0
          var rspReadBeatsAtHalt = 0L
          var rspReadBeatsAtBootDone = 0L
          def fireTrainReq(msgCode: Int, rank: Int): Unit = {
            // Quiesce the BMB master so the station is empty when the request lands: the
            // agent's training-window ban is strict (any ACT/WR/RD between req rise and ack
            // de-assert = controller bug, see DfiMemoryAgent). Then fire the request and
            // re-open the master on the same cycle: with the DUT gates intact the backlog
            // stalls at canSpawn (bus stays silent); without them it spawns within a few
            // cycles and the agent fails the sim.
            assert(!trainReqFired, "fireTrainReq called twice")
            trainReqFired = true
            stopGenerating = true
            var drainWait = 0
            while ((bmbAgent.pendingCounter != 0 || bmbAgent.rspQueue.exists(_.nonEmpty)) && drainWait < 100000) {
              dut.clockDomain.waitSampling()
              drainWait += 1
            }
            assert(
              bmbAgent.pendingCounter == 0 && !bmbAgent.rspQueue.exists(_.nonEmpty),
              "BMB traffic never drained before trainReq"
            )
            dut.clockDomain.waitSampling(16) // let the CmdTxd pipeline regs run dry
            // Re-open BMB generation 6 cycles BEFORE firing trainReq: the incoming command
            // then reaches the spawn point AFTER trainBusy rises (gate can hold it) but its
            // ACT would still land INSIDE the training window if the gates were missing —
            // the agent's traffic ban then fails the sim deterministically (counter-proof B).
            stopGenerating = false
            dut.clockDomain.waitSampling(6)
            dut.io.trainReq.valid #= true
            dut.io.trainReq.msgCode #= msgCode
            dut.io.trainReq.rank #= rank
            dut.clockDomain.waitSampling()
            dut.io.trainReq.valid #= false
            trafficRspReadAtResume = rspReadBeats
          }
          def fireLegacyWrLvl(): Unit = {
            // Same quiesce-and-reopen recipe as fireTrainReq: the backlog command must sit at
            // the spawn gate AFTER wrLvlBusy rises, so a missing/leaky gate spawns it inside
            // the window and the agent's ban fails the sim (counter-proof hook).
            assert(!wrLvlTriggerFired, "fireLegacyWrLvl called twice")
            wrLvlTriggerFired = true
            stopGenerating = true
            var drainWait = 0
            while ((bmbAgent.pendingCounter != 0 || bmbAgent.rspQueue.exists(_.nonEmpty)) && drainWait < 100000) {
              dut.clockDomain.waitSampling()
              drainWait += 1
            }
            assert(
              bmbAgent.pendingCounter == 0 && !bmbAgent.rspQueue.exists(_.nonEmpty),
              "BMB traffic never drained before the legacy wrLvl trigger"
            )
            dut.clockDomain.waitSampling(16) // let the CmdTxd pipeline regs run dry
            stopGenerating = false
            dut.clockDomain.waitSampling(6)
            dut.io.legacyWrLvlTrigger.valid #= true
            dut.clockDomain.waitSampling()
            dut.io.legacyWrLvlTrigger.valid #= false
            wrLvlRspReadAtResume = rspReadBeats
          }
          while (c < 12000) {
            val ras = dut.io.dfi.control.rasN.toLong
            val cas = dut.io.dfi.control.casN.toLong
            val we = dut.io.dfi.control.weN.toLong
            val idleMask = (1L << dut.dfiConfig.frequencyRatio) - 1
            if (ras != 0xffffffffL && ras != idleMask) dfiCmdCount += 1
            else if (cas != idleMask) dfiCmdCount += 1
            else if (we != idleMask) dfiCmdCount += 1
            if (dut.io.bmb.cmd.valid.toBoolean && dut.io.bmb.cmd.ready.toBoolean) bmbCmdFireCount += 1
            if (bootEnabled) {
              val reqNow = dut.io.dfiCtrlMsg.ctrlmsgReq.toBoolean
              if (reqNow && !bootPrevReq) bootMsgSeq += dut.io.dfiCtrlMsg.ctrlmsg.toLong
              bootPrevReq = reqNow
              if (dut.io.bootTrainDone.toBoolean) {
                bootDonePulses += 1
                rspReadBeatsAtBootDone = rspReadBeats
              }
              if (!bootHalted && dut.io.ctrlMsgError.toBoolean) bootHalted = true
              if (bootHalted) {
                if (bootHaltedAtCycle < 0) {
                  bootHaltedAtCycle = c
                  rspReadBeatsAtHalt = rspReadBeats
                }
                // Permanent-halt proof: after ctrlMsgError latches, ANY non-idle command-bus
                // cycle (same detector as dfiCmdCount) is a scheduled command = traffic
                // resumed = bug. (The BMB master self-throttles behind the frozen rsp queue,
                // so the injected backlog stays parked in the adapter — rspReadBeats must
                // also never move again.)
                if ((ras != 0xffffffffL && ras != idleMask) || cas != idleMask || we != idleMask) postHaltCmdCount += 1
              }
            }
            if (useCtrlMsg && !bootEnabled && c == 4000 && !trainReqFired) {
              // Mid-run training: quiesce, fire one message, resume traffic immediately.
              if (ctrlMsgAckDelayCycles > 0) fireTrainReq(CtrlMsgCode.ZQCAL, 0)
              else fireTrainReq(CtrlMsgCode.WRLVL, 0)
              var seen = 0
              while (seen < 200 && !trainDoneSeen && !dut.io.ctrlMsgError.toBoolean) {
                if (dut.io.trainDone.toBoolean) trainDoneSeen = true
                dut.clockDomain.waitSampling()
                seen += 1
              }
              if (ctrlMsgAckDelayCycles > 0) {
                assert(!trainDoneSeen, "trainDone pulsed although the PHY never acknowledged")
              } else {
                assert(trainDoneSeen, s"trainDone never pulsed after WRLVL request (waited $seen cycles)")
                assert(!dut.io.ctrlMsgError.toBoolean, "ctrlMsgError latched on the happy path")
              }
            }
            if (legacyWrLvlEnabled && c == 4000 && !wrLvlTriggerFired) {
              // Mid-run legacy write leveling: quiesce, fire the trigger, wait for done.
              fireLegacyWrLvl()
              var seen = 0
              while (seen < 400 && !wrLvlDoneSeen && !dut.io.legacyWrLvlError.toBoolean) {
                if (dut.io.legacyWrLvlDone.toBoolean) { wrLvlDoneSeen = true; wrLvlDoneAtCycle = (simTime() / 10).toInt }
                dut.clockDomain.waitSampling()
                seen += 1
              }
              assert(wrLvlDoneSeen, s"legacyWrLvlDone never pulsed after the trigger (waited $seen cycles)")
              assert(!dut.io.legacyWrLvlError.toBoolean, "wrLvlError latched on the happy path")
            }
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
          var lastRefreshCycle = -1
          var minRefreshInterval = Int.MaxValue
          val rCmdPhase = dut.dfiConfig.timeConfig.cmdPhase
          val rCsNum = dut.dfiConfig.chipSelectNumber
          val rIdleCs = (1L << rCsNum) - 1
          for (rc <- 0 until idleCycles) {
            dut.clockDomain.waitSampling()
            val rRas = (dut.io.dfi.control.rasN.toLong >> rCmdPhase) & 1L
            val rCas = (dut.io.dfi.control.casN.toLong >> rCmdPhase) & 1L
            val rWe = (dut.io.dfi.control.weN.toLong >> rCmdPhase) & 1L
            val rCs = (dut.io.dfi.control.csN.toLong >> (rCmdPhase * rCsNum)) & rIdleCs
            if (rRas == 0 && rCas == 0 && rWe == 1 && rCs != rIdleCs) {
              if (lastRefreshCycle >= 0) minRefreshInterval = math.min(minRefreshInterval, rc - lastRefreshCycle)
              lastRefreshCycle = rc
              refreshCount += 1
            }
          }
          println(
            s"[REFRESH] ${name}: refreshCount=$refreshCount minInterval=$minRefreshInterval " +
              s"(tREF=${dut.dfiConfig.sdram.tREF} MC cycles, REF_RATE=${dut.dfiConfig.sdram.sdramtime.REF_RATE})"
          )
          assert(
            refreshCount >= refreshMinCount,
            s"${name}: expected >= $refreshMinCount refreshes in idle window, got $refreshCount"
          )
          println(s"[COUNT] ${name}: cmdWriteCount=$cmdWriteCount rspReadCount=$rspReadCount")
          // Init micro-program end-of-run checks (only for configs carrying an init table;
          // init-less legacy configs keep the agent's gate closed, so no new prints/asserts).
          if (initSteps.nonEmpty) {
            // Baseline evolution (approved): [INIT] exports one line PER RANK and every rank
            // must have received the COMPLETE MRS file (last write per MR index wins) plus
            // ZQCL when the preset has a ZQ step — not just whichever rank the old unreset cs
            // latch happened to pick. This is the invariant the broadcast-cs init fix buys.
            val mrsFile = initSteps
              .filter(_.kind == SdramInitStep.MRS)
              .foldLeft(Map.empty[Int, Int])((acc, s) => acc + (s.mrIndex -> s.opCode))
            val hasZq = initSteps.exists(_.kind == SdramInitStep.ZQ)
            for (r <- 0 until dut.dfiConfig.chipSelectNumber) {
              val mr = dfiMemoryAgent.modeRegisters.getOrElse(r, mutable.Map[Int, Long]())
              println(
                s"[INIT] ${name}: rank$r mrs=${mr.map { case (k, v) => s"$k->0x${v.toHexString}" }.mkString(",")} zq=${dfiMemoryAgent.zqclSeenPerRank(r)}"
              )
              for ((mIdx, mOp) <- mrsFile) {
                assert(
                  mr.get(mIdx).contains(mOp.toLong),
                  s"${name}: rank$r missing MRS mr$mIdx=0x${mOp.toHexString} (got ${mr.map { case (k, v) => s"$k->0x${v.toHexString}" }.mkString(",")})"
                )
              }
              if (hasZq) assert(dfiMemoryAgent.zqclSeenPerRank(r), s"${name}: rank$r never received ZQCL")
            }
            if (generation == SdramGeneration.DDR2)
              assert(!dfiMemoryAgent.zqclSeen, s"${name}: DDR2 init must not issue ZQCL (no ZQ in DDR2)")
          }
          // DDR4 CA parity end-of-run checks: the happy path must have seen zero ALERT_n cycles
          // (any DUT parity bug would have raised it), the injected variant must have seen the
          // alert AND latched it in the DUT's sticky caParityError register.
          if (caParity) {
            println(
              s"[CAPARITY] ${name}: alertLowCount=${dfiMemoryAgent.alertLowCount} " +
                s"dut.caParityError=${dut.caParityError.toBoolean} corruptArmed=${dfiMemoryAgent.parityCorruptCycle}"
            )
            if (injectParityError) {
              assert(
                dfiMemoryAgent.alertLowCount > 0,
                s"${name}: injected parity error did not raise ALERT_n"
              )
              assert(dut.caParityError.toBoolean, s"${name}: DUT did not latch caParityError on ALERT_n")
            } else {
              assert(
                dfiMemoryAgent.alertLowCount == 0,
                s"${name}: ALERT_n raised without injected parity error (DUT parity generation bug)"
              )
              assert(!dut.caParityError.toBoolean, s"${name}: caParityError latched without any parity error")
            }
          }
          // DFI 6.0.1 ctrlmsg end-of-run checks (only for ctrlmsg configs; inert otherwise).
          if (useCtrlMsg && !bootEnabled) {
            println(
              s"[CTRLMSG] $name: reqCount=${dfiMemoryAgent.ctrlMsgReqCount} doneCount=${dfiMemoryAgent.ctrlMsgDoneCount} " +
                s"ackSeen=${dfiMemoryAgent.ctrlMsgAckSeen} msg=0x${dfiMemoryAgent.ctrlMsgReqMsg.toHexString} " +
                s"data=0x${dfiMemoryAgent.ctrlMsgReqData.toHexString} dut.ctrlMsgError=${dut.io.ctrlMsgError.toBoolean}"
            )
            if (ctrlMsgAckDelayCycles > 0) {
              // Timeout case: the PHY acked beyond tctrlmsg_resp, so the DUT must have latched
              // the sticky error, revoked dfi_ctrlmsg_req and never pulsed trainDone.
              assert(dut.io.ctrlMsgError.toBoolean, s"$name: ctrlMsgError not latched after resp timeout (sticky register broken)")
              assert(!dfiMemoryAgent.ctrlMsgAckSeen, s"$name: PHY acked although its delay exceeds the resp window")
              assert(dfiMemoryAgent.ctrlMsgDoneCount == 0, s"$name: DONE seen on the timeout path")
              assert(!trainDoneSeen, s"$name: trainDone pulsed on the timeout path")
            } else {
              // Happy case: full handshake (REQ -> ACK -> DONE), zero error, and normal read
              // traffic resumed and completed after training.
              assert(!dut.io.ctrlMsgError.toBoolean, s"$name: ctrlMsgError latched without an error path")
              assert(
                dfiMemoryAgent.ctrlMsgReqCount == 1 && dfiMemoryAgent.ctrlMsgDoneCount == 1 && dfiMemoryAgent.ctrlMsgAckSeen,
                s"$name: ctrlmsg handshake incomplete"
              )
              assert(dfiMemoryAgent.ctrlMsgReqMsg == CtrlMsgCode.WRLVL, s"$name: REQ msg code was 0x${dfiMemoryAgent.ctrlMsgReqMsg.toHexString}, expected WRLVL")
              // chipSelectNumber=2 here -> multi-rank Table 56 encoding: bit0=0, bit1=1 (no 3DS),
              // bits[7:4]=rank(0) -> 0x0002.
              assert(dfiMemoryAgent.ctrlMsgReqData == 0x0002, s"$name: ctrlmsg data was 0x${dfiMemoryAgent.ctrlMsgReqData.toHexString}, expected 0x0002 (Table 56 multi-rank)")
              assert(rspReadBeats > trafficRspReadAtResume, s"$name: no read traffic after training (traffic did not resume)")
            }
          }
          // P3 legacy write-leveling end-of-run checks (only for useLegacyWrLvl configs;
          // inert otherwise). The [WRLVL] agent prints above recorded the handshake
          // cycle-by-cycle: EN-RAISE -> RESP -> EN-DROP, and the ban window (level up) held
          // with zero ACT/WR/RD leak cycles.
          if (legacyWrLvlEnabled) {
            println(
              s"[WRLVL] $name: enRaise=${dfiMemoryAgent.wrLvlEnRaiseCycle} resp=${dfiMemoryAgent.wrLvlRespCycle} " +
                s"dutDoneAt=$wrLvlDoneAtCycle reqCount=${dfiMemoryAgent.wrLvlReqCount} " +
                s"respCount=${dfiMemoryAgent.wrLvlRespCount} dutError=${dut.io.legacyWrLvlError.toBoolean} " +
                s"postResumeRspBeats=${rspReadBeats - wrLvlRspReadAtResume}"
            )
            assert(
              dfiMemoryAgent.wrLvlReqCount == 1 && dfiMemoryAgent.wrLvlRespCount == 1,
              s"$name: expected exactly one full wrLvl handshake, got req=${dfiMemoryAgent.wrLvlReqCount} resp=${dfiMemoryAgent.wrLvlRespCount}"
            )
            assert(!dut.io.legacyWrLvlError.toBoolean, s"$name: wrLvlError latched on the happy path")
            assert(rspReadBeats > wrLvlRspReadAtResume, s"$name: no traffic after wrLvlDone (did not resume)")
          }
          // P2 boot-training orchestrator end-of-run checks (only for bootTraining configs).
          if (bootEnabled) {
            def seqStr = bootMsgSeq.map(v => f"0x$v%02x").mkString("->")
            val postHaltRspBeats = rspReadBeats - rspReadBeatsAtHalt
            println(
              s"[BOOTTRAIN] $name: seq=$seqStr donePulses=$bootDonePulses " +
                s"error=${dut.io.ctrlMsgError.toBoolean} haltedAt=$bootHaltedAtCycle postHaltCmd=$postHaltCmdCount " +
                s"postHaltRspBeats=$postHaltRspBeats pending=${bmbAgent.pendingCounter} " +
                s"agentReq=${dfiMemoryAgent.ctrlMsgReqCount} agentDone=${dfiMemoryAgent.ctrlMsgDoneCount}"
            )
            if (ctrlMsgAckDelayCycles > 0) {
              // Timeout injection: the PHY never acks ZQCAL within tctrlmsg_resp -> sticky
              // ctrlMsgError, MC permanently halted (no retry, no second message, no traffic),
              // bootTrainDone never pulses, later BMB commands stay stalled.
              assert(bootDonePulses == 0, s"$name: bootTrainDone pulsed on the timeout/halt path")
              assert(dut.io.ctrlMsgError.toBoolean, s"$name: ctrlMsgError not latched on the boot training timeout")
              assert(bootHalted, s"$name: halt window not entered")
              assert(
                bootMsgSeq == mutable.ArrayBuffer[Long](CtrlMsgCode.ZQCAL.toLong),
                s"$name: expected a single ZQCAL request before halt, got $seqStr (retry?)"
              )
              assert(postHaltCmdCount == 0, s"$name: $postHaltCmdCount DFI command cycles after permanent halt (traffic not halted)")
              assert(
                postHaltRspBeats == 0,
                s"$name: $postHaltRspBeats rsp beats completed after halt (traffic resumed on the timeout path)"
              )
              assert(
                bmbAgent.pendingCounter > 0,
                s"$name: no in-flight BMB command stuck at halt (injected business command was serviced?)"
              )
            } else {
              // Happy path: ZQCAL -> WRLVL in that exact order (agent-side proof: the [CTRLMSG]
              // REQ prints above show 0x07 before 0x04), exactly one bootTrainDone pulse, no
              // error, and read traffic resumes after bootTrainDone and completes.
              assert(
                bootMsgSeq == mutable.ArrayBuffer[Long](CtrlMsgCode.ZQCAL.toLong, CtrlMsgCode.WRLVL.toLong),
                s"$name: boot orchestration order must be ZQCAL->WRLVL, got $seqStr"
              )
              assert(bootDonePulses == 1, s"$name: bootTrainDone pulsed $bootDonePulses times, expected exactly 1")
              assert(!dut.io.ctrlMsgError.toBoolean, s"$name: ctrlMsgError latched on the boot training happy path")
              assert(
                dfiMemoryAgent.ctrlMsgReqCount == 2 && dfiMemoryAgent.ctrlMsgDoneCount == 2,
                s"$name: expected two full ctrlmsg handshakes, got req=${dfiMemoryAgent.ctrlMsgReqCount} done=${dfiMemoryAgent.ctrlMsgDoneCount}"
              )
              assert(rspReadBeats > rspReadBeatsAtBootDone, s"$name: no read traffic after bootTrainDone (traffic did not resume)")
            }
          }
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
  def ddr4(
      name: String,
      fr: Int,
      phase: Int,
      map: AddrMap,
      csN: Boolean = false,
      mr5: Int = 0x0000,
      mr4: Int = 0x0000,
      timing: SdramTiming = null,
      refreshMinCount: Int = 1,
      idleCycles: Int = 10000,
      caParity: Boolean = false,
      injectParityError: Boolean = false,
      useCtrlMsg: Boolean = false,
      ctrlMsgAckDelayCycles: Int = 0,
      bootTraining: Boolean = false
  ): Unit =
    runDfiControllerTest(
      name, SdramGeneration.DDR4, fr, phase, 0, 2, 13, map,
      useCsN = csN, bgWidth = 1, cidWidth = 1, bankWidth = 4,
      sdramTiming = if (timing != null) timing else ddr4Timing,
      mr5 = mr5, mr4 = mr4, refreshMinCount = refreshMinCount, idleCycles = idleCycles,
      caParity = caParity, injectParityError = injectParityError,
      useCtrlMsg = useCtrlMsg, ctrlMsgAckDelayCycles = ctrlMsgAckDelayCycles,
      bootTraining = bootTraining
    )
  ddr4("DfiController_DDR4_fr2_RowBankColumn", 2, 0, RowBankColumn)
  ddr4("DfiController_DDR4_fr2_BankRowColumn", 2, 0, BankRowColumn)
  ddr4("DfiController_DDR4_fr2_cmdPhase1", 2, 1, RowBankColumn)
  ddr4("DfiController_DDR4_fr4_cmdPhase0", 4, 0, RowBankColumn)
  ddr4("DfiController_DDR4_fr4_cmdPhase1", 4, 1, RowBankColumn)
  ddr4("DfiController_DDR4_fr2_csN", 2, 0, RowBankColumn, csN = true)
  // DDR4 DBI (MR5 A12=Read DBI, A11=Write DBI): MC inverts bytes with >4 zeros (dbi_n=0) on
  // writes and restores DRAM-inverted read bytes. The agent models both SDRAM behaviors.
  ddr4("DfiController_DDR4_fr2_dbi", 2, 0, RowBankColumn, mr5 = 0x1800)
  // DDR4 CA parity (JESD79-4B §4.17, DFI 6.0.1 §4.3.1): MC drives even parity over the command
  // bus, same cycle as the command (tcapar_lat=0). MR5.A2:A0=1 (PL4) = parity enabled, DBI off.
  // Happy path: the agent recomputes parity per phase and ignores + alerts on any mismatch.
  ddr4("DfiController_DDR4_fr2_caparity", 2, 0, RowBankColumn, mr5 = 0x0001, caParity = true)
  // Injection: the parity bit as seen at the DRAM is flipped on an idle cycle (cycle >= 5000);
  // the agent must raise ALERT_n and the DUT must latch caParityError, with data flow intact.
  ddr4("DfiController_DDR4_fr2_caparity_inject", 2, 0, RowBankColumn, mr5 = 0x0001, caParity = true, injectParityError = true)
  // DFI 6.0.1 ctrlmsg training channel (Table 12/47/48), code 0x04 = Write Leveling (Table 55):
  // driven mid-run while BMB traffic is quiesced; the agent acks within tctrlmsg_resp (delay 0),
  // the training-window traffic ban must hold (spawn/refresh paused), trainDone pulses,
  // ctrlMsgError stays low and normal read/write traffic resumes and completes afterwards.
  ddr4("DfiController_DDR4_fr2_ctrlmsg_wrlvl", 2, 0, RowBankColumn, useCtrlMsg = true)
  // Same channel, code 0x07 = ZQ Calibration; the PHY acks 8 cycles late (> tctrlmsg_resp = 1):
  // the DUT must latch the sticky ctrlMsgError, revoke dfi_ctrlmsg_req and never pulse
  // trainDone (record-and-expose, no retry — same policy as caParityError).
  ddr4("DfiController_DDR4_fr2_ctrlmsg_timeout", 2, 0, RowBankColumn, useCtrlMsg = true, ctrlMsgAckDelayCycles = 8)
  // DDR4 temperature-controlled refresh 2x (JESD79-4B §4.8/§4.9.2, Table 24):
  // MR4 A3=1 (TCR enable) + A2=1 (extended temperature range) programs the DRAM for 2x; the
  // scheduler halves tREFI via SdramTiming.REF_RATE=2 (7800ns -> 3900ns) and carries
  // tRFC2(8Gb)=260ns. The 10k-cycle idle window shows ~2x the 1x baseline refresh count.
  val ddr4Timing2x = ddr4Timing.copy(REF_RATE = 2, RFC = 260)
  // idleCycles=20000: the default 10k idle window saw only a single 2x refresh (the first
  // lands at ~2x tREF = 1560 MC cycles), so minInterval never measured a real gap. 20k
  // guarantees several intervals -> minInterval must land near 780, exactly the 2x tREFI.
  ddr4("DfiController_DDR4_fr2_ref2x", 2, 0, RowBankColumn, mr4 = 0x0C, timing = ddr4Timing2x,
       refreshMinCount = 20, idleCycles = 20000)

  // The REF_NS/REF_RATE refresh knobs are generation-AGNOSTIC scheduler arithmetic (no DDR4
  // protocol semantics): any generation's tREFI can be given an absolute base and halved.
  // DDR3 baseline here: REF=64000us/32Ki rows @200MHz -> tREF=3125 cycles. With
  // REF_NS=7800 -> 1560 cycles, REF_RATE=2 -> 780 cycles: 4x denser than the
  // per-row-derived default, proven by the halved minInterval below.
  val ddr3TimingRefRate = SdramTiming(
    generation = 3, RFC = 260, RAS = 38, RP = 15, RCD = 15, WTR = 8, WTP = 0,
    RTP = 8, RRD = 6, REF = 64000, FAW = 35, REF_NS = 7800, REF_RATE = 2)
  runDfiControllerTest("DfiController_DDR3_fr2_ref_rate2", SdramGeneration.DDR3, 2, 0, 0, 2, 15,
    RowBankColumn, sdramTiming = ddr3TimingRefRate, refreshMinCount = 20, idleCycles = 20000)
  // Same knob on DDR2 (burstLength=4): scheduler behavior must be generation-independent.
  val ddr2TimingRefRate = SdramTiming(
    generation = 2, RFC = 120, RAS = 20, RP = 15, RCD = 15, WTR = 8, WTP = 0,
    RTP = 8, RRD = 7, REF = 64000, FAW = 20, REF_NS = 7800, REF_RATE = 2)
  runDfiControllerTest("DfiController_DDR2_fr2_ref_rate2", SdramGeneration.DDR2, 2, 0, 0, 2, 15,
    RowBankColumn, sdramTiming = ddr2TimingRefRate, refreshMinCount = 20, idleCycles = 20000)

  // ===== DDR3/DDR2 init micro-programs (step-table engine presets) =====
  // Protocol facts (dual-sourced: Micron ddr3.v cmd table in-repo + litedram/init.py sequences):
  // legacy cmd encodings MRS/EMRS=000, ZQCL=110+A10; EMRS via BA=1/2/3, MRS via BA=0, OP on A.
  // DDR3 init (litedram): MRS MR2 -> MR3 -> MR1 -> MR0(DLL reset) -> ZQCL.
  // MR0=0x310: BL8, CL5, WR5, DLL reset (A8=0x100) per the litedram formula.
  runDfiControllerTest("DfiController_DDR3_fr2_init", SdramGeneration.DDR3, 2, 0, 0, 2, 15, RowBankColumn,
    initSteps = List(
      SdramInitStep(SdramInitStep.MRS, 2, 0x200, 8),
      SdramInitStep(SdramInitStep.MRS, 3, 0x000, 8),
      SdramInitStep(SdramInitStep.MRS, 1, 0x000, 8),
      SdramInitStep(SdramInitStep.MRS, 0, 0x310, 8),
      SdramInitStep(SdramInitStep.ZQ, 0, 0, 512)
    ))
  // DDR2 init (litedram): PRE_ALL -> EMRS3 -> EMRS2 -> EMRS1 -> MRS(DLL reset) -> PRE_ALL ->
  // REF -> REF -> MRS -> EMRS1 OCD default -> EMRS1 OCD exit. DDR2 has NO ZQ.
  // MR0=0x552: BL4, CL5, WR2, DLL reset. DLL-reset step waits 256 (200 tDLL lock cycles).
  runDfiControllerTest("DfiController_DDR2_fr2_init", SdramGeneration.DDR2, 2, 0, 0, 2, 15, RowBankColumn,
    initSteps = List(
      SdramInitStep(SdramInitStep.PRE_ALL, 0, 0, 8),
      SdramInitStep(SdramInitStep.MRS, 3, 0x000, 8),
      SdramInitStep(SdramInitStep.MRS, 2, 0x000, 8),
      SdramInitStep(SdramInitStep.MRS, 1, 0x000, 8),
      SdramInitStep(SdramInitStep.MRS, 0, 0x552, 256),
      SdramInitStep(SdramInitStep.PRE_ALL, 0, 0, 8),
      SdramInitStep(SdramInitStep.REF, 0, 0, 8),
      SdramInitStep(SdramInitStep.REF, 0, 0, 8),
      SdramInitStep(SdramInitStep.MRS, 0, 0x452, 8),
      SdramInitStep(SdramInitStep.MRS, 1, 0x452 | 0x380, 8), // EMRS1 OCD default
      SdramInitStep(SdramInitStep.MRS, 1, 0x452, 8) // EMRS1 OCD exit
    ))

  // ===== P2 boot training orchestrator (initDone -> ZQCAL -> WRLVL auto-chain) =====
  // After the DDR4 init micro-program completes, the orchestrator injects ZQCAL then WRLVL
  // through the ctrlmsg channel (agent-side order proof: the [CTRLMSG] REQ prints show 0x07
  // before 0x04; DUT-side: the bootMsgSeq assert), pulses bootTrainDone exactly once and
  // releases traffic. The BMB master generates from t0, so the queued backlog proves the
  // spawn/refresh pause covers the whole orchestration window (the agent's training-window
  // traffic ban double-checks every handshake).
  ddr4("DfiController_DDR4_fr2_boottrain", 2, 0, RowBankColumn, useCtrlMsg = true, bootTraining = true)
  // Timeout injection: the PHY stub acks 8 cycles late (> tctrlmsg_resp = 1), so the ZQCAL
  // handshake fails: sticky ctrlMsgError, MC permanently halted (trainBusy never released,
  // no retry, no second message), bootTrainDone never pulses, and later BMB commands stay
  // stalled (zero DFI command cycles after the halt). The permanent halt also stops
  // auto-refresh by design, so this case expects zero refreshes in the idle window.
  ddr4("DfiController_DDR4_fr2_boottrain_timeout", 2, 0, RowBankColumn, useCtrlMsg = true,
    bootTraining = true, ctrlMsgAckDelayCycles = 8, refreshMinCount = 0)

  // ===== P3 legacy (DDR2/DDR3) write-leveling Stream handshake (MC side) =====
  // The legacy transport (no ctrlmsg channel): after the DDR3 init micro-program, the
  // testbench quiesces traffic, fires the trigger and the MC must raise dfi_wrlvl_en
  // (wrLvlStart level), hold spawn/refresh paused for the whole window (the agent's ban
  // fails the sim on ANY ACT/WR/RD while the level is up), consume the PHY's dfi_wrlvl_resp
  // rising edge (PHY stub answers after wrLvlDoneDelayCycles = 8, inside tWrlvlResp = 16)
  // and resume traffic. The [WRLVL] agent prints record the handshake cycle-by-cycle.
  runDfiControllerTest("DfiController_DDR3_fr2_legacy_wrlvl", SdramGeneration.DDR3, 2, 0, 0, 2, 15, RowBankColumn,
    initSteps = List(
      SdramInitStep(SdramInitStep.MRS, 2, 0x200, 8),
      SdramInitStep(SdramInitStep.MRS, 3, 0x000, 8),
      SdramInitStep(SdramInitStep.MRS, 1, 0x000, 8),
      SdramInitStep(SdramInitStep.MRS, 0, 0x310, 8),
      SdramInitStep(SdramInitStep.ZQ, 0, 0, 512)
    ),
    // Window = tWrlvlResp(64) must comfortably exceed the BMB-to-ACT spawn pipeline lag
    // (~10-18 cycles, measured) so the counter-proof's early-released backlog lands INSIDE
    // the open window; the PHY answers at delay 32, well inside the watchdog.
    useLegacyWrLvl = true, wrLvlDoneDelayCycles = 32, tWrlvlResp = 64)
}
