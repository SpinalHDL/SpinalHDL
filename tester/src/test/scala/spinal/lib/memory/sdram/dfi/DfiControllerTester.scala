package spinal.lib.memory.sdram.dfi

import spinal.lib.bus.bmb.sim.{BmbMasterAgent, BmbRegionAllocator}
import spinal.lib.bus.misc.SizeMapping
import spinal.tester.code.SpinalAnyFunSuite

import scala.util.Random

class DfiControllerTester extends SpinalAnyFunSuite {

  import spinal.core._
  import spinal.core.sim._
  import spinal.lib.bus.bmb.BmbParameter

  import scala.collection.mutable

  test("DfiControllerTester") {
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
          ddrMHZ = 200,
          ddrWrLat = 4,
          ddrRdLat = 4,
          sdramtime = sdramtime
        )
        val timeConfig = DfiTimeConfig(
          frequencyRatio = 2,
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
          chipSelectNumber = 2,
          dataSlice = 1,
          signalConfig = new DDR3SignalConfig(DfiFunctionConfig(), false) {
            override val useWrdataCsN = false
            override val useRddataCsN = false
          },
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
        val dut = DfiController(bmbp, task, dfiConfig, RowBankColumn)
        dut
      }
      .doSimUntilVoid { dut =>
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
        val bmbAgent = new BmbMasterAgent(dut.io.bmb, dut.clockDomain) {
          override def onRspRead(address: BigInt, data: Seq[Byte]): Unit = {
            val ref = (0 until data.length).map(i => dfiMemoryAgent.getByte(addressTranslation(address.toLong + i)))
            if (ref != data) {
              val master = dut.io.bmb
              simFailure(s"Read mismatch on $master\n  REF=$ref\n  DUT=$data")
            }
          }
          override def getCmd(): () => Unit =
            if ((!rspQueue.exists(_.nonEmpty)) | (cmdQueue.nonEmpty)) super.getCmd() else null
          override def maskRandom() = true
          override def onCmdWrite(address: BigInt, data: Byte): Unit = {
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
        while (bmbAgent.rspQueue.exists(_.nonEmpty)) {
          dut.clockDomain.waitSampling(1000)
        }
        dut.clockDomain.waitSampling(10000)
        simSuccess()
      }
  }
}
