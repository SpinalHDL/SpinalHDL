package spinal.lib.memory.sdram.dfi

import spinal.lib.bus.bmb.sim.{BmbMasterAgent, BmbRegionAllocator}
import spinal.lib.bus.misc.SizeMapping
import spinal.tester.code.SpinalAnyFunSuite

import scala.util.Random

class DfiControllerTester extends SpinalAnyFunSuite {

  import spinal.core._
  import spinal.core.sim._
  import spinal.lib.bus.bmb.BmbParameter
  import spinal.lib.memory.sdram.dfi.interface._

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
          sdramTime = sdramtime
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
          frequencyRatio = 2,
          chipSelectNumber = 2,
          dataSlice = 1,
          cmdPhase = 0,
          signalConfig = new DDRSignalConfig(),
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
      .doSimUntilVoid(seed = 2117802787) { dut =>
        dut.clockDomain.forkStimulus(10, resetCycles = 16)
        fork {
          sleep(160)
          dut.clockDomain.assertReset()
          sleep(20)
          dut.clockDomain.deassertReset()
        }
        dut.io.dfi.read.rd.foreach(_.rddataValid #= false)
        dut.clockDomain.waitSampling(100)
        val memorySize = 1 << dut.io.bmb.p.access.addressWidth
        val allowedWrites = mutable.HashMap[Long, Byte]()
        val allowedWritesStandby = mutable.HashMap[Long, Byte]()
        val addrMap = dut.addrMap
        def rearrangeBits(value: Long, segmentLengths: Array[Int], order: Seq[Int]): Long = {
          require(
            segmentLengths.length == order.length,
            "The length of the segment length array and the order array must be the same"
          )
          require(segmentLengths.sum == 64, "The sum of all segment lengths must be equal to 64")
          require(order.distinct.length == order.length, "The elements in a order array must be unique")

          val binaryString = value.toBinaryString.reverse.padTo(64, '0').reverse
          val segments = Array.fill[String](segmentLengths.length)("")
          var offset = 0
          for (i <- segmentLengths.indices) {
            val segment = binaryString.substring(offset, offset + segmentLengths(i))
            segments(i) = segment
            offset += segmentLengths(i)
          }
          val rearrangedBinaryString = order.map(segments).mkString("")
          java.lang.Long.parseLong(rearrangedBinaryString, 2)
        }
        def addressTranslation(bmbAddr: Long): Long = {
          val segmentLengths = Array.fill[Int](6)(0)
          segmentLengths(0) = 64 - dut.dfiConfig.sdram.byteAddressWidth
          segmentLengths(1) = dut.dfiConfig.sdram.bankWidth
          segmentLengths(2) = dut.dfiConfig.sdram.rowWidth
          segmentLengths(3) = dut.dfiConfig.sdram.columnWidth - log2Up(dut.dfiConfig.transferPerBurst)
          segmentLengths(4) = log2Up(dut.dfiConfig.transferPerBurst)
          segmentLengths(5) = log2Up(dut.dfiConfig.sdram.bytePerWord)
          val maping = segmentLengths.zipWithIndex
          var temp = maping(0)
          addrMap match {
            case RowBankColumn => {
              temp = maping(1)
              maping(1) = maping(2)
              maping(2) = temp
            }
            case BankRowColumn => {}
            case RowColumnBank => {
              temp = maping(1)
              maping(1) = maping(2)
              maping(2) = maping(3)
              maping(3) = maping(1)
            }
          }
          val dfiAddr = rearrangeBits(bmbAddr, maping.map(_._1), maping.map(_._2).toSeq)
          dfiAddr
        }

        val bmbq = mutable.Queue[(String, Byte)]()
        val dfiq = mutable.Queue[(String, Byte)]()

        val dfiMemoryAgent = new DfiMemoryAgent(dut.io.dfi, dut.clockDomain) {
          override def setByte(address: Long, value: Byte): Unit = {
            val option = allowedWrites.get(address)
            assert(option.isDefined)
            assert(option.get == value, s"$address is address, \n$allowedWrites\n$allowedWritesStandby")
            super.setByte(address, value)
            dfiq.enqueue((address.toBinaryString, value))
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
            bmbq.enqueue((addressLong.toBinaryString, data))
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
