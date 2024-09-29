package spinal.lib

import spinal.tester.code.SpinalAnyFunSuite

class DfiControllerTester extends SpinalAnyFunSuite {

  import spinal.core._
  import spinal.core.sim._
  import spinal.lib.bus.bmb.BmbParameter
  import spinal.lib.memory.sdram.dfi._
  import spinal.lib.memory.sdram.dfi.interface._

  import scala.collection.mutable

  test("DfiControllerTester") {
    SimConfig
      .compile {
        val tp: TaskParameter =
          TaskParameter(timingWidth = 5, refWidth = 23, cmdBufferSize = 64, dataBufferSize = 64, rspBufferSize = 64)
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
        val dc: DfiConfig = DfiConfig(
          frequencyRatio = 4,
          chipSelectNumber = 2,
          bgWidth = 0,
          cidWidth = 0,
          dataSlice = 1,
          cmdPhase = 0,
          signalConfig = new DDRSignalConfig(),
          timeConfig = timeConfig,
          sdram = sdram
        )
        val bmbp: BmbParameter = BmbParameter(
          addressWidth = sdram.byteAddressWidth + log2Up(dc.chipSelectNumber),
          dataWidth = dc.beatWidth,
          sourceWidth = 1,
          contextWidth = 2,
          lengthWidth = 6,
          alignment = BmbParameter.BurstAlignement.WORD
        )
        val dut = DfiController(bmbp, tp, dc)
        dut.bmbBridge.bmbAdapter.io.output.rsp.payload.last.simPublic()
        dut
      }
      .doSimUntilVoid { dut =>
        dut.clockDomain.forkStimulus(10)
        import dut._
        val writeQueue = mutable.Queue[BigInt]()
        val readQueue = mutable.Queue[BigInt]()

        def write(
            array: Array[Int],
            address: BigInt = BigInt(bmbp.access.addressWidth - log2Up(task.bytePerTaskMax), simRandom) << log2Up(
              task.bytePerTaskMax
            )
        ) = {

          io.bmb.cmd.address #= address
          io.bmb.cmd.length #= array.length * dut.dc.bytePerBeat - 1
          io.bmb.cmd.opcode #= 1
          io.bmb.cmd.valid #= true

          io.bmb.cmd.data.randomize()
          clockDomain.waitSampling()
          for (arr <- array.tail) {
            while (!io.bmb.cmd.ready.toBoolean) {
              io.bmb.cmd.valid #= false
              clockDomain.waitSampling()
            }
            io.bmb.cmd.valid #= true
            io.bmb.cmd.data.randomize()
            writeQueue.enqueue(io.bmb.cmd.data.toBigInt)
            if (arr == array.last) io.bmb.cmd.last #= true
            clockDomain.waitSampling()
          }
          io.bmb.cmd.last #= false
          io.bmb.cmd.valid #= false
          writeQueue.enqueue(io.bmb.cmd.data.toBigInt)
        }

        def read(
            beatCount: Int,
            address: BigInt = BigInt(bmbp.access.addressWidth - log2Up(task.bytePerTaskMax), simRandom) << log2Up(
              task.bytePerTaskMax
            )
        ): Unit = {
          io.bmb.cmd.address #= address
          io.bmb.cmd.length #= beatCount * dut.dc.bytePerBeat - 1
          io.bmb.cmd.opcode #= 0
          io.bmb.cmd.valid #= true
          io.bmb.cmd.last #= true
          clockDomain.waitSampling()

          io.bmb.cmd.last #= false
          io.bmb.cmd.valid #= false
          io.bmb.cmd.opcode #= 1
          clockDomain.waitSamplingWhere(dut.io.dfi.read.rden(0).toBoolean)

          io.bmb.rsp.ready #= true
        }

        def readdata(beatCount: Int): Unit = {
          for (i <- 0 until beatCount) {
            dut.io.dfi.read.rd.foreach(_.rddataValid #= true)
            dut.io.dfi.read.rd.foreach(_.rddata.randomize())
            clockDomain.waitSampling()
            readQueue.enqueue(
              dut.io.dfi.read.rd
                .map(_.rddata.toBigInt)
                .reverse
                .zipWithIndex
                .reduceLeft((a, b) => (a._1 + (b._1 << (b._2 * dc.phyIoWidth)), 0))
                ._1
            )
          }
          dut.io.dfi.read.rd.foreach(_.rddataValid #= false)
          clockDomain.waitSamplingWhere(bmbBridge.bmbAdapter.io.output.rsp.payload.last.toBoolean)
          clockDomain.waitSampling()
          io.bmb.rsp.ready #= false
        }

        val bmbDatas = new Array[Int]((1 << bmbp.access.lengthWidth) / dut.dc.bytePerBeat)
        for (i <- 0 until (bmbDatas.length)) {
          bmbDatas(i) = i
        }

        fork {
          dut.clockDomain.assertReset()
          dut.clockDomain.fallingEdge()
          sleep(10)
          while (true) {
            dut.clockDomain.clockToggle()
            sleep(5)
          }
        }

        fork {
          var writeSlicesCount: Int = 0
          var writeDataBigInt: BigInt = 0
          var writeDataTemp: BigInt = 0
          while (true) {
            clockDomain.waitSampling()
            for (wr <- io.dfi.write.wr) {
              if (wr.wrdataEn.toBoolean) {
                writeDataBigInt += wr.wrdata.toBigInt << (dc.frequencyRatio - writeSlicesCount - 1) * dc.phyIoWidth
                writeSlicesCount += 1
                if (writeSlicesCount == dc.frequencyRatio) {
                  writeSlicesCount = 0
                  writeDataTemp = writeQueue.dequeue()
                  assert(
                    writeDataTemp == writeDataBigInt,
                    s"writeQueue.dequeue'${writeDataTemp.hexString()}' =/= writeDataBigInt'${writeDataBigInt.hexString()}'"
                  )
                  writeDataBigInt = 0
                }
              }
            }
          }
        }
        fork {
          var readDataTemp: BigInt = 0
          while (true) {
            clockDomain.waitSampling()
            if (io.bmb.rsp.valid.toBoolean) {
              readDataTemp = readQueue.dequeue()
              assert(
                readDataTemp == io.bmb.rsp.data.toBigInt,
                s"writeQueue.dequeue'${readDataTemp.hexString()}' =/= writeDataBigInt'${io.bmb.rsp.data.toBigInt.hexString()}'"
              )
            }
          }
        }

        fork {
          io.dfi.read.rd.foreach(_.rddataValid #= false)
          clockDomain.waitSampling(10)
          io.bmb.cmd.valid #= false
          io.bmb.cmd.last #= false
          io.bmb.cmd.source #= 0
          io.bmb.cmd.opcode.randomize()
          io.bmb.cmd.address #= 0
          io.bmb.cmd.length #= 0
          io.bmb.cmd.data #= 0
          io.bmb.cmd.mask #= 0
          io.bmb.cmd.context #= 0
          clockDomain.waitSampling(5)
          write(array = bmbDatas)
          clockDomain.waitSampling(10)
          write(array = bmbDatas)
          clockDomain.waitSampling(10)
          write(array = bmbDatas)
          clockDomain.waitSampling(10)

          read(beatCount = bmbDatas.size)
          clockDomain.waitSampling(
            2
          ) // The time interval is less than or equal to log2Up((timeConfig.tPhyRdlat + timeConfig.tRddataEn + dc.beatCount-1)/dc.beatCount + 1)
          readdata(bmbDatas.size)
          clockDomain.waitSampling(5)
          read(beatCount = bmbDatas.size)
          clockDomain.waitSampling()
          readdata(bmbDatas.size)
          clockDomain.waitSampling(5)

          write(array = bmbDatas)
          clockDomain.waitSampling(5)
          write(array = bmbDatas)
          clockDomain.waitSampling(20)
          simSuccess()
        }
      }
  }
}
