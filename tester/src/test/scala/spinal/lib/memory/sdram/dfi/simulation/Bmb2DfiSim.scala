package spinal.lib.memory.sdram.dfi.simulation

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.memory.sdram.dfi._
import spinal.lib.memory.sdram.dfi.interface._

import scala.collection.mutable

case class Bmb2DfiSim(x: Int) extends Component {

  val task: TaskParameter =
    TaskParameter(timingWidth = 5, refWidth = 23, cmdBufferSize = 64, dataBufferSize = 256, rspBufferSize = 256)
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
    ddrWrLat = 6,
    ddrRdLat = 6,
    sdramtime = sdramtime
  )
  val timeConfig = DfiTimeConfig(
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
    frequencyRatio = 1,
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
    addressWidth = sdram.byteAddressWidth + log2Up(dfiConfig.chipSelectNumber),
    dataWidth = dfiConfig.beatWidth,
    sourceWidth = 0,
    contextWidth = 2,
    lengthWidth = 10,
    alignment = BmbParameter.BurstAlignement.WORD
  )
  val io = new Bundle {
    val bmb = slave(Bmb(bmbp))
    val dfi = master(Dfi(dfiConfig))
  }
  val bmb2dfi = DfiController(bmbp, task, dfiConfig)
  bmb2dfi.io.bmb <> io.bmb
  bmb2dfi.io.dfi <> io.dfi
}

object Bmb2DfiSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave
      .compile {
        val dut = Bmb2DfiSim(1)
        dut.bmb2dfi.bmbBridge.bmbAdapter.io.output.rsp.payload.last.simPublic()
        dut
      }
      .doSimUntilVoid { dut =>
        dut.clockDomain.forkStimulus(10000)
        import dut._
        fork {
          dut.clockDomain.assertReset()
          sleep(50000)
          dut.clockDomain.deassertReset()
          sleep(10000)
        }
        fork {
          dut.clockDomain.risingEdge()
          sleep(10000)
          while (true) {
            dut.clockDomain.clockToggle()
            sleep(5000)
          }
        }

        val writeQueue = mutable.Queue[BigInt]()
        val readQueue = mutable.Queue[BigInt]()
        val bmbDatas = new Array[Int]((1 << dut.bmbp.access.lengthWidth) / dut.dfiConfig.bytePerBeat)
        for (i <- 0 until (bmbDatas.length)) {
          bmbDatas(i) = i
        }

        fork {
          var writeSlicesCount: Int = 0
          var writeDataBigInt: BigInt = 0
          var writeDataTemp: BigInt = 0
          while (true) {
            clockDomain.waitSampling()
            for (wr <- io.dfi.write.wr) {
              if (wr.wrdataEn.toBoolean) {
                writeDataBigInt += wr.wrdata.toBigInt << (dfiConfig.frequencyRatio - writeSlicesCount - 1) * dfiConfig.phyIoWidth
                writeSlicesCount += 1
                if (writeSlicesCount == dfiConfig.frequencyRatio) {
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
          def write(
              array: Array[Int],
              address: BigInt = BigInt(dut.bmbp.access.addressWidth - log2Up(task.bytePerTaskMax), simRandom) << log2Up(
                task.bytePerTaskMax
              )
          ) = {

            io.bmb.cmd.address #= address
            io.bmb.cmd.length #= array.length * dut.dfiConfig.bytePerBeat - 1
            io.bmb.cmd.opcode #= 1
            io.bmb.cmd.valid #= true
            io.bmb.cmd.data.randomize()
            println("write command")
            clockDomain.waitSampling()
            for (arr <- array.tail) {
              while (!io.bmb.cmd.ready.toBoolean) {
                io.bmb.cmd.valid #= false
                clockDomain.waitSampling()
              }
              io.bmb.cmd.valid #= true
              io.bmb.cmd.data.randomize()
              writeQueue.enqueue(io.bmb.cmd.data.toBigInt)
              if (arr == array.last) {
                io.bmb.cmd.valid #= false
                clockDomain.waitSampling(5)
                io.bmb.cmd.valid #= true
                io.bmb.cmd.last #= true
                io.bmb.cmd.data.randomize()
              } else io.bmb.cmd.data.randomize()
              clockDomain.waitSampling()
            }
            io.bmb.cmd.last #= false
            io.bmb.cmd.valid #= false
            writeQueue.enqueue(io.bmb.cmd.data.toBigInt)
          }
          def read(
              beatCount: Int,
              address: BigInt = BigInt(dut.bmbp.access.addressWidth - log2Up(task.bytePerTaskMax), simRandom) << log2Up(
                task.bytePerTaskMax
              )
          ): Unit = {
            io.bmb.cmd.address #= address
            io.bmb.cmd.length #= beatCount * dut.dfiConfig.bytePerBeat - 1
            io.bmb.cmd.opcode #= 0
            io.bmb.cmd.valid #= true
            io.bmb.cmd.last #= true
            clockDomain.waitSampling()

            io.bmb.cmd.last #= false
            io.bmb.cmd.valid #= false
            io.bmb.cmd.opcode #= 1
            println("read command")
            clockDomain.waitSampling( dfiConfig.timeConfig.tPhyRdlat / dfiConfig.frequencyRatio
                                      ) // The time interval is less than or equal to log2Up((timeConfig.tPhyRdlat + timeConfig.tRddataEn + dfiConfig.beatCount-1)/dfiConfig.beatCount + 1)
            for (i <- 0 until (1 << bmbp.access.lengthWidth) / task.bytePerTaskMax) {
              readdata(task.bytePerTaskMax / dfiConfig.bytePerBeat)
            }
            clockDomain.waitSamplingWhere(dut.io.bmb.rsp.payload.last.toBoolean)
//            clockDomain.waitSampling(5)
            io.bmb.rsp.ready #= false
            println("reading is OK")
          }
          def readdata(beatCount: Int): Unit = {
            if(io.dfi.read.rden(dfiConfig.frequencyRatio-1).toBoolean) {
              io.bmb.rsp.ready #= true
            }else{
              clockDomain.waitSamplingWhere(dut.io.dfi.read.rden(dfiConfig.frequencyRatio-1).toBoolean)
//              clockDomain.waitSampling(4)
              io.bmb.rsp.ready #= true
            }
            for (i <- 0 until beatCount) {
              dut.io.dfi.read.rd.foreach(_.rddataValid #= true)
              dut.io.dfi.read.rd.foreach(_.rddata.randomize())
              clockDomain.waitSampling()
              readQueue.enqueue(
                dut.io.dfi.read.rd
                  .map(_.rddata.toBigInt)
                  .reverse
                  .zipWithIndex
                  .reduceLeft((a, b) => (a._1 + (b._1 << (b._2 * dfiConfig.phyIoWidth)), 0))
                  ._1
              )
            }
            dut.io.dfi.read.rd.foreach(_.rddataValid #= false)
          }

          io.dfi.read.rd.foreach(_.rddataValid #= false)
          clockDomain.waitSampling(30)
          io.bmb.cmd.valid #= false
          io.bmb.cmd.last #= false
          io.bmb.cmd.source #= 0

          io.bmb.cmd.opcode.randomize()
          io.bmb.cmd.address #= 0
          io.bmb.cmd.length #= 0
          io.bmb.cmd.data #= 0
          io.bmb.cmd.mask #= 0
          io.bmb.cmd.context #= 0

          write(array = bmbDatas)
          println("writing is OK")
          clockDomain.waitSampling(10)
          write(array = bmbDatas)
          println("writing is OK")
          clockDomain.waitSampling(10)
          write(array = bmbDatas)
          println("writing is OK")
          clockDomain.waitSampling(50)

          read(address=64,beatCount = bmbDatas.size)
          clockDomain.waitSampling(10)
          read(beatCount = bmbDatas.size)
          clockDomain.waitSampling(50)

          write(array = bmbDatas)
          println("writing is OK")
          clockDomain.waitSampling(10)
          write(array = bmbDatas)
          println("writing is OK")

          clockDomain.waitSampling(100)

          simSuccess()

        }
      }
  }
}
