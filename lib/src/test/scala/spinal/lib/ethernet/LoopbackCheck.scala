package spinal.lib.ethernet

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.bus.amba4.axis._

import scala.collection.mutable
import scala.math._
import scala.util.Random

class Loopback(
    txConfig: TxGenerics,
    headerGenerateConfig: HeaderGeneratorGenerics,
    rxConfig: RxGenerics,
    headerRecognizeConfig: HeaderRecognizerGenerics
) extends Component {

  val dataAxisInCfg = Axi4StreamConfig(
    dataWidth = txConfig.DATA_BYTE_CNT,
    userWidth = txConfig.DATA_TUSER_WIDTH,
    useStrb = txConfig.DATA_USE_TSTRB,
    useKeep = txConfig.DATA_USE_TKEEP,
    useLast = txConfig.DATA_USE_TLAST,
    useUser = txConfig.DATA_USE_TUSER
  )

  val dataAxisOutCfg = Axi4StreamConfig(
    dataWidth = rxConfig.DATA_BYTE_CNT,
    userWidth = rxConfig.DATA_TUSER_WIDTH,
    useStrb = rxConfig.DATA_USE_TSTRB,
    useKeep = rxConfig.DATA_USE_TKEEP,
    useLast = rxConfig.DATA_USE_TLAST,
    useUser = rxConfig.DATA_USE_TUSER
  )

  val io = new Bundle {
    val metaIn = slave Stream MetaData()
    val dataAxisIn = slave(Axi4Stream(dataAxisInCfg))

    val metaOut = master Stream MetaData()
    val dataAxisOut = master(Axi4Stream(dataAxisOutCfg))
  }

  val tx = new TxTop(txConfig, headerGenerateConfig)
  val rx = new RxTop(rxConfig, headerRecognizeConfig)

  tx.io.metaIn << io.metaIn
  tx.io.dataAxisIn << io.dataAxisIn

  rx.io.dataAxisIn << tx.io.dataAxisOut

  io.dataAxisOut << rx.io.dataAxisOut
  io.metaOut << rx.io.metaOut
}

object LoopbackMain extends App {
  val rxConfig = RxGenerics()
  val generateConfig = HeaderGeneratorGenerics()
  val txConfig = TxGenerics()
  val recognizeConfig = HeaderRecognizerGenerics()

  SpinalVerilog(new Loopback(txConfig, generateConfig, rxConfig, recognizeConfig))
}

case class simConfig(
    sendTimes: Int = 50000,
    useRandomPacket: Boolean = true,
    packetLen: Int = 64,
    dstIpAddr: String = "c0a80103",
    dstPort: String = "156",
    dstMacAddr: String = "fccffccffccf",
    srcIpAddr: String = "c0a80102",
    srcPort: String = "156",
    srcMacAddr: String = "fcacad123456"
)
object LoopbackCheckSim extends App {

  val txConfig = TxGenerics()
  val rxConfig = RxGenerics()
  val genConfig = HeaderGeneratorGenerics()
  val recConfig = HeaderRecognizerGenerics()
  val simCfg = simConfig()

  // dont use vcs
  SimConfig.withWave
    .compile(
      new Loopback(txConfig, genConfig, rxConfig, recConfig)
    )
    .doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      val dataQueue = new mutable.Queue[String]
      def initPort(): Unit = {
        dut.io.metaIn.payload.dstIpAddr #= 0
        dut.io.metaIn.payload.dstPort #= 0
        dut.io.metaIn.payload.dstMacAddr #= 0
        dut.io.metaIn.payload.srcPort #= 0
        dut.io.metaIn.payload.srcIpAddr #= 0
        dut.io.metaIn.payload.srcMacAddr #= 0
        dut.io.metaIn.dataLen #= 0
        dut.io.metaIn.dataLen #= 0
        dut.io.metaIn.valid #= false

        dut.io.dataAxisOut.ready #= true
        dut.io.metaOut.ready #= true

        dut.io.dataAxisIn.valid #= false
        dut.io.dataAxisIn.data #= 0
        dut.io.dataAxisIn.last #= false
        dut.io.dataAxisIn.keep #= 0
        dut.io.dataAxisIn.user #= 0
      }

      initPort()
      dut.clockDomain.waitSampling(50)

      println("init monitor")
      def monitor(): Unit = {
        while (true) {
          if (dut.io.dataAxisOut.valid.toBoolean) {
            val ref = dataQueue.dequeue()
            val resData = dut.io.dataAxisOut.data.toBigInt.toString(16)
            val resKeep = dut.io.dataAxisOut.keep.toLong
            val resValid = (log10(resKeep + 1) / log10(2.0)).toInt
            val resNum = if ((resData.length - resValid * 2) < 0) 0 else (resData.length - resValid * 2)
            val res = resData.substring(resNum)
            val refAppend = "0" * (64 - ref.length) + ref
            val resAppend = "0" * (64 - res.length) + res
            println(f"ref: $refAppend, res: $resAppend")
            assert(refAppend == resAppend, s"require ${refAppend}, but return ${resAppend}")
          }
          dut.clockDomain.waitSampling()
        }
      }

      val enaMonitor = fork {
        monitor()
      }
      println("start data drive")

      for (idx <- 0 until simCfg.sendTimes) {
        val sendDataBytes =
          if (simCfg.useRandomPacket) Random.nextInt(1024).abs + 1
          else simCfg.packetLen
        println(idx + " times transaction...")
        driveTransaction(sendDataBytes)
      }

      def driveTransaction(sendDataBytes: Int): Unit = {
        val a = fork {
          loadMeta(sendDataBytes)
        }
        val b = fork {
          loadData(sendDataBytes)
        }
        a.join()
        b.join()
        dut.clockDomain.waitSampling(Random.nextInt(20).abs)
      }

      def loadMeta(sendDataBytes: Int): Unit = {
        dut.io.metaIn.payload.dstMacAddr #= simCfg.dstMacAddr.asHex
        dut.io.metaIn.payload.dstIpAddr #= simCfg.dstIpAddr.asHex
        dut.io.metaIn.payload.dstPort #= simCfg.dstPort.asHex
        dut.io.metaIn.payload.srcMacAddr #= simCfg.srcMacAddr.asHex
        dut.io.metaIn.payload.srcIpAddr #= simCfg.srcIpAddr.asHex
        dut.io.metaIn.payload.srcPort #= simCfg.srcPort.asHex
        dut.io.metaIn.payload.dataLen #= sendDataBytes
        dut.io.metaIn.valid #= true
        dut.clockDomain.waitSampling()
        dut.io.metaIn.valid #= false
      }
      def loadData(sendDataBytes: Int): Unit = {
        for (i <- 0 until (sendDataBytes.toFloat / 32.0f).ceil.toInt) {
          val randData = BigInt(256, Random).toString(16)
          dut.io.dataAxisIn.data #= randData.asHex
          dut.io.dataAxisIn.valid #= true
          dut.io.dataAxisIn.user #= 0
          if (i == (sendDataBytes.toFloat / 32.0f).ceil.toInt - 1) {
            dut.io.dataAxisIn.keep #= (pow(2, sendDataBytes - i * 32) - 1).toLong.abs
            dut.io.dataAxisIn.last #= true
            val subLen =
              if ((randData.length - (sendDataBytes - i * 32) * 2) < 0) 0
              else randData.length - (sendDataBytes - i * 32) * 2
            dataQueue.enqueue(randData.substring(subLen))
          } else {
            dut.io.dataAxisIn.last #= false
            dut.io.dataAxisIn.keep #= (pow(2, 32) - 1).toLong.abs
            dataQueue.enqueue(randData)
          }
          dut.clockDomain.waitSampling()
          dut.io.dataAxisIn.data #= 0
          dut.io.dataAxisIn.valid #= false
          dut.io.dataAxisIn.last #= false
          dut.io.dataAxisIn.keep #= 0
        }
      }

      dut.clockDomain.waitSampling(50)
      simSuccess()
    }
}
