package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.sim._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.bus.wishbone._
import spinal.lib.wishbone.sim._
import spinal.lib.sim._
import scala.util.Random

class WishboneDecoderComponent(config : WishboneConfig,decodings : Seq[SizeMapping]) extends Component{
  val io = new Bundle{
    val busIN = slave(Wishbone(config))
    val busOUT = Vec(master(Wishbone(config)),decodings.size)
  }
  val ff = Reg(Bool())
  val outs = io.busOUT zip decodings
  val decoder = WishboneDecoder(io.busIN,outs)
}

class SpinalSimWishboneDecoderTester extends SpinalAnyFunSuite{
  def testDecoder(config : WishboneConfig,decodings : Seq[SizeMapping],description : String = ""): Unit = {
    val fixture = SimConfig.allOptimisation.withFstWave.compile(new WishboneDecoderComponent(config,decodings).setDefinitionName(s"WishboneDecoderComponent_${description}"))
    fixture.doSim(description){ dut =>
      dut.clockDomain.forkStimulus(period=10)
      val busInDriver = WishboneDriver(dut.io.busIN)
      val busOutDrivers = dut.io.busOUT.map(WishboneDriver(_))

      dut.clockDomain.waitSampling(10)
      SimTimeout(1000*10000)
      val sco = ScoreboardInOrder[WishboneTransaction]()

      val seq = WishboneSequencer{
        WishboneTransaction().randomAdressInRange(Random.shuffle(decodings).head).randomizeData(Math.pow(2,config.dataWidth).toInt-1)
      }

      WishboneMonitor(dut.io.busIN){ bus =>
        sco.pushRef(WishboneTransaction.sampleAsMaster(bus))
      }

      dut.io.busOUT.foreach{busOut =>
        WishboneMonitor(busOut){ bus =>
          sco.pushDut(WishboneTransaction.sampleAsMaster(bus))
        }
      }
      busOutDrivers.foreach(_.slaveSink())

      for(repeat <- 0 until 100){
        seq.generateTransactions(1000)

        while(!seq.isEmpty){
          val tran = seq.nextTransaction
          busInDriver.drive(tran, we = true)
          dut.clockDomain.waitSampling(1)
        }

        dut.clockDomain.waitSampling(10)
      }
    }
  }

  test("classicWishboneDecoder"){
    val size = 100
    val config = WishboneConfig(32,8)
    val decodings = for(i <- 1 to 20) yield SizeMapping(i*size,size-1)
    testDecoder(config,decodings,"classicWishboneDecoder")
  }

  test("pipelinedWishboneDecoder"){
    val size = 100
    val config = WishboneConfig(32,8).pipelined
    val decodings = for(i <- 1 to 20) yield SizeMapping(i*size,size-1)
    testDecoder(config,decodings,"pipelinedWishboneDecoder")
  }

  test("DecoderSelectorBug"){
    val config = WishboneConfig(32,16)
    val decodings = List(SizeMapping(0x2000,12 Bytes),SizeMapping(0x1000, 12 Bytes))
    testDecoder(config,decodings,"DecoderSelectorBug")
  }
}
