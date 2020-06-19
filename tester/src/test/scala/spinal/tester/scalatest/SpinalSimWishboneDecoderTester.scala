package spinal.tester.scalatest

import org.scalatest.FunSuite
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
  val ff = Reg(Bool)
  val outs = io.busOUT zip decodings
  val decoder = WishboneDecoder(io.busIN,outs)
}

class SpinalSimWishboneDecoderTester extends FunSuite{
  def testDecoder(config : WishboneConfig,decodings : Seq[SizeMapping],description : String = ""): Unit = {
    val fixture = SimConfig.allOptimisation.compile(rtl = new WishboneDecoderComponent(config,decodings))
    fixture.doSim(description){ dut =>
      dut.clockDomain.forkStimulus(period=10)
      dut.io.busIN.CYC #= false
      dut.io.busIN.STB #= false
      dut.io.busIN.WE #= false
      dut.io.busIN.ADR #= 0
      dut.io.busIN.DAT_MOSI #= 0
      dut.io.busOUT.foreach{ bus =>
        if(bus.config.isPipelined) bus.STALL #= false
        bus.ACK #= false
        bus.DAT_MOSI #= 0
      }
      dut.clockDomain.waitSampling(10)
      SimTimeout(1000*10000)
      val sco = ScoreboardInOrder[WishboneTransaction]()
      val dri = new WishboneDriver(dut.io.busIN, dut.clockDomain)

      val seq = WishboneSequencer{
        WishboneTransaction().randomAdressInRange(Random.shuffle(decodings).head).randomizeData(Math.pow(2,config.dataWidth).toInt-1)
      }

      val monIN = WishboneMonitor(dut.io.busIN, dut.clockDomain){ bus =>
        sco.pushRef(WishboneTransaction.sampleAsMaster(bus))
      }

      dut.io.busOUT.foreach{busOut =>
        WishboneMonitor(busOut, dut.clockDomain){ bus =>
        sco.pushDut(WishboneTransaction.sampleAsMaster(bus))
        }
      }

      dut.io.busOUT.foreach{busOut =>
        val driver = new WishboneDriver(busOut, dut.clockDomain)
        driver.slaveSink()
      }

      for(repeat <- 0 until 100){
        seq.generateTransactions(1000)
        val ddd = fork{
          while(!seq.isEmpty){
            val tran = seq.nextTransaction
            dri.drive(tran ,true)
            dut.clockDomain.waitSampling(1)
          }
        }
        ddd.join()
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
