package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import spinal.sim._
import spinal.lib._
import spinal.lib.bus.wishbone._
import spinal.lib.wishbone.sim._
import spinal.lib.sim._
import scala.util.Random

class WishboneSimpleBusAdapted( configIn : WishboneConfig,
                                configOut : WishboneConfig,
                                allowAddressResize : Boolean = false,
                                allowDataResize : Boolean = false,
                                allowTagResize : Boolean = false) extends Component{
  val io = new Bundle{
    val busIN = slave(Wishbone(configIn))
    val busOUT = master(Wishbone(configOut))
  }
  val ff = Reg(Bool)
  val adapter = WishboneAdapter(io.busIN,io.busOUT,allowAddressResize,allowDataResize,allowTagResize)
}

class SpinalSimWishboneAdapterTester extends FunSuite{
  def testBus(confIN:WishboneConfig,confOUT:WishboneConfig,allowAddressResize: Boolean = false,allowDataResize: Boolean = false,allowTagResize: Boolean = false, description : String = ""): Unit = {
    val fixture = SimConfig.allOptimisation.compile(rtl = new WishboneSimpleBusAdapted(confIN,confOUT))
    fixture.doSim(description){ dut =>
      dut.clockDomain.forkStimulus(period=10)
      dut.io.busIN.CYC #= false
      dut.io.busIN.STB #= false
      dut.io.busIN.WE #= false
      dut.io.busIN.ADR #= 0
      dut.io.busIN.DAT_MOSI #= 0
      if(dut.io.busOUT.config.isPipelined) dut.io.busOUT.STALL #= false
      dut.io.busOUT.ACK #= false
      dut.io.busOUT.DAT_MOSI #= 0
      dut.clockDomain.waitSampling(10)
      SimTimeout(1000*20*100)
      val sco = ScoreboardInOrder[WishboneTransaction]()
      val dri = new WishboneDriver(dut.io.busIN, dut.clockDomain)
      val dri2 = new WishboneDriver(dut.io.busOUT, dut.clockDomain)

      val seq = WishboneSequencer{
        WishboneTransaction(BigInt(Random.nextInt(200)),BigInt(Random.nextInt(200)))
        }

      val mon1 = WishboneMonitor(dut.io.busIN, dut.clockDomain){ bus =>
        sco.pushRef(WishboneTransaction.sampleAsMaster(bus))
      }

      val mon2 = WishboneMonitor(dut.io.busOUT, dut.clockDomain){ bus =>
        sco.pushDut(WishboneTransaction.sampleAsMaster(bus))
      }

      dri2.slaveSink()

      for(repeat <- 0 until 1000){
        seq.generateTransactions(10)
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

  test("passthroughAdapter"){
    val confIN = WishboneConfig(8,8)
    val confOUT = WishboneConfig(8,8)
    testBus(confIN,confOUT,description="passthroughAdapter")
  }

  test("passthroughAdapterPipelined"){
    val confIN = WishboneConfig(8,8).pipelined
    val confOUT = WishboneConfig(8,8).pipelined
    testBus(confIN,confOUT,description="passthroughAdapterPipelined")
  }

  test("classicToPipelined"){
    val confIN = WishboneConfig(8,8)
    val confOUT = WishboneConfig(8,8).pipelined
    testBus(confIN,confOUT,description="classicToPipelined")
  }

  test("pipelinedToClassic"){
    val confIN = WishboneConfig(8,8).pipelined
    val confOUT = WishboneConfig(8,8)
    testBus(confIN,confOUT,description="pipelinedToClassic")
  }
}