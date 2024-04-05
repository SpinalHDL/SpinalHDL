package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
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
                                allowTagResize : Boolean = false,
                                autoconnect : Boolean = false) extends Component{
  val io = new Bundle{
    val busIN = slave(Wishbone(configIn))
    val busOUT = master(Wishbone(configOut))
  }
  val connectionArea = if(autoconnect) new Area {
    io.busIN <> io.busOUT
  } else new Area {
    val adapter = WishboneAdapter(io.busIN,io.busOUT,allowAddressResize,allowDataResize,allowTagResize)
  }
}

class SpinalSimWishboneAdapterTester extends SpinalAnyFunSuite{
  def testBus(confIN:WishboneConfig,confOUT:WishboneConfig,allowAddressResize: Boolean = false,allowDataResize: Boolean = false,allowTagResize: Boolean = false, autoconnect : Boolean = false, description : String = ""): Unit = {
    val fixture = SimConfig.allOptimisation.compile(rtl = new WishboneSimpleBusAdapted(confIN,confOUT, autoconnect=autoconnect){val miaou = out(RegNext(False))}.setDefinitionName(description))
    fixture.doSim(description){ dut =>
      dut.clockDomain.forkStimulus(period=10)

      val hostDriver = WishboneDriver(dut.io.busIN)
      val deviceDriver = WishboneDriver(dut.io.busOUT)

      dut.clockDomain.waitSampling(10)
      SimTimeout(1000*20*100)
      val sco = ScoreboardInOrder[WishboneTransaction]()

      val seq = WishboneSequencer.randomGen(confIN)

      new WishboneMonitor(dut.io.busIN, dut.clockDomain, true).addResponseCallback(
        (bus : Wishbone, transaction: WishboneTransaction, we: Boolean) =>
          sco.pushRef(transaction)
      )


      new WishboneMonitor(dut.io.busOUT, dut.clockDomain, true).addResponseCallback(
        (bus : Wishbone, transaction: WishboneTransaction, we: Boolean) =>
          sco.pushDut(transaction)
      )

      deviceDriver.slaveSink()

      for(repeat <- 0 until 100){
        seq.generateTransactions(10)

        while(!seq.isEmpty){
          val tran = seq.nextTransaction
          hostDriver.drive(tran, we = true)
          dut.clockDomain.waitSampling(1)
        }

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

  for(autoconnect <- Seq(true, false);
      inGranularity <- Seq(AddressGranularity.WORD, AddressGranularity.BYTE, AddressGranularity.UNSPECIFIED);
      outGranularity <- Seq(AddressGranularity.WORD, AddressGranularity.BYTE, AddressGranularity.UNSPECIFIED)) {
    val confIN = WishboneConfig(16,32, addressGranularity = inGranularity)
    val confOUT = WishboneConfig(16,32, addressGranularity = outGranularity).pipelined
    val description = "classicToPipelined_" + (if (autoconnect) "auto" else "adapter") + "_" + inGranularity + "_" + outGranularity
    test(description) {
      testBus(confIN, confOUT, autoconnect = autoconnect, description = description)
    }
  }

  test("pipelinedToClassic"){
    val confIN = WishboneConfig(8,8).pipelined
    val confOUT = WishboneConfig(8,8)
    testBus(confIN,confOUT,description="pipelinedToClassic")
  }
}