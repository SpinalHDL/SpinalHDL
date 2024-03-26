package spinal.tester.scalatest

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.wishbone._
import spinal.lib.wishbone.sim._
import spinal.lib.sim._

import scala.language.postfixOps

class WishboneSimpleSlave(config : WishboneConfig) extends Component{
  val io = new Bundle{
    val bus = slave(Wishbone(config))
  }
  val busCtrl = WishboneSlaveFactory(io.bus)
  val regA = busCtrl.createReadOnly(Bits(busCtrl.busDataWidth bits), 10 * config.dataWidth / 8)
  regA := 100
  val regB = busCtrl.createReadAndWrite(Bits(busCtrl.busDataWidth bits), 0)
  regB.init(1)
}

class SpinalSimWishboneSlaveFactoryTester extends SpinalAnyFunSuite{
  def testBus(conf:WishboneConfig, description : String = ""): Unit = {
    val fixture = SimConfig.allOptimisation.compile(rtl = new WishboneSimpleSlave(conf))
    fixture.doSim(description){ dut =>
      dut.clockDomain.forkStimulus(period=10)
      SimTimeout(1000*20*100)

      dut.io.bus.CYC #= false
      dut.io.bus.STB #= false
      dut.io.bus.WE #= false
      dut.io.bus.ADR #= 0
      dut.io.bus.DAT_MOSI #= 0
      if(dut.io.bus.config.useLOCK) dut.io.bus.LOCK #= false
      if(dut.io.bus.config.isPipelined) dut.io.bus.STALL #= false

      dut.clockDomain.waitSampling()

      val wordInc = conf.wordAddressInc(AddressGranularity.WORD)

      val scoreboard = ScoreboardInOrder[WishboneTransaction]()
      for(ref <- List(
        WishboneTransaction(10 * wordInc, 100), WishboneTransaction(0, 1),
        WishboneTransaction(10 * wordInc, 100), WishboneTransaction(0, 1),
        WishboneTransaction(10 * wordInc, 100), WishboneTransaction(0, 2)
      )) {
        scoreboard.pushRef(ref)
      }

      WishboneMonitor(dut.io.bus, dut.clockDomain){ bus =>
           scoreboard.pushDut(WishboneTransaction.sampleAsMaster(bus))
      }

      val dri = new WishboneDriver(dut.io.bus, dut.clockDomain)
      dri.drive(scala.collection.immutable.Seq(WishboneTransaction(10*wordInc), WishboneTransaction(0)), we = false)
      dri.drive(scala.collection.immutable.Seq(WishboneTransaction(10*wordInc, 200), WishboneTransaction(0, 2)), we = true)
      dri.drive(scala.collection.immutable.Seq(WishboneTransaction(10*wordInc), WishboneTransaction(0)), we = false)

      scoreboard.checkEmptyness()
//
//      for(repeat <- 0 until 100){
//        driver.drive(for(x <- 1 to 10) yield WishboneTransaction(address=10).randomizeData(dut.io.bus.config.dataWidth), we = true)
//        dut.clockDomain.waitSampling(10)
//        driver.drive(for(x <- 1 to 10) yield WishboneTransaction(address=10), we = false)
//        dut.clockDomain.waitSampling(10)
//      }
//

    }
  }

  test("classic"){
    val conf = WishboneConfig(8,8)
    testBus(conf,description="classic")
  }

  test("classic32"){
    val conf = WishboneConfig(32, 32)
    testBus(conf, description="classic32")
  }

  test("classic32_byte"){
    val conf = WishboneConfig(32, 32, addressGranularity = AddressGranularity.BYTE)
    testBus(conf, description="classic32_byte")
  }

  test("classic32_word"){
    val conf = WishboneConfig(32, 32, addressGranularity = AddressGranularity.WORD)
    testBus(conf, description="classic32_word")
  }

  //  test("pipelined"){
//    val conf = WishboneConfig(8,8).pipelined
//    testBus(conf,description="pipelined")
//  }
}