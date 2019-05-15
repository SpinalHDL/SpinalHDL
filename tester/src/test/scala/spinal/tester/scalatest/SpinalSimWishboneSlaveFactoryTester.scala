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

class WishboneSimpleSlave(config : WishboneConfig) extends Component{
  val io = new Bundle{
    val bus = slave(Wishbone(config))
  }
  val busCtrl = WishboneSlaveFactory(io.bus)
  val reg = busCtrl.createReadWrite(Bits(busCtrl.busDataWidth bits),10)
}

class SpinalSimWishboneSlaveFactoryTester extends FunSuite{
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

      dut.io.bus.ACK #= false
      dut.io.bus.DAT_MISO #= 0

      val scoreboard = ScoreboardInOrder[WishboneTransaction]()
      val driver = new WishboneDriver(dut.io.bus, dut.clockDomain)
      // val monitor = WishboneMonitor(dut.io.bus, dut.clockDomain){ bus =>
      //   if(bus.WE.toBoolean)
      //     scoreboard.pushRef(WishboneTransaction.sampleAsSlave(bus))
      //   else
      //     scoreboard.pushDut(WishboneTransaction.sampleAsMaster(bus))
      // }

      for(repeat <- 0 until 1000){
        driver.drive(for(x <- 1 to 10) yield WishboneTransaction(address=10).randomizeData(dut.io.bus.config.dataWidth),true)
        dut.clockDomain.waitSampling(10)
        driver.drive(for(x <- 1 to 10) yield WishboneTransaction(address=10),false)
        dut.clockDomain.waitSampling(10)

      }
    }
  }

  test("classic"){
    val conf = WishboneConfig(8,8)
    testBus(conf,description="classic")
  }

  test("pipelined"){
    val conf = WishboneConfig(8,8).pipelined
    testBus(conf,description="pipelined")
  }
}