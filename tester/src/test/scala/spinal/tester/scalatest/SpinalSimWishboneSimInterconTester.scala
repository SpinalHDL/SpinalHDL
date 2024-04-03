package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.tester
import spinal.core._
import spinal.core.sim._
import spinal.sim._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.bus.wishbone._
import spinal.lib.wishbone.sim._
import spinal.lib.sim._
import scala.util.Random
import scala.collection.Seq

class WishboneInterconComponent(config : WishboneConfig,n_masters: Int,decodings : Seq[SizeMapping]) extends Component{
  val io = new Bundle{
    val busMasters = Vec(slave(Wishbone(config)),n_masters)
    val busSlaves = Vec(master(Wishbone(config)),decodings.size)
  }
  val intercon = new WishboneInterconFactory()
  val slaves = io.busSlaves zip decodings
  val masters = io.busMasters.map(_ -> io.busSlaves)

  for( slave <- slaves){
    intercon.addSlave(slave._1 , slave._2)
  }
  for( master <- masters){
    intercon.addMaster(master._1,master._2)
  }
}

class SpinalSimWishboneSimInterconTester extends SpinalAnyFunSuite{

  def send_transaction(id: BigInt, driver_master: WishboneDriver, slaves: Seq[(Wishbone,SizeMapping)],req: Int = 10): Unit = {
    val scoreboard_master = ScoreboardInOrder[WishboneTransaction]()
    val sequencer_master = WishboneSequencer{
      WishboneTransaction(data=id)
    }

    val master = driver_master.bus
    val clockDomain = master.CYC.clockDomain

    WishboneMonitor(master){ bus =>
      scoreboard_master.pushRef(WishboneTransaction.sampleAsSlave(bus))
    }

    def monitor_slave(slave: (Wishbone,SizeMapping),scoreboard: ScoreboardInOrder[WishboneTransaction]): Unit = WishboneMonitor(slave._1){ bus =>
      val transaction = WishboneTransaction.sampleAsSlave(bus)
      if(id === transaction.data) {
        scoreboard.pushDut(transaction)
      }
    }

    scala.util.Random.shuffle(slaves).foreach{slave =>
      monitor_slave(slave, scoreboard_master)
      (0 to req).foreach{x => sequencer_master.addTransaction(WishboneTransaction(data=id).randomAdressInRange(slave._2))}
    }
    (1 to sequencer_master.transactions.size).foreach{ x =>
      driver_master.drive(sequencer_master.nextTransaction, true)
      clockDomain.waitSampling()
    }
    waitUntil(scoreboard_master.matches >= req*sequencer_master.transactions.size)
    scoreboard_master.checkEmptyness()
  }


  def testIntercon(config : WishboneConfig,decodings : Seq[SizeMapping],masters: Int,description : String = ""): Unit = {
    val fixture = SimConfig.allOptimisation.compile(rtl = new WishboneInterconComponent(config,masters,decodings))
    fixture.doSim(description){ dut =>
      dut.clockDomain.forkStimulus(period=10)

      SimTimeout(dut.io.busMasters.size*dut.io.busSlaves.size*10*100)

      val busDrivers = dut.io.busMasters.map(WishboneDriver(_))
      dut.io.busSlaves.map(WishboneDriver(_)).foreach(_.slaveSink())

      dut.clockDomain.waitSampling(10)

      val slaves = dut.slaves
      val masterPool = scala.collection.mutable.ListBuffer[SimThread]()

      scala.util.Random.shuffle(busDrivers.zipWithIndex).foreach{master =>
        masterPool += fork{
          send_transaction(master._2,master._1,slaves,1)
        }
      }

      masterPool.foreach{process => process.join()}
      dut.clockDomain.waitSampling(10)
    }
  }

  test("classicWishboneIntercon"){
    val masters = 10
    val slaves = 10
    val size = 1024
    val config = WishboneConfig(32,16)
    val decodings = for(i <- 1 to slaves) yield SizeMapping(i*size,size-1)
    decodings.foreach(println(_))
    testIntercon(config,decodings,masters,"classicWishboneIntercon")
  }

  test("pipelinedWishboneDecoder"){
    val masters = 10
    val slaves = 10
    val size = 1024
    val config = WishboneConfig(32,16).pipelined
    val decodings = for(i <- 1 to slaves) yield SizeMapping(i*size,size-1)
    decodings.foreach(println(_))
    testIntercon(config,decodings,masters,"pipelinedWishboneDecoder")
  }

  test("InterconSelectorBug"){
    val masters = 2
    val slaves = 2
    val config = WishboneConfig(32,16)
    val decodings = List(SizeMapping(0x2000,12 Bytes),SizeMapping(0x1000, 12 Bytes))
    testIntercon(config,decodings,masters,"InterconSelectorBug")
  }

}
