package spinal.tester.scalatest

import org.scalatest.FunSuite
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

class SpinalSimWishboneSimInterconTester extends FunSuite{
  def testIntercon(config : WishboneConfig,decodings : Seq[SizeMapping],masters: Int,description : String = ""): Unit = {
    val fixture = SimConfig.allOptimisation.compile(rtl = new WishboneInterconComponent(config,masters,decodings))
    fixture.doSim(description){ dut =>
      def send_transaction(id: BigInt,master: Wishbone,slaves: Seq[(Wishbone,SizeMapping)],req: Int = 10): Unit = {
        val scoreboard_master = ScoreboardInOrder[WishboneTransaction]()
        val sequencer_master = WishboneSequencer{
          WishboneTransaction(data=id)
        }
        val driver_master = new WishboneDriver(master,dut.clockDomain)
        def driver_slave(slave: (Wishbone,SizeMapping)) = new WishboneDriver(slave._1,dut.clockDomain)

        val monitor_master = WishboneMonitor(master,dut.clockDomain){ bus =>
          scoreboard_master.pushRef(WishboneTransaction.sampleAsSlave(bus))
        }

        def monitor_slave(slave: (Wishbone,SizeMapping),scoreboard: ScoreboardInOrder[WishboneTransaction]) = WishboneMonitor(slave._1,dut.clockDomain){ bus =>
          val ID = id
          val transaction = WishboneTransaction.sampleAsSlave(bus)
          transaction match{
            case WishboneTransaction(_,ID,_,_,_) => scoreboard.pushDut(transaction)
            case _ =>
          }
        }

        scala.util.Random.shuffle(slaves).foreach{slave =>
          driver_slave(slave).slaveSink()
          monitor_slave(slave, scoreboard_master)
          (0 to req).foreach{x => sequencer_master.addTransaction(WishboneTransaction(data=id).randomAdressInRange(slave._2))}
        }
        (1 to sequencer_master.transactions.size).foreach{ x =>
            driver_master.drive(sequencer_master.nextTransaction,true)
            dut.clockDomain.waitSampling()
        }
        waitUntil(scoreboard_master.matches >= req*sequencer_master.transactions.size)
      }

      dut.clockDomain.forkStimulus(period=10)

      SimTimeout(dut.io.busMasters.size*dut.io.busSlaves.size*10*100)

      dut.io.busMasters.foreach{ bus =>
        bus.CYC #= false
        bus.STB #= false
        bus.WE #= false
        bus.ADR #= 0
        bus.DAT_MOSI #= 0
        if(bus.config.useLOCK) bus.LOCK #= false
      }

      dut.io.busSlaves.foreach{ bus =>
        bus.ACK #= false
        bus.DAT_MISO #= 0
      }
      dut.clockDomain.waitSampling(10)
      val masters = dut.io.busMasters
      val slaves = dut.slaves
      val n_transactions = 10
      val masterPool = scala.collection.mutable.ListBuffer[SimThread]()
      val sss = scala.collection.mutable.ListBuffer[((Wishbone,Int),(Wishbone,SizeMapping))]()

      scala.util.Random.shuffle(masters.zipWithIndex).foreach{master =>
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
