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

  class InterconTest extends Component{
    val io = new Bundle{
        val masters = Vec(slave(Wishbone(WishboneConfig(16,8))),3)
        val slaves = Vec(master(Wishbone(WishboneConfig(8,8))),5)
    }
    val config = WishboneConfig(16,8)
    val intercon = new WishboneInterconFactory(config)
    val slvs = for( x <- (1 to io.slaves.size+1)) yield SizeMapping(10*x,10)

    intercon.addMasters(io.masters)
    intercon.addSlavesWithAdapter(io.slaves zip slvs)

    intercon.build()
  }

class SpinalSimWishboneInterconTester extends FunSuite{
  test("RandomIntercon"){
    val compiled = SimConfig.allOptimisation.withWave.compile(rtl = new InterconTest)
    compiled.doSim("randomTransactionIntercon"){ dut =>
      dut.io.masters.suspendable.foreach{ bus =>
        bus.CYC #= false
        bus.ACK #= false
        bus.STB #= false
        bus.WE #= false
        bus.ADR #= 0
        bus.DAT_MOSI #= 0
      }

      dut.io.slaves.suspendable.foreach{ bus =>
        bus.ACK #= false
        bus.STB #= false
        bus.DAT_MISO #= 0
      }

      SimTimeout(1000000*10)

      dut.clockDomain.forkStimulus(period=10)
      dut.clockDomain.waitSampling(10)

      val driveSlaves = for(slavebus <- dut.io.slaves) yield new WishboneSlave(slavebus, dut.clockDomain)
      val driveMasters = for(masterbus <- dut.io.masters) yield new WishboneDrive(masterbus, dut.clockDomain)
      val transactions = for(x <- (1 to (dut.io.slaves.size))) yield WishboneTransaction(x*10, x*10)

      for( x <- driveSlaves.zipWithIndex){
        x._1.addTrigger(AddressRange((x._2+1)*10,10)){ bus => bus.DAT_MISO #= (x._2+1)*10}
      }

      val masterPool = scala.collection.mutable.ListBuffer[SimThread]()

      scala.util.Random.shuffle(driveMasters).suspendable.foreach{ Mst =>
        masterPool += fork{
          sleep(scala.util.Random.nextInt(100).toLong)
          scala.util.Random.shuffle(transactions).suspendable.foreach{ Transaction =>
            dut.clockDomain.waitSampling()
            val rec = Mst.read(Transaction)
            rec match{
              case Transaction => println("Master %d: Success".format(Transaction.address))
              case _ => {
                println("Master %d: Failure".format(Transaction.address))
                println("Transmitted: " + Transaction.toString)
                println("Received:    " + rec.toString)
                simFailure()
              }
            }
          }
        }
      }
      masterPool.suspendable.foreach{th => th.join}
      dut.clockDomain.waitSampling()
      simSuccess()

    }
  }
}
