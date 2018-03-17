package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import scala.collection.mutable

import spinal.lib._
import spinal.lib.bus.wishbone._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.wishbone.sim._

class WishboneInterconFactory(){
  val masters = mutable.ListBuffer[Wishbone]()
  val slaves = mutable.Map[Wishbone,SizeMapping]()

  def addSlave(wb : Wishbone, mapping : SizeMapping) : Unit = {
    slaves += (wb -> mapping)
  }

  def addMaster(wb : Wishbone) : Unit = {
    masters += wb
  }

  def build() = new Area {
    val arbiters = for(slave <- slaves.unzip._1) yield new Area{
      val arbiter = new WishboneArbiter(slave.config, masters.size)
      arbiter.io.output <> slave
      arbiter.setPartialName(slave,"arbiter")
    }

    val decoders = for(master <- masters) yield new Area{
      val decoder = new WishboneDecoder(master.config, slaves.unzip._2.toList)
      decoder.io.input <> master
      decoder.setPartialName(master,"decoder")
    }

    for((arbiter,count_arb) <- (arbiters).zipWithIndex){
      for((decoder,count_dec) <- (decoders).zipWithIndex){
        decoder.decoder.io.outputs(count_arb) <> arbiter.arbiter.io.inputs(count_dec)
      }
    }
  }
}

class InterconTest extends Component{
  val io = new Bundle{
    val masters = Vec(slave(Wishbone(WishboneConfig(8,8))),10)
    val slaves = Vec(master(Wishbone(WishboneConfig(8,8))),10)
  }
  val intercon = new WishboneInterconFactory()
  val config = WishboneConfig(8,8)
  for(mst <- io.masters){
    intercon.addMaster(mst)
  }

  for(slv <- io.slaves.zipWithIndex){
    intercon.addSlave(slv._1,SizeMapping(10*(slv._2+1),10))
  }

  intercon.build()
}

object SpinalSimWishboneInterconTester{
  def main(args: Array[String]): Unit = {
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
            //println(Transaction.toString)
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
