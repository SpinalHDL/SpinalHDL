package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.tester

import spinal.lib.bus.wishbone._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.wishbone.sim._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import scala.collection.mutable
import spinal.lib._

class SpinalSimWishboneArbiterTester extends FunSuite{

  def clearWishboneBus(bus : Wishbone) : Unit = {
    if( bus.isMasterInterface) bus.CYC #= false
    if( bus.isMasterInterface) bus.ADR #= 0
    if( bus.isMasterInterface) bus.DAT_MOSI #= 0
    if(!bus.isMasterInterface) bus.DAT_MISO #= 0
    if( bus.isMasterInterface) bus.STB #= false
    if( bus.isMasterInterface) bus.WE #= false
    if(!bus.isMasterInterface) bus.ACK #= false

    ///////////////////////////
    // OPTIONAL FLOW CONTROS //
    ///////////////////////////
    if(bus.config.useSTALL && !bus.isMasterInterface) bus.STALL #= false
    if(bus.config.useERR   && !bus.isMasterInterface) bus.ERR #= false
    if(bus.config.useRTY   && !bus.isMasterInterface) bus.RTY #= false
    if(bus.config.useSEL   &&  bus.isMasterInterface) bus.SEL #= 0

    //////////
    // TAGS //
    //////////
    if(bus.config.useTGA &&  bus.isMasterInterface) bus.TGA #= 0
    if(bus.config.useTGC &&  bus.isMasterInterface) bus.TGC #= 0
    if(bus.config.useCTI &&  bus.isMasterInterface) bus.CTI #= 0
    if(bus.config.useBTE &&  bus.isMasterInterface) bus.BTE #= 0
    if(bus.config.useTGD && !bus.isMasterInterface) bus.TGD_MISO #= 0
    if(bus.config.useTGD &&  bus.isMasterInterface) bus.TGD_MOSI #= 0
   }

  test("WishboneArbiterRandomSingleTransaction"){
    val compiled = SimConfig.allOptimisation.compile(
      rtl = WishboneArbiter(
        config = WishboneConfig(8,8),
        inputCount = 20
      )
    )

    compiled.doSim("randomMaster"){ dut =>
      //dut.io.inputs.foreach{ bus => bus.clearAll() }
      //dut.io.output.clearAll()
      dut.io.inputs.suspendable.foreach{ bus =>
       bus.CYC #= false
       bus.STB #= false
       bus.WE #= false
       bus.ADR #= 0
       bus.DAT_MOSI #= 0
       //clearWishboneBus(bus)
      }
      //clearWishboneBus(dut.io.output)
      dut.io.output.ACK #= false

      dut.clockDomain.forkStimulus(period=10)
      dut.clockDomain.waitSampling(10)

      val driveMasters = for(masterbus <- dut.io.inputs) yield new WishboneDrive(masterbus, dut.clockDomain)

      val driveSlave = new WishboneSlave(dut.io.output, dut.clockDomain)

      val transactions = for(x <- (0 to (dut.io.inputs.size - 1))) yield new WishboneTransaction(x,x)

      for(x <- (0 to (dut.io.inputs.size - 1))){
        driveSlave.addTrigger(AddressRange(x,0)){ bus =>
          bus.DAT_MISO #= bus.ADR.toInt
        }
      }

      val masterPool = scala.collection.mutable.ListBuffer[SimThread]()
      scala.util.Random.shuffle(driveMasters zip transactions).suspendable.foreach{ Mst =>
        masterPool += fork{
          sleep(scala.util.Random.nextInt(15*dut.io.inputs.size).toLong)
          val rec = Mst._1.read(Mst._2)
          rec match{
            case WishboneTransaction(Mst._2.address,Mst._2.data ,_,_,_) => println("Master %d: Success".format(Mst._2.address))
            case _ => {
              println("Master %d: Failure".format(Mst._2.address))
              println("Transmitted: " + Mst.toString)
              println("Received:    " + rec.toString)
              simFailure()
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
