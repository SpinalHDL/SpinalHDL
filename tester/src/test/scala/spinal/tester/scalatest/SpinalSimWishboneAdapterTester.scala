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

case class AdapterTest(conf1 : WishboneConfig,conf2 : WishboneConfig) extends Component{
  val io = new Bundle {
    val wbm = slave(Wishbone(conf1))
    val wbs = master(Wishbone(conf2))
  }
  val adapter = WishboneAdapter(io.wbm,io.wbs,true)
  val ddd = Reg(Bool)
}

class SpinalSimWishboneAdapterTester extends FunSuite{
  test("StandardToPipelined"){
    val compiled = SimConfig.allOptimisation.compile(rtl = new AdapterTest(
      WishboneConfig(8,8),
      WishboneConfig(8,8).pipelined))

    compiled.doSim("AdapterStandardToPipelined"){ dut =>
        dut.io.wbm.CYC #= false
        dut.io.wbm.ACK #= false
        dut.io.wbm.STB #= false
        dut.io.wbm.WE #= false
        dut.io.wbm.ADR #= 0
        dut.io.wbm.DAT_MOSI #= 0

        dut.io.wbs.ACK #= false
        dut.io.wbs.STB #= false
        dut.io.wbs.DAT_MISO #= 0

      dut.clockDomain.forkStimulus(period=10)
      dut.clockDomain.waitSampling(10)

      val driveSlave =  new WishbonePipelinedSlave(dut.io.wbs, dut.clockDomain)
      val driveMaster = new WishboneDrive(dut.io.wbm, dut.clockDomain)
      val transactions = for(x <- (1 to 10)) yield WishboneTransaction(x, x)

      driveSlave.addTrigger(AddressRange(1,10)){ bus =>
        bus.DAT_MISO #= bus.ADR.toInt
      }

      transactions.suspendable.foreach{ Transaction =>
        // sleep(scala.util.Random.nextInt(100).toLong)
        dut.clockDomain.waitSampling()
        val rec = driveMaster.read(Transaction)
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
      dut.clockDomain.waitSampling()
      simSuccess()
    }
  }

  test("PipelinedToStandard"){
    val compiled = SimConfig.allOptimisation.withWave.compile(rtl = new AdapterTest(
      WishboneConfig(8,8).pipelined,
      WishboneConfig(8,8)))

    compiled.doSim("AdapterPipelinedToStandard"){ dut =>
        dut.io.wbm.CYC #= false
        dut.io.wbm.ACK #= false
        dut.io.wbm.STB #= false
        dut.io.wbm.WE #= false
        dut.io.wbm.ADR #= 0
        dut.io.wbm.DAT_MOSI #= 0

        dut.io.wbs.ACK #= false
        dut.io.wbs.STB #= false
        dut.io.wbs.DAT_MISO #= 0

      dut.clockDomain.forkStimulus(period=10)
      dut.clockDomain.waitSampling(10)

      val driveSlave =  new WishboneSlave(dut.io.wbs, dut.clockDomain)
      val driveMaster = new WishbonePipelinedDrive(dut.io.wbm, dut.clockDomain)
      val transactions = for(x <- (1 to 10)) yield WishboneTransaction(x, x)

      driveSlave.addTrigger(AddressRange(1,10)){ bus =>
        bus.DAT_MISO #= bus.ADR.toInt
      }

      transactions.suspendable.foreach{ Transaction =>
        // sleep(scala.util.Random.nextInt(100).toLong)
        dut.clockDomain.waitSampling()
        val rec = driveMaster.read(Transaction)
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
      dut.clockDomain.waitSampling()
      simSuccess()
    }
  }
}
