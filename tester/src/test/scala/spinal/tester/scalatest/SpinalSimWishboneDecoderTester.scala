package spinal.tester.scalatest

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._

import org.scalatest.FunSuite
import spinal.lib.bus.wishbone._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.wishbone.sim._
import spinal.tester

case class DecoderTest(config : WishboneConfig, decodings : Seq[SizeMapping]) extends Component{
  val io = new Bundle {
    val input = slave(Wishbone(config))
    val outputs = Vec(master(Wishbone(config)),decodings.size)
  }
  val decoder = WishboneDecoder(config,decodings)

  io.input <> decoder.io.input
  io.outputs <> decoder.io.outputs

  val dummy = Reg(Bool)
}

class SpinalSimWishboneDecoderTester extends FunSuite{
  test("randomTransaction"){
    val compiled = SimConfig.allOptimisation.compile(
      rtl = new DecoderTest(
        config = WishboneConfig(8,8),
        decodings = List(SizeMapping(10,10),SizeMapping(20,10),SizeMapping(30,10),SizeMapping(40,10),SizeMapping(50,10),SizeMapping(60,10),SizeMapping(70,10),SizeMapping(80,10),SizeMapping(90,10))
      )
    )

    compiled.doSim("randomTransaction"){ dut =>
      dut.io.outputs.suspendable.foreach{ bus =>
       bus.CYC #= false
       bus.ACK #= false
       bus.STB #= false
       bus.WE #= false
       bus.ADR #= 0
       bus.DAT_MOSI #= 0
      }

      dut.io.input.CYC #= false
      dut.io.input.ACK #= false
      dut.io.input.STB #= false

      dut.clockDomain.forkStimulus(period=10)
      dut.clockDomain.waitSampling(10)

      val driveSlave = for(masterbus <- dut.io.outputs) yield new WishboneSlave(masterbus, dut.clockDomain)

      val driveMaster = new WishboneDrive(dut.io.input, dut.clockDomain)

      val transactions = for(decode <- dut.decodings) yield WishboneTransaction(decode.base.toLong, decode.size.toLong)

      for( x <- (driveSlave zip dut.decodings)){
        x._1.addTrigger(AddressRange(x._2.base,x._2.size.toInt)){ bus => bus.DAT_MISO #= bus.ADR.toInt}
      }

      transactions.suspendable.foreach{ Tran =>
        dut.clockDomain.waitSampling()
        val rec = driveMaster.read(Tran)
        rec match{
          case WishboneTransaction(Tran.address,Tran.address,_,_,_) => println("Address %d: Success".format(Tran.address))
          case _ => {
            println("Address %d: Failure".format(Tran.address))
            println("Transmitted: " + Tran.toString)
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