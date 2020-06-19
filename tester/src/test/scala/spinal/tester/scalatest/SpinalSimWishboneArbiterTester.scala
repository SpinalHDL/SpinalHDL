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

class WishboneArbiterComponent(config : WishboneConfig,size: Int) extends Component{
  val io = new Bundle{
    val busIN = Vec(slave(Wishbone(config)),size)
    val busOUT = master(Wishbone(config))
  }
  val ff = Reg(Bool)
  val arbiter = WishboneArbiter(io.busIN,io.busOUT)
}

class SpinalSimWishboneArbiterTester extends FunSuite{
  def testArbiter(config : WishboneConfig,size: Int,description : String = ""): Unit = {
    val fixture = SimConfig.allOptimisation.compile(rtl = new WishboneArbiterComponent(config,size))
    fixture.doSim(description){ dut =>
      dut.clockDomain.forkStimulus(period=10)
      dut.io.busIN.foreach{ bus =>
        bus.CYC #= false
        bus.STB #= false
        bus.WE #= false
        bus.ADR #= 0
        bus.DAT_MOSI #= 0
        if(bus.config.useLOCK) bus.LOCK #= false
        }
        //if(dut.io.busOUT.config.isPipelined) dut.io.busOUT.STALL #= false
        dut.io.busOUT.ACK #= false
        if(dut.io.busOUT.config.useERR)dut.io.busOUT.ERR #= false
        dut.io.busOUT.DAT_MOSI #= 0
      dut.clockDomain.waitSampling(10)
      SimTimeout(1000*10000)
      val score = ScoreboardInOrder[WishboneTransaction]()


      //val drivers = dut.io.busIN.map{bus => new WishboneDriver(bus,dut.clockDomain)}
      val receiver = new WishboneDriver(dut.io.busOUT,dut.clockDomain)
      val receiveMonitor = WishboneMonitor(dut.io.busOUT,dut.clockDomain){ bus =>
        score.pushRef(WishboneTransaction.sampleAsMaster(bus))
        //pushDut
      }
      val driverMonitor = dut.io.busIN.map{ busIN =>
        WishboneMonitor(busIN,dut.clockDomain){ bus =>
          score.pushDut(WishboneTransaction.sampleAsMaster(bus))
        }
      }

      receiver.slaveSink()
      for(repeat <- 0 until 100){
        val masterPool = scala.collection.mutable.ListBuffer[SimThread]()
        dut.io.busIN.foreach{ bus =>
        val dri = new WishboneDriver(bus,dut.clockDomain)
          masterPool += fork{
            val seq = WishboneSequencer{
              WishboneTransaction().randomizeAddress(100).randomizeData(100)
            }
            seq.generateTransactions(10)
            sleep(Random.nextInt(100))
            while(!seq.isEmpty){
              dri.drive(seq.nextTransaction,true)
              dut.clockDomain.waitSampling(1)
              sleep(Random.nextInt(100))
            }
          }
        }
        masterPool.foreach{process => process.join()}
        dut.clockDomain.waitSampling(10)
      }
    }
  }

  test("classicWishboneArbiter"){
    val size = 20
    val config = WishboneConfig(32,8)
    testArbiter(config,size,"classicWishboneArbiter")
  }


  test("pipelinedWishboneArbiter"){
    val size = 20
    val config = WishboneConfig(32,8).pipelined
    testArbiter(config,size,"pipelinedWishboneArbiter")
  }

  test("classicWishboneArbiterSEL"){
    val size = 20
    val config = WishboneConfig(32,8,selWidth = 2,useERR = true,useLOCK = true)
    testArbiter(config,size,"classicWishboneArbiterSEL")
  }

  test("WishboneArbiterBug"){
    val size = 2
    val config = WishboneConfig(addressWidth=32,
                                dataWidth=8,
                                // selWidth=2,
                                useRTY=true)
                                // useERR=true)
    testArbiter(config,size,"WishboneArbiterBug")
  }
  // test("pipelinedWishboneDecoder"){
  //   val size = 100
  //   val config = WishboneConfig(32,8).pipelined
  //   val decodings = for(i <- 1 to 20) yield SizeMapping(i*size,size-1)
  //   testDecoder(config,decodings,"pipelinedWishboneDecoder")
  // }
}
