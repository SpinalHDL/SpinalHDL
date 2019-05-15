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

class wishbonesimplebus(config : WishboneConfig) extends Component{
  val io = new Bundle{
    val busmaster = slave(Wishbone(config))
    val busslave = master(Wishbone(config))
  }
  val ff = Reg(Bool)
  io.busmaster <> io.busslave
}
class SpinalSimWishboneSimTester extends FunSuite{
  val compiled = SimConfig.allOptimisation.compile(rtl = new wishbonesimplebus(WishboneConfig(8,8)))
  val compPipe = SimConfig.allOptimisation.compile(rtl = new wishbonesimplebus(WishboneConfig(8,8).pipelined))

 test("DriveSingle"){
   compiled.doSim("DriveSingle"){ dut =>
     dut.clockDomain.forkStimulus(period=10)
     dut.io.busmaster.CYC #= false
     dut.io.busmaster.STB #= false
     dut.io.busmaster.WE #= false
     dut.io.busmaster.ADR #= 0
     dut.io.busmaster.DAT_MOSI #= 0
     dut.io.busslave.ACK #= false
     dut.io.busslave.DAT_MOSI #= 0
     dut.clockDomain.waitSampling(10)
     SimTimeout(500 * 1000)
     val sco = ScoreboardInOrder[WishboneTransaction]()
     val dri = new WishboneDriver(dut.io.busmaster, dut.clockDomain)
     val dri2 = new WishboneDriver(dut.io.busslave, dut.clockDomain)

     val seq = WishboneSequencer{
       WishboneTransaction(BigInt(Random.nextInt(200)),BigInt(Random.nextInt(200)))
      }

     val mon1 = WishboneMonitor(dut.io.busmaster, dut.clockDomain){ bus =>
       sco.pushRef(WishboneTransaction.sampleAsMaster(bus))
     }

     val mon2 = WishboneMonitor(dut.io.busslave, dut.clockDomain){ bus =>
       sco.pushDut(WishboneTransaction.sampleAsMaster(bus))
     }

     dri2.slaveSink()

     for(repeat <- 0 until 1000){
       seq.generateTransactions(10)
       val ddd = fork{
         while(!seq.isEmpty){
           val tran = seq.nextTransaction
           dri.drive(tran ,true)
           dut.clockDomain.waitSampling(1)
         }
       }
       ddd.join()
       dut.clockDomain.waitSampling(10)
     }
   }
 }

 test("DriveCycle"){
   compiled.doSim("DriveCycle"){ dut =>
     dut.clockDomain.forkStimulus(period=10)
     dut.io.busmaster.CYC #= false
     dut.io.busmaster.STB #= false
     dut.io.busmaster.WE #= false
     dut.io.busmaster.ADR #= 0
     dut.io.busmaster.DAT_MOSI #= 0
     dut.io.busslave.ACK #= false
     dut.io.busslave.DAT_MOSI #= 0
     dut.clockDomain.waitSampling(10)
     SimTimeout(10000 * 1000)
      val sco = ScoreboardInOrder[WishboneTransaction]()
      val dri = new WishboneDriver(dut.io.busmaster, dut.clockDomain)
      val dri2 = new WishboneDriver(dut.io.busslave, dut.clockDomain)

      val seq = WishboneSequencer{
         for(i <- 0 to Random.nextInt(20))
           yield WishboneTransaction(BigInt(Random.nextInt(200)),BigInt(Random.nextInt(200)))
      }

     val mon1 = WishboneMonitor(dut.io.busmaster, dut.clockDomain){ bus =>
       sco.pushRef(WishboneTransaction.sampleAsMaster(bus))
     }

     val mon2 = WishboneMonitor(dut.io.busslave, dut.clockDomain){ bus =>
       sco.pushDut(WishboneTransaction.sampleAsMaster(bus))
     }

     dri2.slaveSink()

     for(repeat <- 0 until 1000){
       seq.generateTransactions(10)
       val ddd = fork{
         while(!seq.isEmpty){
           val tran = seq.nextTransaction
           dri.drive(tran ,true)
           dut.clockDomain.waitSampling(1)
         }
       }
       ddd.join()
       dut.clockDomain.waitSampling(10)
     }
   }
 }

  test("DriveSinglePipelined"){
    compPipe.doSim("DriveSinglePipelined"){ dut =>
      dut.clockDomain.forkStimulus(period=10)
      dut.io.busmaster.CYC #= false
      dut.io.busmaster.STB #= false
      dut.io.busslave.STALL #= false
      dut.io.busmaster.WE #= false
      dut.io.busmaster.ADR #= 0
      dut.io.busmaster.DAT_MOSI #= 0
      dut.io.busslave.ACK #= false
      dut.io.busslave.DAT_MOSI #= 0
      dut.clockDomain.waitSampling(10)

      SimTimeout(1000 * 1000)
      val sco = ScoreboardInOrder[WishboneTransaction]()
      val dri = new WishboneDriver(dut.io.busmaster, dut.clockDomain)
      val dri2 = new WishboneDriver(dut.io.busslave, dut.clockDomain)

      val seq = WishboneSequencer{
        WishboneTransaction(BigInt(Random.nextInt(200)),BigInt(Random.nextInt(200)))
       }

      val mon1 = WishboneMonitor(dut.io.busmaster, dut.clockDomain){ bus =>
        sco.pushRef(WishboneTransaction.sampleAsMaster(bus))
      }

      val mon2 = WishboneMonitor(dut.io.busslave, dut.clockDomain){ bus =>
        sco.pushDut(WishboneTransaction.sampleAsMaster(bus))
      }

      dri2.slaveSink()

      for(repeat <- 0 until 1000){
        seq.generateTransactions(10)
        val ddd = fork{
          while(!seq.isEmpty){
            val tran = seq.nextTransaction
            dri.drive(tran ,true)
            dut.clockDomain.waitSampling(5)
          }
        }
        ddd.join()
        dut.clockDomain.waitSampling(10)
      }
    }
  }

  test("DriveCyclePipelined"){
    compPipe.doSim("DriveCyclePipelined"){ dut =>
      dut.clockDomain.forkStimulus(period=10)
      dut.io.busmaster.CYC #= false
      dut.io.busmaster.STB #= false
      dut.io.busslave.STALL #= false
      dut.io.busmaster.WE #= false
      dut.io.busmaster.ADR #= 0
      dut.io.busmaster.DAT_MOSI #= 0
      dut.io.busslave.ACK #= false
      dut.io.busslave.DAT_MOSI #= 0
      dut.clockDomain.waitSampling(10)

      SimTimeout(1000 * 1000000)
      val sco = ScoreboardInOrder[WishboneTransaction]()
      val dri = new WishboneDriver(dut.io.busmaster, dut.clockDomain)
      val dri2 = new WishboneDriver(dut.io.busslave, dut.clockDomain)

      val seq = WishboneSequencer{
         for(i <- 0 to Random.nextInt(20))
           yield WishboneTransaction(BigInt(Random.nextInt(200)),BigInt(Random.nextInt(200)))
       }

      val mon1 = WishboneMonitor(dut.io.busmaster, dut.clockDomain){ bus =>
        sco.pushRef(WishboneTransaction.sampleAsMaster(bus))
      }

      val mon2 = WishboneMonitor(dut.io.busslave, dut.clockDomain){ bus =>
        sco.pushDut(WishboneTransaction.sampleAsMaster(bus))
      }

      dri2.slaveSink()

      for(repeat <- 0 until 1000){
        seq.generateTransactions(10)
        val ddd = fork{
          while(!seq.isEmpty){
            val tran = seq.nextTransaction
            dri.drive(tran ,true)
            dut.clockDomain.waitSampling(5)
          }
        }
        ddd.join()
        dut.clockDomain.waitSampling(10)
      }
    }
  }
}
