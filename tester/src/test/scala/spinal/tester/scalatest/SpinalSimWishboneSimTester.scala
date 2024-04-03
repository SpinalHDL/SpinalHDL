package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim.{SimCompiled, _}
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
  val ff = Reg(Bool())
  io.busmaster <> io.busslave
}
class SpinalSimWishboneSimTester extends SpinalAnyFunSuite{

  lazy val compiled : SimCompiled[wishbonesimplebus] = SimConfig.allOptimisation.compile(new wishbonesimplebus(WishboneConfig(8, 8)).setDefinitionName("wishbonesimplebus_std"))
  lazy val compPipe : SimCompiled[wishbonesimplebus] = SimConfig.allOptimisation.compile(new wishbonesimplebus(WishboneConfig(8, 8).pipelined).setDefinitionName("wishbonesimplebus_pipe"))


  test("compile") {
    compiled
    compPipe
  }

 test("DriveSingle"){
   compiled.doSim("DriveSingle") { dut =>
     dut.clockDomain.forkStimulus(period=10)
     val dri = WishboneDriver(dut.io.busmaster)
     val dri2 = WishboneDriver(dut.io.busslave)
     dut.clockDomain.waitSampling(10)
     SimTimeout(20 us)
     val sco = ScoreboardInOrder[WishboneTransaction]()

     val seq = WishboneSequencer.randomGen(dut.io.busmaster.config, maxCnt = 1)

     WishboneMonitor(dut.io.busmaster){ bus =>
       sco.pushRef(WishboneTransaction.sampleAsSlave(bus))
     }

     WishboneMonitor(dut.io.busslave){ bus =>
       sco.pushDut(WishboneTransaction.sampleAsSlave(bus))
     }

     dri2.slaveSink()

     for(repeat <- 0 until 1000){
       seq.generateTransactions(10)
       while(!seq.isEmpty){
         val tran = seq.nextTransaction
         dri.drive(tran, we = true)
         dut.clockDomain.waitSampling(1)
       }
       dut.clockDomain.waitSampling(10)
     }
   }
 }

 test("DriveCycle"){
   compiled.doSim("DriveCycle"){ dut =>
     dut.clockDomain.forkStimulus(period=10)
     val dri = new WishboneDriver(dut.io.busmaster, dut.clockDomain)
     val dri2 = new WishboneDriver(dut.io.busslave, dut.clockDomain)

     dut.clockDomain.waitSampling(10)
     SimTimeout(10000 * 1000)
      val sco = ScoreboardInOrder[WishboneTransaction]()

      val seq = WishboneSequencer.randomGen(dut.io.busmaster.config, 20)

     WishboneMonitor(dut.io.busmaster){ bus =>
       sco.pushRef(WishboneTransaction.sampleAsSlave(bus))
     }

     WishboneMonitor(dut.io.busslave){ bus =>
       sco.pushDut(WishboneTransaction.sampleAsSlave(bus))
     }

     dri2.slaveSink()

     for(repeat <- 0 until 1000){
       seq.generateTransactions(10)
       while(!seq.isEmpty){
         val tran = seq.nextTransaction
         dri.drive(tran, we = true)
         dut.clockDomain.waitSampling(1)
       }
       dut.clockDomain.waitSampling(10)
     }
   }
 }

  test("DriveSinglePipelined"){
    compPipe.doSim("DriveSinglePipelined"){ dut =>
      dut.clockDomain.forkStimulus(period=10)
      val mDriver = WishboneDriver(dut.io.busmaster)
      val sDriver = WishboneDriver(dut.io.busslave)
      dut.clockDomain.waitSampling(10)

      SimTimeout(1000 * 1000 * 10)
      val sco = ScoreboardInOrder[WishboneTransaction]()

      val seq = WishboneSequencer.randomGen(dut.io.busmaster.config)

      WishboneMonitor(dut.io.busmaster){ bus =>
        sco.pushRef(WishboneTransaction.sampleAsMaster(bus))
      }

      WishboneMonitor(dut.io.busslave){ bus =>
        sco.pushDut(WishboneTransaction.sampleAsMaster(bus))
      }

      sDriver.slaveSink()

      for(repeat <- 0 until 1000){
        seq.generateTransactions(10)
        while(!seq.isEmpty){
          val tran = seq.nextTransaction
          mDriver.drive(tran, true)
          dut.clockDomain.waitSampling(5)
        }
        dut.clockDomain.waitSampling(10)
      }
    }
  }

  test("DriveCyclePipelined"){
    compPipe.doSim("DriveCyclePipelined"){ dut =>
      dut.clockDomain.forkStimulus(period=10)
      val dri = WishboneDriver(dut.io.busmaster)
      val dri2 = WishboneDriver(dut.io.busslave)
      dut.clockDomain.waitSampling(10)

      SimTimeout(1000 * 1000000)
      val sco = ScoreboardInOrder[WishboneTransaction]()

      val seq = WishboneSequencer.randomGen(dut.io.busmaster.config, 20)

      WishboneMonitor(dut.io.busmaster){ bus =>
        sco.pushRef(WishboneTransaction.sampleAsSlave(bus))
      }

      WishboneMonitor(dut.io.busslave){ bus =>
        sco.pushDut(WishboneTransaction.sampleAsSlave(bus))
      }

      dri2.slaveSink()

      for(repeat <- 0 until 1000){
        seq.generateTransactions(10)

        while(!seq.isEmpty){
          val tran = seq.nextTransaction
          dri.drive(tran ,true)
          dut.clockDomain.waitSampling(5)
        }

        dut.clockDomain.waitSampling(10)
      }
    }
  }
}
