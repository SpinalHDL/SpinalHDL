package spinal.lib.com.spi.ddr

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba3.apb.Apb3
import spinal.sim._
import spinal.lib._
import spinal.lib.sim.{FlowMonitor, ScoreboardInOrder, SimData}
import spinal.tester.{SpinalAnyFunSuite, SpinalSimTester}

import scala.collection.mutable.ListBuffer
import scala.util.Random


//case class SpiDdrSlave(spi: SpiDdrMaster) {
//  fork{
//    while(true){
//      waitUntil(spi.sclk.)
//    }
//  }
//  def expectWrite(data: Int, mod: ParameterMod): Unit = {
//
//  }
//
//  def read(data: Int, mod: ParameterMod): Unit = {
//
//  }
//}
//
//
//case class SpinalSimSpiDdrCmdData(read: Boolean, write: Boolean, data: Int)
//case class SpinalSimSpiDdrCmdSs(id : Int, enable : Boolean)



class SpinalSimSpiXdrMaster extends SpinalAnyFunSuite {
  SpinalSimTester { env =>
    import env._
    var compiled: SimCompiled[SpiXdrMasterCtrl.TopLevel] = null

    def doCompile(): Unit ={
      compiled = SimConfig.withConfig(SpinalConfig(verbose = true)).compile(
        SpiXdrMasterCtrl(
          SpiXdrMasterCtrl.Parameters(12, 12, SpiXdrParameter(dataWidth = 4, ioRate = 2, ssWidth = 3))
            .addFullDuplex(0, rate = 1, ddr = false)
            .addFullDuplex(1, rate = 1, ddr = true)
            .addFullDuplex(2, rate = 2, ddr = false)
            .addFullDuplex(3, rate = 2, ddr = true)

            .addHalfDuplex(4, rate = 1, ddr = false, spiWidth = 2)
            .addHalfDuplex(5, rate = 1, ddr = true, spiWidth = 2)
            .addHalfDuplex(6, rate = 2, ddr = false, spiWidth = 2)
            .addHalfDuplex(7, rate = 2, ddr = true, spiWidth = 2)

            .addHalfDuplex(8, rate = 1, ddr = false, spiWidth = 4)
            .addHalfDuplex(9, rate = 1, ddr = true, spiWidth = 4)
            .addHalfDuplex(10, rate = 2, ddr = false, spiWidth = 4)
            .addHalfDuplex(11, rate = 2, ddr = true, spiWidth = 4)
            .addFullDuplex(12, rate = 1, ddr = false, dataWidth = 9)
            .addFullDuplex(13, rate = 2, ddr = true, dataWidth = 10)
            .addHalfDuplex(14, rate = 1, ddr = false, spiWidth = 2, dataWidth = 12)
            .addHalfDuplex(15, rate = 1, ddr = true,  spiWidth = 2, dataWidth = 12)
        )
      )
    }
    test(prefix + "compile") {
      doCompile()
    }


    for (cpol <- List(false, true); cpha <- List(false, true); repeat <- 0 to 0) {
      val name = s"$prefix test cpol=$cpol cpha=$cpha $repeat"
      test(name) {
        if(compiled == null){
          doCompile()
        }
        compiled.doSim(name, 32) { dut =>
          SimTimeout(10 * 1000000)
          dut.clockDomain.forkStimulus(10)
          dut.io.config.kind.cpha #= cpha
          dut.io.config.kind.cpol #= cpol
          dut.io.config.ss.activeHigh #= 0
          dut.io.cmd.valid #= false
          dut.clockDomain.waitSampling()
          dut.clockDomain.waitSampling()


          val rspScoreboard = ScoreboardInOrder[Int]()
          val rspMonitor = FlowMonitor(dut.io.rsp, dut.clockDomain) { rsp =>
            rspScoreboard.pushDut(rsp.data.toInt)
          }

          while (rspScoreboard.matches < (300 * durationFactor).toInt) {
            dut.io.cmd.valid #= true
            //          if(Random.nextFloat() < 0.8){
            val mod = dut.p.mods.apply(Random.nextInt(dut.p.mods.length))
            val writeData, readData = Random.nextInt(1 << mod.dataWidth)
            val write, read = Random.nextBoolean()
            val sclkToggle = Random.nextInt(1 << 2)

            dut.io.cmd.kind #= false
            dut.io.cmd.data #= writeData
            dut.io.cmd.write #= write
            dut.io.cmd.read #= read
            dut.io.config.mod #= mod.id
            dut.io.config.sclkToggle #= sclkToggle

            if (read) rspScoreboard.pushRef(readData)
            val spiThread = fork {
              for (beat <- 0 until mod.dataWidth / mod.bitrate) {
                for (counter <- 0 until (if (mod.clkRate == 1) (sclkToggle + 1) * (if (mod.slowDdr) 1 else 2) else 1)) {
                  dut.clockDomain.waitSampling()
                  if (mod.clkRate != 1) {
                    assert(dut.io.spi.sclk.write.toInt == ((0 until dut.p.spi.ioRate).filter(i => i / (dut.p.spi.ioRate / mod.clkRate) % 2 == 1).map(1 << _).reduce(_ | _) ^ (if (cpol ^ cpha) (1 << dut.p.spi.ioRate) - 1 else 0)))
                  } else {
                    if (mod.slowDdr)
                      assert(dut.io.spi.sclk.write.toInt == (if (cpol ^ cpha ^ (beat % 2 == 1)) (1 << dut.p.spi.ioRate) - 1 else 0))
                    else
                      assert(dut.io.spi.sclk.write.toInt == (if (cpol ^ cpha ^ (counter > sclkToggle)) (1 << dut.p.spi.ioRate) - 1 else 0))
                  }
                  val beatBuffer = beat
                  fork {
                    dut.clockDomain.waitSampling()
                    for ((pin, tasks) <- mod.readMapping.groupBy(_.pin)) {
                      dut.io.spi.data(pin).read #= tasks.filter(m => ((readData >> m.target + mod.dataWidth - (beatBuffer + 1) * mod.bitrate) & 1) != 0).map(1 << _.phase).fold(0)(_ | _)
                    }
                  }
                  for (m <- mod.writeMapping) {
                    assert(dut.io.spi.data(m.pin).writeEnable.toBoolean == write || mod.ouputHighWhenIdle)
                    if (!write && mod.ouputHighWhenIdle) {
                      assert(((dut.io.spi.data(m.pin).write.toInt >> m.phase) & 1) == 1)
                    } else {
                      assert(((dut.io.spi.data(m.pin).write.toInt >> m.phase) & 1) == ((writeData >> m.source + mod.dataWidth - (beatBuffer + 1) * mod.bitrate) & 1))
                    }
                  }
                }
              }
            }

            dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
            dut.io.cmd.valid #= false
            for (repeat <- 0 until Random.nextInt(4)) {
              dut.clockDomain.waitSampling()
              assert(dut.io.spi.sclk.write.toInt == (if (cpol) 3 else 0))
              if (!mod.ouputHighWhenIdle)
                dut.io.spi.data.foreach(data => assert(data.writeEnable.toBoolean == false))
              else {
                //TODO
              }
            }
          }
        }
      }
      Thread.sleep(1000)
    }
  }
}
