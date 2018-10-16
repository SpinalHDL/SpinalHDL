package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.com.i2c._
import spinal.sim._
import spinal.lib._
import spinal.lib.com.spi.ddr._
import spinal.lib.sim.{FlowMonitor, ScoreboardInOrder, SimData}

import scala.collection.mutable.ListBuffer
import scala.util.Random


//case class SpiDdrSlave(spi: SpiDdrMaster) {
//  fork{
//    while(true){
//      waitUntil(spi.sclk.)
//    }
//  }
//  def expectWrite(data: Int, mod: ParameterMod): Unit@suspendable = {
//
//  }
//
//  def read(data: Int, mod: ParameterMod): Unit@suspendable = {
//
//  }
//}
//
//
//case class SpinalSimSpiDdrCmdData(read: Boolean, write: Boolean, data: Int)
//case class SpinalSimSpiDdrCmdSs(id : Int, enable : Boolean)

class SpinalSimSpiDdrMaster extends FunSuite {

  var compiled : SimCompiled[SpiDdrMasterCtrl.TopLevel] = null
  test("compile"){
    compiled = SimConfig.withWave.withConfig(SpinalConfig(verbose = true)).compile(
      SpiDdrMasterCtrl(
        SpiDdrMasterCtrl.Parameters(8,12,SpiDdrParameter(dataWidth=4, ioRate = 2, ssWidth=3))
//          .addFullDuplex(0, rate = 1, ddr = false)
//          .addFullDuplex(1, rate = 1, ddr = true)
//          .addFullDuplex(2, rate = 2, ddr = false)
//          .addFullDuplex(3, rate = 2, ddr = true)

//          .addHalfDuplex(2, rate = 1, ddr = false, spiWidth = 2)
          .addHalfDuplex(3, rate = 1, ddr = true, spiWidth = 2)
      )
    )
  }


  for(cpol <- List(false, true); cpha <- List(false, true); repeat <- 0 to 0) {
    val name = s"test cpol=$cpol cpha=$cpha $repeat"
    test(name) {
      compiled.doSim(name, 32) { dut =>
        SimTimeout(10*1000000)
        dut.clockDomain.forkStimulus(10)
        dut.io.config.kind.cpha #= cpha
        dut.io.config.kind.cpol #= cpol
        dut.io.cmd.valid #= false
        dut.clockDomain.waitSampling()
        dut.clockDomain.waitSampling()


        val rspScoreboard = ScoreboardInOrder[Int]()
        val rspMonitor = FlowMonitor(dut.io.rsp, dut.clockDomain){ rsp =>
          rspScoreboard.pushDut(rsp.data.toInt)
        }

        while(rspScoreboard.matches < 300){
          dut.io.cmd.valid #= true
//          if(Random.nextFloat() < 0.8){
          val writeData, readData = Random.nextInt(256)
          val write, read = Random.nextBoolean()
          val sclkToogle = Random.nextInt(1 << 2)
          val mod = dut.p.mods.apply(Random.nextInt(dut.p.mods.length))

          dut.io.cmd.kind #= false
          dut.io.cmd.data #= writeData
          dut.io.cmd.write #= write
          dut.io.cmd.read #= read
          dut.io.config.mod #= mod.id
          dut.io.config.sclkToogle #= sclkToogle

          if(read) rspScoreboard.pushRef(readData)
          val spiThread = fork{
            var beat = 0
            val spiWidth = mod.bitrate
            val ddr = mod.ddr
            val fullRate = mod.clkRate != 1
            val dataRate = if(ddr) spiWidth *2 else spiWidth
            val dataMask = (1 << dataRate) - 1
            Suspendable.repeat(8/dataRate){
              var counter = 0
              Suspendable.repeat(if (!fullRate) (sclkToogle+1) * 2 else 1) {
                dut.clockDomain.waitSampling()
                val high = counter > sclkToogle
                if(fullRate){
                  assert(dut.io.spi.sclk.write.toInt == (if(cpol ^ cpha) 1 else 2))
                } else {
                  assert(dut.io.spi.sclk.write.toInt == (if(cpol ^ cpha ^ high) 3 else 0))
                }
                val beatBuffer = beat
                fork{
                  dut.clockDomain.waitSampling()
                  if(mod.id == 0)
                    dut.io.spi.data(1).read #= ((readData >> (8-dataRate-beatBuffer*dataRate)) & 1)*3
                  else{
                    for(i <- 0 until spiWidth) {
                      if(!ddr)
                        dut.io.spi.data(i).read #= ((readData >> (8-dataRate-beatBuffer*dataRate + i)) & 1)*3
                      else{
                        if(!fullRate){
                          dut.io.spi.data(i).read #= ((readData >> (8-(if(!high) spiWidth else dataRate)-beatBuffer*dataRate + i)) & 1)*3
                        } else {
                          dut.io.spi.data(i).read #= ((readData >> (8-dataRate-beatBuffer*dataRate + i)) & 1) + ((readData >> (8-spiWidth-beatBuffer*dataRate + i)) & 1)*2
                        }
                      }
                    }
                  }
                }

                for(i <- 0 until spiWidth) {
                  if(!ddr)
                    assert(dut.io.spi.data(i).write.toInt == ((writeData >> (8-dataRate-beat*dataRate+i)) & 1) * 3)
                  else{
                    if(!fullRate){
                      assert(dut.io.spi.data(i).write.toInt == ((writeData >> (8-(if(!high) spiWidth else dataRate)-beat*dataRate + i)) & 1)*3)
                    } else {
                      assert(dut.io.spi.data(i).write.toInt == ((writeData >> (8-dataRate-beat*dataRate + i)) & 1)*2 + ((writeData >> (8-spiWidth-beat*dataRate + i)) & 1))
                    }
                  }
                  assert(dut.io.spi.data(i).writeEnable.toBoolean == write)
                }
                counter += 1
              }
              beat += 1
            }
          }
//          } else {
//            dut.io.cmd.kind #= true
//          }

          dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
          dut.io.cmd.valid #= false
          Suspendable.repeat(Random.nextInt(4)){
            dut.clockDomain.waitSampling()
            assert(dut.io.spi.sclk.write.toInt == (if(cpol) 3 else 0))
            dut.io.spi.data.foreach(data => assert(data.writeEnable.toBoolean == false))
          }


        }
      }
    }
  }

}

