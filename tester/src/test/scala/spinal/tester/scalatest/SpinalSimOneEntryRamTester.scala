package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core.sim._
import spinal.core._
import spinal.lib._

class SpinalSimOneEntryRamTester extends FunSuite{
  test("general"){
    SimConfig.withWave.doSim(new Component{
      val mem = Mem(Bits(8 bits), 1) randBoot()
      val writePort = slave(mem.writePort)
      val readSyncPort = slave(mem.readSyncPort)
      val readAsyncPort = new Area{
        val address = in(UInt(0 bits))
        val data = out(mem(address))
      }
    }){dut =>
      dut.clockDomain.forkStimulus(10)

      dut.writePort.valid #= true
      dut.writePort.data #= 42
      dut.clockDomain.waitSampling()
      dut.writePort.valid #= false
      dut.readSyncPort.cmd.valid #= true
      dut.clockDomain.waitSampling()

      var readSyncModel = 42
      var ramModel = 42
      for(repeat <- 0 to 100){
        dut.writePort.randomize()
        dut.readSyncPort.cmd.randomize()
        dut.readAsyncPort.address.randomize()

        dut.clockDomain.waitSampling()
        assert(dut.readSyncPort.rsp.toInt == readSyncModel)
        assert(dut.readAsyncPort.data.toInt == ramModel)
        if(dut.readSyncPort.cmd.valid.toBoolean){
          readSyncModel = ramModel
        }
        if(dut.writePort.valid.toBoolean){
          ramModel = dut.writePort.data.toInt
        }
        sleep(1)
        assert(dut.readAsyncPort.data.toInt == ramModel)
      }
    }
  }

  test("rwPort"){
    SimConfig.withWave.doSim(new Component{
      val mem = Mem(Bits(8 bits), 1) randBoot()
      val readWrite = new Area{
        val address = U""
        val writeData = in Bits(8 bits)
        val enable, write = in Bool()
        val readData = out(mem.readWriteSync(address, writeData, enable, write))
      }
      val readSyncPort = slave(mem.readSyncPort)
      val readAsyncPort = new Area{
        val address = in(UInt(0 bits))
        val data = out(mem(address))
      }
    }){dut =>
      dut.clockDomain.forkStimulus(10)

      dut.readWrite.enable #= true
      dut.readWrite.write #= true
      dut.readWrite.writeData #= 42
      dut.clockDomain.waitSampling()
      dut.readWrite.write #= true
      dut.readSyncPort.cmd.valid #= true
      dut.clockDomain.waitSampling()

      var readWriteModel = 42
      var readSyncModel = 42
      var ramModel = 42
      for(repeat <- 0 until 100){
        dut.readWrite.enable.randomize()
        dut.readWrite.write.randomize()
        dut.readWrite.writeData.randomize()
        dut.readSyncPort.cmd.randomize()
        dut.readAsyncPort.address.randomize()

        dut.clockDomain.waitSampling()
        assert(dut.readSyncPort.rsp.toInt == readSyncModel)
        assert(dut.readAsyncPort.data.toInt == ramModel)
        if(dut.readSyncPort.cmd.valid.toBoolean){
          readSyncModel = ramModel
        }
        if(dut.readWrite.enable.toBoolean && ! dut.readWrite.write.toBoolean){
          readWriteModel = ramModel
        }
        if(dut.readWrite.enable.toBoolean && dut.readWrite.write.toBoolean){
          ramModel = dut.readWrite.writeData.toInt
        }
        sleep(1)
        assert(dut.readAsyncPort.data.toInt == ramModel)
      }
    }
  }


  test("rom"){
    SimConfig.withWave.doSim(new Component{
      val mem = Mem(Bits(8 bits), 1) init(Seq(B"xAA"))
      val readSyncPort = slave(mem.readSyncPort)
      val readAsyncPort = new Area{
        val address = in(UInt(0 bits))
        val data = out(mem(address))
      }
    }){dut =>
      dut.clockDomain.forkStimulus(10)

      dut.clockDomain.waitSampling()
      dut.readSyncPort.cmd.valid #= true
      dut.clockDomain.waitSampling()

      var readSyncModel = 0xAA
      var ramModel = 0xAA
      for(repeat <- 0 until 100){
        dut.readSyncPort.cmd.randomize()
        dut.readAsyncPort.address.randomize()

        dut.clockDomain.waitSampling()
        assert(dut.readSyncPort.rsp.toInt == readSyncModel)
        assert(dut.readAsyncPort.data.toInt == ramModel)
        if(dut.readSyncPort.cmd.valid.toBoolean){
          readSyncModel = ramModel
        }
        sleep(1)
        assert(dut.readAsyncPort.data.toInt == ramModel)
      }
    }
  }
}
