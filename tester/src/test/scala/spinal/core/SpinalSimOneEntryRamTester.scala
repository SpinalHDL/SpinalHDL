package spinal.core

import spinal.core.sim._
import spinal.lib._
import spinal.tester.{SpinalSimFunSuite, SpinalAnyFunSuite}

class SpinalSimOneEntryRamTester extends SpinalSimFunSuite{
  test("general") {
    SimConfig.doSim(new Component {
      val mem = Mem(Bits(8 bits), 1).randBoot()
      val writePort = slave(mem.writePort)
      val readSyncPort = slave(mem.readSyncPort)
      val readAsyncPort = new Area {
        val address = in(UInt(0 bits))
        val data = out(mem(address))
      }
    }) { dut =>
      dut.clockDomain.forkStimulus(10)

      dut.writePort.valid #= true
      dut.writePort.data #= 42
      dut.clockDomain.waitSampling()
      dut.writePort.valid #= false
      dut.readSyncPort.cmd.valid #= true
      dut.clockDomain.waitSampling()

      var readSyncModel = 42
      var ramModel = 42
      for (repeat <- 0 to 100) {
        dut.writePort.randomize()
        dut.readSyncPort.cmd.randomize()
        dut.readAsyncPort.address.randomize()

        dut.clockDomain.waitSampling()
        assert(dut.readSyncPort.rsp.toInt == readSyncModel)
        assert(dut.readAsyncPort.data.toInt == ramModel)
        if (dut.readSyncPort.cmd.valid.toBoolean) {
          readSyncModel = ramModel
        }
        if (dut.writePort.valid.toBoolean) {
          ramModel = dut.writePort.data.toInt
        }
        sleep(1)
        assert(dut.readAsyncPort.data.toInt == ramModel)
      }
    }
  }

  test("rwPort") {
    SimConfig.doSim(new Component {
      val mem = Mem(Bits(8 bits), 1).randBoot()
      val readWrite = new Area {
        val address = U""
        val writeData = in Bits (8 bits)
        val enable, write = in Bool()
        val readData = out(mem.readWriteSync(address, writeData, enable, write))
      }
      val readSyncPort = slave(mem.readSyncPort)
      val readAsyncPort = new Area {
        val address = in(UInt(0 bits))
        val data = out(mem(address))
      }
    }) { dut =>
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
      for (repeat <- 0 until 100) {
        dut.readWrite.enable.randomize()
        dut.readWrite.write.randomize()
        dut.readWrite.writeData.randomize()
        dut.readSyncPort.cmd.randomize()
        dut.readAsyncPort.address.randomize()

        dut.clockDomain.waitSampling()
        assert(dut.readSyncPort.rsp.toInt == readSyncModel)
        assert(dut.readAsyncPort.data.toInt == ramModel)
        if (dut.readSyncPort.cmd.valid.toBoolean) {
          readSyncModel = ramModel
        }
        if (dut.readWrite.enable.toBoolean && !dut.readWrite.write.toBoolean) {
          readWriteModel = ramModel
        }
        if (dut.readWrite.enable.toBoolean && dut.readWrite.write.toBoolean) {
          ramModel = dut.readWrite.writeData.toInt
        }
        sleep(1)
        assert(dut.readAsyncPort.data.toInt == ramModel)
      }
    }
  }

  test("rom") {
    SimConfig.doSim(new Component {
      val mem = Mem(Bits(8 bits), 1) init (Seq(B"xAA"))
      val readSyncPort = slave(mem.readSyncPort)
      val readAsyncPort = new Area {
        val address = in(UInt(0 bits))
        val data = out(mem(address))
      }
    }) { dut =>
      dut.clockDomain.forkStimulus(10)

      dut.clockDomain.waitSampling()
      dut.readSyncPort.cmd.valid #= true
      dut.clockDomain.waitSampling()

      var readSyncModel = 0xAA
      var ramModel = 0xAA
      for (repeat <- 0 until 100) {
        dut.readSyncPort.cmd.randomize()
        dut.readAsyncPort.address.randomize()

        dut.clockDomain.waitSampling()
        assert(dut.readSyncPort.rsp.toInt == readSyncModel)
        assert(dut.readAsyncPort.data.toInt == ramModel)
        if (dut.readSyncPort.cmd.valid.toBoolean) {
          readSyncModel = ramModel
        }
        sleep(1)
        assert(dut.readAsyncPort.data.toInt == ramModel)
      }
    }
  }
}


class SpinalSimRamTester extends SpinalAnyFunSuite {
  test("general") {
    SimConfig.withConfig(SpinalConfig(device = Device.XILINX)).compile(new Component {
      val ram = Mem(Bits(32 bits), 256)

      val wrEnable = in Bool()
      val wrAddress = in UInt(8 bits)
      val wrData = in Bits(32 bits)
      val wrMask = in Bits(4 bits)

      ram.write(wrAddress, wrData, wrEnable, wrMask)

      val rdAddress = in UInt(8 bits)
      val rdData = out Bits(32 bits)
      rdData := ram.readAsync(rdAddress)
    }).doSim{dut =>
      dut.clockDomain.forkStimulus(10)


      def wr(address : Int, data : Long, mask : Long): Unit ={
        dut.clockDomain.waitSampling()
        dut.wrEnable #= true
        dut.wrAddress #= address
        dut.wrData #= data
        dut.wrMask #= mask
        dut.clockDomain.waitSampling()
      }

      wr(42, 0x00112233l, 0x1)
      wr(43, 0x44556677l, 0x3)
      wr(44, 0x8899AABBl, 0xF)
      wr(42, 0xFFFFFFFFl, 0xE)
      wr(43, 0xFFFFFFFFl, 0xC)
      wr(44, 0xFFFFFFFFl, 0x0)

      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling()

      def rdCheck(address : Int, data : Long, mask : Long): Unit ={
        dut.rdAddress #= address
        sleep(1)
        assert((dut.rdData.toLong & mask) == (data & mask))
      }

      rdCheck(42, 0x00112233l, 0x000000FF)
      rdCheck(43, 0x44556677l, 0x0000FFFF)
      rdCheck(44, 0x8899AABBl, 0xFFFFFFFF)
    }
  }
}
