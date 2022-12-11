package spinal.core

import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._

import spinal.tester.{SpinalTesterCocotbBase, SpinalTesterGhdlBase, SpinalSimFunSuite}
import org.scalatest.funsuite.AnyFunSuite

object MemTester extends App{
  class MemTester extends Component {
    val mem = Mem(Bits(32 bits), 16)
    mem.initBigInt(List(
      0xbf6c8ffel,
      0x6f431025l,
      0xde894aeel,
      0xb007da48l,
      0xe39eca02l,
      0x1732378dl,
      0xd47822e3l,
      0xac711616l,
      0x0d45b17el,
      0xd863729dl,
      0x2210305el,
      0x73cf58e5l,
      0x80d3c954l,
      0xf56fb952l,
      0x9580e19cl,
      0xdd3b620dl
    ))

    val readSync = slave(mem.readSyncPort)
    val readAsync = new Bundle {
      val address = in UInt(4 bits)
      val data = out(mem.readAsync(address))
    }
    val write = new Bundle {
      val valid = in Bool()
      val address = in UInt(4 bits)
      val data = in Bits(32 bits)
      val mask = in Bits(4 bits)
      mem.write(address,data,valid,mask)
    }
    val readWrite = new Bundle {
      val valid = in Bool()
      val write = in Bool()
      val address = in UInt(4 bits)
      val writeData = in Bits(32 bits)
      val mask = in Bits(4 bits)
      val readData = out(mem.readWriteSync(address,writeData,valid,write,mask))

    }
  }

  SpinalVerilog(new MemTester)
  SpinalVhdl(new MemTester)
}

object RomTester {
  object MyEnum extends SpinalEnum{
    val a,b,c = newElement()
  }

  class DataStruct extends Bundle{
    val bool = Bool()
    val bits = Bits(9 bits)
    val uint = UInt(10 bits)
    val sint = SInt(11 bits)
    val enumeration = MyEnum()
  }

  class RomTester extends Component {
    def lit(bool : Boolean,bits : Int,uint : Int,sint : Int,enumeration : MyEnum.E) = {
      val data = new DataStruct
      data.bool := Bool(bool)
      data.bits := B(bits)
      data.uint := U(uint)
      data.sint := S(sint)
      data.enumeration := enumeration
      data
    }
    def initValues = List(
      lit(false,0,0,0,MyEnum.a),
      lit(true,0,0,0,MyEnum.a),
      lit(false,0x1FF,0,0,MyEnum.a),
      lit(false,0,0x3FF,0,MyEnum.a),
      lit(false,0,0,-1,MyEnum.a),
      lit(false,0,0,0,MyEnum.c),
      lit(false,43,74,88,MyEnum.b)
    )
    val rom = Mem(new DataStruct,initValues)

    val address = in UInt(3 bits)
    val data = out(rom(address))
  }

  class RomTesterSymbols extends Component {
    val rom = Mem(Bits(32 bits),8) init(Seq(
      BigInt(0x01234567l),
      BigInt(0x12345670l),
      BigInt(0x10293857l),
      BigInt(0x0abcfe23l),
      BigInt(0x02938571l),
      BigInt(0xabcfe230l),
      BigInt(0x717833aal),
      BigInt(0x17833aa6l)
    ))
    rom.write(U"000",B(0, 32 bits),False,B"0000")
    val address = in UInt(3 bits)
    val data = out(rom(address))
  }

  class RomTesterSymbolsSInt extends Component {
    val rom = Mem(SInt(32 bits),8) init(Seq(
      S(0x01234567, 32 bits),
      S(0x12345670, 32 bits),
      S(0x10293857, 32 bits),
      S(0x0abcfe23, 32 bits),
      S(0x02938571, 32 bits),
      S(0xabcfe230, 32 bits),
      S(0x717833aa, 32 bits),
      S(0x17833aa6, 32 bits)
    ))
    rom.write(U"000",S(0, 32 bits),False,B"0000")
    val address = in UInt(3 bits)
    val data = out(rom(address))
  }
}

import RomTester.{RomTesterSymbols, RomTesterSymbolsSInt}

class RomTesterGhdlBoot extends SpinalTesterGhdlBase {
  override def getName: String = "RomTester"
  override def createToplevel: Component = new RomTester.RomTester
}

class RomTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "RomTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/RomTester"
  override def createToplevel: Component = new RomTester.RomTester
  override def noVhdl = true
  override def backendConfig(config: SpinalConfig) = config.copy(inlineRom = true)
}

class RomTesterCocotbBoot2 extends SpinalTesterCocotbBase {
  override def getName: String = "RomTester2"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/RomTester2"
  override def createToplevel: Component = new RomTester.RomTester().setDefinitionName("RomTester2")
  override def noVhdl = true
  override def backendConfig(config: SpinalConfig) = config.copy(inlineRom = false)

  override def genVerilog: Unit = {
    super.genVerilog
    import scala.sys.process._
    s"rm $pythonTestLocation/RomTester2.v_toplevel_rom.bin".!
    s"cp RomTester2.v_toplevel_rom.bin $pythonTestLocation".!
  }
}

class RomTesterCocotbBoot3 extends SpinalTesterCocotbBase {
  override def getName: String = "RomTester2"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/RomTester3"
  override def createToplevel: Component = new RomTester.RomTesterSymbols().setDefinitionName("RomTester3")
  override def noVhdl = true
  override def backendConfig(config: SpinalConfig) = config.copy(inlineRom = false)

  override def genVerilog: Unit = {
    super.genVerilog
    import scala.sys.process._
    for(i <- 0 to 3) {
      s"rm $pythonTestLocation/RomTester3.v_toplevel_rom_symbol$i.bin".!
      s"cp RomTester3.v_toplevel_rom_symbol$i.bin $pythonTestLocation".!
    }
  }
}

class SpinalSimRomTester extends AnyFunSuite {
  test("test1"){
    import spinal.core.sim._
    import spinal.sim._
    SimConfig.compile(new RomTesterSymbols()).doSim{ dut =>
      val rom = Seq(
        BigInt(0x01234567l),
        BigInt(0x12345670l),
        BigInt(0x10293857l),
        BigInt(0x0abcfe23l),
        BigInt(0x02938571l),
        BigInt(0xabcfe230l),
        BigInt(0x717833aal),
        BigInt(0x17833aa6l)
      )

      for(repeat <- 0 until 100){
        dut.address.randomize()
        sleep(1)
        assert(dut.data.toBigInt == rom(dut.address.toInt))
      }

    }
  }

  test("testSInt"){
    import spinal.core.sim._
    import spinal.sim._
    SimConfig.compile(new RomTesterSymbolsSInt()).doSim{ dut =>
      val rom = Seq(
        BigInt(0x01234567),
        BigInt(0x12345670),
        BigInt(0x10293857),
        BigInt(0x0abcfe23),
        BigInt(0x02938571),
        BigInt(0xabcfe230),
        BigInt(0x717833aa),
        BigInt(0x17833aa6)
      )

      for(repeat <- 0 until 100){
        dut.address.randomize()
        sleep(1)
        assert(dut.data.toBigInt == rom(dut.address.toInt))
      }
    }
  }
}

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


class SpinalSimRamTester extends AnyFunSuite {
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
