

/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package spinal.core

import org.scalatest.funsuite.AnyFunSuite
// import spinal.tester.scalatest.RomTester.{RomTesterSymbols, RomTesterSymbolsSInt}
import spinal.tester.{SpinalAnyFunSuite, SpinalTesterGhdlBase, SpinalTesterCocotbBase}

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
    s"cp $workspaceRoot/RomTester2.v_toplevel_rom.bin $pythonTestLocation".!
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
      s"cp $workspaceRoot/RomTester3.v_toplevel_rom_symbol$i.bin $pythonTestLocation".!
    }
  }
}

class SpinalSimRomTester extends SpinalAnyFunSuite {
  test("test1"){
    import spinal.core.sim._
    import spinal.sim._
    SimConfig.compile(new RomTester.RomTesterSymbols()).doSim{ dut =>
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
    SimConfig.compile(new RomTester.RomTesterSymbolsSInt()).doSim{ dut =>
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