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
import spinal.core.sim._
import spinal.sim._
import spinal.tester.{SpinalAnyFunSuite, SpinalTesterCocotbBase, SpinalTesterGhdlBase}

case class ExpectedDataStruct(bool: Boolean, bits: BigInt, uint: BigInt, sint: BigInt, enumeration: Int)

object RomTester {

  object MyEnum extends SpinalEnum {
    val a, b, c = newElement()
  }

  class DataStruct extends Bundle {
    val bool = Bool()
    val bits = Bits(9 bits)
    val uint = UInt(10 bits)
    val sint = SInt(11 bits)
    val enumeration = MyEnum()
  }

  object RomTestData {
    val ROM_CONTENT_BIGINT: Seq[BigInt] = Seq(
      BigInt(0x01234567L),
      BigInt(0x12345670L),
      BigInt(0x10293857L),
      BigInt(0x0abcfe23L),
      BigInt(0x02938571L),
      BigInt(0xabcfe230L),
      BigInt(0x717833aaL),
      BigInt(0x17833aa6L)
    )
    val ROM_CONTENT_BIGINT2: Seq[BigInt] = Seq(
      BigInt(0x01234567),
      BigInt(0x12345670),
      BigInt(0x10293857),
      BigInt(0x0abcfe23),
      BigInt(0x02938571),
      BigInt(0xabcfe230),
      BigInt(0x717833aa),
      BigInt(0x17833aa6)
    )
    val ROM_CONTENT_BITS: Seq[Bits] = ROM_CONTENT_BIGINT.map(B(_, 32 bits))
    val ROM_CONTENT_SINT: Seq[SInt] = ROM_CONTENT_BIGINT2.map(S(_, 32 bits))
    val ADDRESS_WIDTH = 3
    val ROM_DEPTH = 1 << ADDRESS_WIDTH
  }

  class RomTester extends Component {
    def lit(bool: Boolean, bits: Int, uint: Int, sint: Int, enumeration: MyEnum.E) = {
      val data = new DataStruct
      data.bool := Bool(bool)
      data.bits := B(bits, 9 bits)
      data.uint := U(uint, 10 bits)
      data.sint := S(sint, 11 bits)
      data.enumeration := enumeration
      data
    }
    def initValues = List(
      lit(false, 0, 0, 0, MyEnum.a),
      lit(true, 0, 0, 0, MyEnum.a),
      lit(false, 0x1ff, 0, 0, MyEnum.a),
      lit(false, 0, 0x3ff, 0, MyEnum.a),
      lit(false, 0, 0, -1, MyEnum.a),
      lit(false, 0, 0, 0, MyEnum.c),
      lit(false, 43, 74, 88, MyEnum.b)
    )

    val rom = Mem(new DataStruct, initValues)

    val address = in UInt (RomTestData.ADDRESS_WIDTH bits)

    val data = out(rom(address))

  }

  class RomTesterSymbols extends Component {
    val rom = Mem(Bits(32 bits), RomTestData.ROM_DEPTH) init (RomTestData.ROM_CONTENT_BITS)

    rom.write(address = U(0, RomTestData.ADDRESS_WIDTH bits), data = B(0, 32 bits), enable = True)

    val address = in UInt (RomTestData.ADDRESS_WIDTH bits)

    val data = out(rom(address))

  }

  class RomTesterSymbolsSInt extends Component {
    val rom = Mem(SInt(32 bits), RomTestData.ROM_DEPTH) init (RomTestData.ROM_CONTENT_SINT)

    rom.write(address = U(0, RomTestData.ADDRESS_WIDTH bits), data = S(0, 32 bits), enable = True)

    val address = in UInt (RomTestData.ADDRESS_WIDTH bits)

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
    import java.nio.file.{Files, Paths, StandardCopyOption}
    val source = Paths.get(s"$workspaceRoot/RomTester2.v_toplevel_rom.bin")
    val target = Paths.get(s"$pythonTestLocation/RomTester2.v_toplevel_rom.bin")
    Files.deleteIfExists(target)
    if (Files.exists(source)) {
      Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING)
    } else {
      println(s"Warning: Source file not found for RomTester2: $source")
    }
  }
}

class RomTesterCocotbBoot3 extends SpinalTesterCocotbBase {
  override def getName: String = "RomTester3"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/RomTester3"
  override def createToplevel: Component = new RomTester.RomTesterSymbols().setDefinitionName("RomTester3")
  override def noVhdl = true
  override def backendConfig(config: SpinalConfig) = config.copy(inlineRom = false)

  override def genVerilog: Unit = {
    super.genVerilog
    import java.nio.file.{Files, Paths, StandardCopyOption}
    for (i <- 0 to 3) {
      val source = Paths.get(s"$workspaceRoot/RomTester3.v_toplevel_rom_symbol$i.bin")
      val target = Paths.get(s"$pythonTestLocation/RomTester3.v_toplevel_rom_symbol$i.bin")
      Files.deleteIfExists(target)
      if (Files.exists(source)) {
        Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING)
      }
    }
  }
}

class SpinalSimRomTester extends SpinalAnyFunSuite {

  /** Verifies a ROM component with combinational read through simulation.
    * Performs random address access checks followed by an exhaustive check.
    *
    * @param dutGenerator A function that creates the DUT instance (() => T).
    * @param expectedRomValues The initial BigInt values used for ROM init (ignoring elaboration writes).
    * @param getAddress   Function to access the address input port of the DUT (T => UInt).
    * @param getData      Function to access the data output port of the DUT (T => BitVector).
    * @param addressWidth The width of the address bus in bits.
    * @param randomCycles The number of random access cycles to perform.
    * @tparam T The type of the DUT Component.
    */
  def verifyRomSimCombRead[T <: Component](
      dutGenerator: => T,
      expectedRomValues: Seq[BigInt],
      getAddress: T => UInt,
      getData: T => BitVector,
      addressWidth: Int,
      randomCycles: Int = 100
  ): Unit = {
    SimConfig.withFstWave.compile(dutGenerator).doSim { dut =>
      val romDepth = 1 << addressWidth

      println(s"Starting COMBINATIONAL ROM verification for ${dut.getClass.getSimpleName}")

      val romContentToCompare = expectedRomValues

      println(s"Running $randomCycles random access checks...")
      for (repeat <- 0 until randomCycles) {
        val randomAddr = scala.util.Random.nextInt(romDepth)
        getAddress(dut) #= randomAddr
        sleep(1)
        if (randomAddr < romContentToCompare.length) {
          val expectedData = romContentToCompare(randomAddr)
          val actualData = getData(dut).toBigInt

          val isSymbolRom =
            dut.isInstanceOf[RomTester.RomTesterSymbols] || dut.isInstanceOf[RomTester.RomTesterSymbolsSInt]
          if (!(isSymbolRom && randomAddr == 0)) {
            assert(
              actualData == expectedData,
              f"Random check failed at address $randomAddr%d: Expected $expectedData%x, Got $actualData%x"
            )
          } else {
            println(
              f"Info: Skipping check for address 0 on ${dut.getClass.getSimpleName} due to elaboration-time write."
            )
          }
        } else {}
      }
      println("Random access checks passed (with potential skips for addr 0).")

      println(s"Running exhaustive check for all $romDepth addresses...")
      for (addr <- 0 until romDepth) {
        getAddress(dut) #= addr
        sleep(1)
        if (addr < romContentToCompare.length) {
          val expectedData = romContentToCompare(addr)
          val actualData = getData(dut).toBigInt
          val isSymbolRom =
            dut.isInstanceOf[RomTester.RomTesterSymbols] || dut.isInstanceOf[RomTester.RomTesterSymbolsSInt]
          if (!(isSymbolRom && addr == 0)) {
            assert(
              actualData == expectedData,
              f"Exhaustive check failed at address $addr%d: Expected $expectedData%x, Got $actualData%x"
            )
          } else {}
        } else {}
      }
      println("Exhaustive check passed (with potential skips for addr 0).")
      println(s"COMBINATIONAL ROM verification for ${dut.getClass.getSimpleName} completed.")
    }
  }

  test("testBitsRomComb") {
    println("Testing COMBINATIONAL ROM with Bits data type...")
    verifyRomSimCombRead[RomTester.RomTesterSymbols](
      dutGenerator = new RomTester.RomTesterSymbols(),
      expectedRomValues = RomTester.RomTestData.ROM_CONTENT_BIGINT,
      getAddress = (dut: RomTester.RomTesterSymbols) => dut.address,
      getData = (dut: RomTester.RomTesterSymbols) => dut.data,
      addressWidth = RomTester.RomTestData.ADDRESS_WIDTH
    )
  }

  test("testSIntRomComb") {
    println("Testing COMBINATIONAL ROM with SInt data type...")
    verifyRomSimCombRead[RomTester.RomTesterSymbolsSInt](
      dutGenerator = new RomTester.RomTesterSymbolsSInt(),
      expectedRomValues = RomTester.RomTestData.ROM_CONTENT_BIGINT,
      getAddress = (dut: RomTester.RomTesterSymbolsSInt) => dut.address,
      getData = (dut: RomTester.RomTesterSymbolsSInt) => dut.data,
      addressWidth = RomTester.RomTestData.ADDRESS_WIDTH
    )
  }

  test("testDataStructRom") {
    println("Testing COMBINATIONAL ROM with DataStruct data type...")
    val randomCycles = 100
    val addressWidth = 3
    val romDepth = 1 << addressWidth

    val expectedValues: Seq[ExpectedDataStruct] = Seq(
      ExpectedDataStruct(bool = false, bits = 0, uint = 0, sint = 0, enumeration = RomTester.MyEnum.a.position),
      ExpectedDataStruct(bool = true, bits = 0, uint = 0, sint = 0, enumeration = RomTester.MyEnum.a.position),
      ExpectedDataStruct(bool = false, bits = 0x1ff, uint = 0, sint = 0, enumeration = RomTester.MyEnum.a.position),
      ExpectedDataStruct(bool = false, bits = 0, uint = 0x3ff, sint = 0, enumeration = RomTester.MyEnum.a.position),
      ExpectedDataStruct(bool = false, bits = 0, uint = 0, sint = -1, enumeration = RomTester.MyEnum.a.position),
      ExpectedDataStruct(bool = false, bits = 0, uint = 0, sint = 0, enumeration = RomTester.MyEnum.c.position),
      ExpectedDataStruct(bool = false, bits = 43, uint = 74, sint = 88, enumeration = RomTester.MyEnum.b.position)
    )

    val defaultValue =
      ExpectedDataStruct(bool = false, bits = 0, uint = 0, sint = 0, enumeration = RomTester.MyEnum.a.position)

    SimConfig.withFstWave.compile(new RomTester.RomTester()).doSim { dut =>
      println(s"Starting COMBINATIONAL ROM verification for ${dut.getClass.getSimpleName}")

      def checkData(addr: Int, expected: ExpectedDataStruct): Unit = {
        val actualBool = dut.data.bool.toBoolean
        val actualBits = dut.data.bits.toBigInt
        val actualUint = dut.data.uint.toBigInt
        val actualSint = dut.data.sint.toBigInt
        val actualEnum = dut.data.enumeration.toEnum.position

        assert(
          actualBool == expected.bool,
          f"Bool mismatch at addr $addr%d: Expected ${expected.bool}, Got $actualBool"
        )
        assert(
          actualBits == expected.bits,
          f"Bits mismatch at addr $addr%d: Expected ${expected.bits}%x, Got $actualBits%x"
        )
        assert(
          actualUint == expected.uint,
          f"UInt mismatch at addr $addr%d: Expected ${expected.uint}%x, Got $actualUint%x"
        )

        val expectedSintBigInt =
          if (expected.sint < 0) BigInt(expected.sint.toString(2).takeRight(11), 2) else expected.sint

        assert(
          dut.data.sint.toBigInt == expected.sint,
          f"SInt mismatch at addr $addr%d: Expected ${expected.sint}, Got ${dut.data.sint.toBigInt}"
        )
        assert(
          actualEnum == expected.enumeration,
          f"Enum mismatch at addr $addr%d: Expected ${expected.enumeration}, Got $actualEnum"
        )
      }

      println(s"Running $randomCycles random access checks...")
      for (repeat <- 0 until randomCycles) {
        val randomAddr = scala.util.Random.nextInt(romDepth)
        dut.address #= randomAddr
        sleep(1)
        if (randomAddr < expectedValues.length) {
          checkData(randomAddr, expectedValues(randomAddr))
        } else {
          checkData(randomAddr, defaultValue)
        }
      }
      println("Random access checks passed.")

      println(s"Running exhaustive check for all $romDepth addresses...")
      for (addr <- 0 until romDepth) {
        dut.address #= addr
        sleep(1)
        if (addr < expectedValues.length) {
          checkData(addr, expectedValues(addr))
        } else {
          checkData(addr, defaultValue)
        }
      }
      println("Exhaustive check passed.")
      println(s"COMBINATIONAL ROM verification for ${dut.getClass.getSimpleName} completed successfully.")
    }
  }
}
