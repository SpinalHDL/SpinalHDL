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

    def toExpected: ExpectedDataStruct = ExpectedDataStruct(
      bool = bool.toBoolean,
      bits = bits.toBigInt,
      uint = uint.toBigInt,
      sint = sint.toBigInt,
      enumeration = enumeration.toEnum.position
    )
  }

  object RomTestData {
    val ADDRESS_WIDTH = 3
    val ROM_DEPTH = 1 << ADDRESS_WIDTH
    val DATA_WIDTH = 32

    val ROM_CONTENT_BIT_PATTERNS: Seq[BigInt] = Seq(
      BigInt(0x01234567L),
      BigInt(0x12345670L),
      BigInt(0x10293857L),
      BigInt(0x0abcfe23L),
      BigInt(0x02938571L),
      BigInt(0xabcfe230L),
      BigInt(0x717833aaL),
      BigInt(0x17833aa6L)
    )

    val DATA_STRUCT_EXPECTED_VALUES: Seq[ExpectedDataStruct] = Seq(
      ExpectedDataStruct(bool = false, bits = 0, uint = 0, sint = 0, enumeration = MyEnum.a.position),
      ExpectedDataStruct(bool = true, bits = 0, uint = 0, sint = 0, enumeration = MyEnum.a.position),
      ExpectedDataStruct(bool = false, bits = 0x1ff, uint = 0, sint = 0, enumeration = MyEnum.a.position),
      ExpectedDataStruct(bool = false, bits = 0, uint = 0x3ff, sint = 0, enumeration = MyEnum.a.position),
      ExpectedDataStruct(bool = false, bits = 0, uint = 0, sint = -1, enumeration = MyEnum.a.position),
      ExpectedDataStruct(bool = false, bits = 0, uint = 0, sint = 0, enumeration = MyEnum.c.position),
      ExpectedDataStruct(bool = false, bits = 43, uint = 74, sint = 88, enumeration = MyEnum.b.position)
    )

    val DATA_STRUCT_DEFAULT_VALUE = ExpectedDataStruct(
      bool = false,
      bits = 0,
      uint = 0,
      sint = 0,
      enumeration = MyEnum.a.position
    )
  }

  class RomTester extends Component {
    private def lit(bool: Boolean, bitsVal: Int, uintVal: Int, sintVal: Int, enumVal: MyEnum.E): DataStruct = {
      val data = new DataStruct
      data.bool := Bool(bool)
      data.bits := B(bitsVal, 9 bits)
      data.uint := U(uintVal, 10 bits)
      data.sint := S(sintVal, 11 bits)
      data.enumeration := enumVal
      data
    }

    private def initValues: List[DataStruct] = List(
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
    val data = out(rom.readAsync(address = address))
  }

  class RomTesterSymbols extends Component {

    val rom = Mem(Bits(RomTestData.DATA_WIDTH bits), RomTestData.ROM_DEPTH) init {
      Seq(
        B(0x01234567L, 32 bits),
        B(0x12345670L, 32 bits),
        B(0x10293857L, 32 bits),
        B(0x0abcfe23L, 32 bits),
        B(0x02938571L, 32 bits),
        B(0xabcfe230L, 32 bits),
        B(0x717833aaL, 32 bits),
        B(0x17833aa6L, 32 bits)
      )
    }

    rom.write(address = U(0, RomTestData.ADDRESS_WIDTH bits), data = B(0, RomTestData.DATA_WIDTH bits), enable = False)
    val address = in UInt (RomTestData.ADDRESS_WIDTH bits)
    val data = out(rom.readAsync(address = address))
  }

  class RomTesterSymbolsSInt extends Component {

    val rom = Mem(SInt(RomTestData.DATA_WIDTH bits), RomTestData.ROM_DEPTH) init {
      Seq(
        S(0x01234567, 32 bits),
        S(0x12345670, 32 bits),
        S(0x10293857, 32 bits),
        S(0x0abcfe23, 32 bits),
        S(0x02938571, 32 bits),
        S(0xabcfe230, 32 bits),
        S(0x717833aa, 32 bits),
        S(0x17833aa6, 32 bits)
      )
    }

    // This write happens during elaboration and modifies the initial content at address 0
    rom.write(address = U(0, RomTestData.ADDRESS_WIDTH bits), data = S(0, RomTestData.DATA_WIDTH bits), enable = False)
    val address = in UInt (RomTestData.ADDRESS_WIDTH bits)
    val data = out(rom.readAsync(address = address))
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
  override def backendConfig(config: SpinalConfig) = config.copy(inlineRom = true) // Inline ROM for this test
}

class RomTesterCocotbBoot2 extends SpinalTesterCocotbBase {
  override def getName: String = "RomTester2"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/RomTester2"
  override def createToplevel: Component = new RomTester.RomTester().setDefinitionName(getName)
  override def noVhdl = true
  override def backendConfig(config: SpinalConfig) = config.copy(inlineRom = false) // Don't inline ROM

  override def genVerilog: Unit = {
    super.genVerilog
    // Copy generated ROM binary file to the Cocotb test location
    import java.nio.file.{Files, Paths, StandardCopyOption}
    val source = Paths.get(s"$workspaceRoot/${getName}.v_toplevel_rom.bin")
    val targetDir = Paths.get(pythonTestLocation)
    val target = targetDir.resolve(s"${getName}.v_toplevel_rom.bin")
    try {
      Files.createDirectories(targetDir)
      Files.deleteIfExists(target)
      if (Files.exists(source)) {
        Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING)
        println(s"Copied ROM file from $source to $target")
      } else {
        println(s"Warning: Source ROM file not found: $source")
      }
    } catch {
      case e: Exception => println(s"Error copying ROM file for $getName: ${e.getMessage}")
    }
  }
}

class RomTesterCocotbBoot3 extends SpinalTesterCocotbBase {
  override def getName: String = "RomTester3"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/RomTester3"
  override def createToplevel: Component = new RomTester.RomTesterSymbols().setDefinitionName(getName)
  override def noVhdl = true
  override def backendConfig(config: SpinalConfig) = config.copy(inlineRom = false) // Don't inline ROM

  override def genVerilog: Unit = {
    super.genVerilog
    // Copy generated ROM symbol binary files to the Cocotb test location
    import java.nio.file.{Files, Paths, StandardCopyOption}
    val targetDir = Paths.get(pythonTestLocation)
    try {
      Files.createDirectories(targetDir)

      val source = Paths.get(s"$workspaceRoot/${getName}.v_toplevel_rom.bin")
      val target = targetDir.resolve(s"${getName}.v_toplevel_rom.bin")
      Files.deleteIfExists(target)
      if (Files.exists(source)) {
        Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING)
        println(s"Copied ROM symbol file from $source to $target")
      } else {
        println(
          s"Warning: Main source ROM symbol file not found: $source"
        )
      }
    } catch {
      case e: Exception => println(s"Error copying ROM symbol files for $getName: ${e.getMessage}")
    }
  }
}

class SpinalSimRomTester extends SpinalAnyFunSuite {

  val RANDOM_CYCLES = 100

  val MASK_32_BIT = (BigInt(1) << RomTester.RomTestData.DATA_WIDTH) - 1

  test("testBitsRomComb") {
    val addressWidth = RomTester.RomTestData.ADDRESS_WIDTH
    val romDepth = RomTester.RomTestData.ROM_DEPTH
    val expectedRomValues = RomTester.RomTestData.ROM_CONTENT_BIT_PATTERNS
    // Flag indicating if an elaboration-time write to address 0 occurred
    val hasElabWriteAddr0 = false

    SimConfig.withFstWave.compile(new RomTester.RomTesterSymbols()).doSim { dut =>
      println(s"Starting simulation for ${dut.getClass.getSimpleName}")

      // Test random addresses
      for (repeat <- 0 until RANDOM_CYCLES) {
        val randomAddr = scala.util.Random.nextInt(romDepth)
        dut.address #= randomAddr
        sleep(1)
        val actualData = dut.data.toBigInt
        if (hasElabWriteAddr0 && randomAddr == 0) {
          assert(actualData == 0, f"Random check failed at address 0 (post-write): Expected 0, Got $actualData%x")
        } else if (randomAddr < expectedRomValues.length) {
          val expectedData = expectedRomValues(randomAddr)
          assert(
            actualData == expectedData,
            f"Random check failed at address $randomAddr%d: Expected pattern $expectedData%x, Got $actualData%x"
          )
        } else {
          // Out of bounds addresses should read 0
          assert(
            actualData == 0,
            f"Random check failed at address $randomAddr%d (out of bounds): Expected 0, Got $actualData%x"
          )
        }
      }

      // Test all addresses exhaustively
      for (addr <- 0 until romDepth) {
        dut.address #= addr
        sleep(1)
        val actualData = dut.data.toBigInt
        if (hasElabWriteAddr0 && addr == 0) {
          assert(actualData == 0, f"Exhaustive check failed at address 0 (post-write): Expected 0, Got $actualData%x")
        } else if (addr < expectedRomValues.length) {
          val expectedData = expectedRomValues(addr)
          assert(
            actualData == expectedData,
            f"Exhaustive check failed at address $addr%d: Expected pattern $expectedData%x, Got $actualData%x"
          )
        } else {
          // Out of bounds addresses should read 0
          assert(
            actualData == 0,
            f"Exhaustive check failed at address $addr%d (out of bounds): Expected 0, Got $actualData%x"
          )
        }
      }
    }
  }

  test("testSIntRomComb") {
    val addressWidth = RomTester.RomTestData.ADDRESS_WIDTH
    val romDepth = RomTester.RomTestData.ROM_DEPTH

    val expectedRomValues = RomTester.RomTestData.ROM_CONTENT_BIT_PATTERNS
    // Flag indicating if an elaboration-time write to address 0 occurred
    val hasElabWriteAddr0 = false

    SimConfig.withFstWave.compile(new RomTester.RomTesterSymbolsSInt()).doSim { dut =>
      println(s"Starting simulation for ${dut.getClass.getSimpleName}")

      // Test random addresses
      for (repeat <- 0 until RANDOM_CYCLES) {
        val randomAddr = scala.util.Random.nextInt(romDepth)
        dut.address #= randomAddr
        sleep(1)

        val actualDataBits = dut.data.toBigInt & MASK_32_BIT
        val actualSigned = dut.data.toBigInt

        if (hasElabWriteAddr0 && randomAddr == 0) {
          assert(actualSigned == 0, f"Random check failed at address 0 (post-write): Expected S(0), Got $actualSigned")
        } else if (randomAddr < expectedRomValues.length) {
          val expectedDataPattern = expectedRomValues(randomAddr)
          // For SInt, check the bit pattern matches, SpinalSim handles sign conversion for `actualSigned`
          assert(
            actualDataBits == expectedDataPattern,
            f"Random check failed at address $randomAddr%d: Expected BIT PATTERN $expectedDataPattern%x, Got pattern $actualDataBits%x (signed value $actualSigned)"
          )
        } else {
          // Out of bounds addresses should read 0 (bit pattern 0)
          assert(
            actualDataBits == 0,
            f"Random check failed at address $randomAddr%d (out of bounds): Expected pattern 0, Got $actualDataBits%x"
          )
        }
      }

      // Test all addresses exhaustively
      for (addr <- 0 until romDepth) {
        dut.address #= addr
        sleep(1)
        val actualDataBits = dut.data.toBigInt & MASK_32_BIT
        val actualSigned = dut.data.toBigInt

        if (hasElabWriteAddr0 && addr == 0) {
          assert(
            actualSigned == 0,
            f"Exhaustive check failed at address 0 (post-write): Expected S(0), Got $actualSigned"
          )
        } else if (addr < expectedRomValues.length) {
          val expectedDataPattern = expectedRomValues(addr)
          // For SInt, check the bit pattern matches
          assert(
            actualDataBits == expectedDataPattern,
            f"Exhaustive check failed at address $addr%d: Expected BIT PATTERN $expectedDataPattern%x, Got pattern $actualDataBits%x (signed value $actualSigned)"
          )
        } else {
          // Out of bounds addresses should read 0 (bit pattern 0)
          assert(
            actualDataBits == 0,
            f"Exhaustive check failed at address $addr%d (out of bounds): Expected pattern 0, Got $actualDataBits%x"
          )
        }
      }
    }
  }

  test("testDataStructRomComb") {
    val addressWidth = RomTester.RomTestData.ADDRESS_WIDTH
    val romDepth = RomTester.RomTestData.ROM_DEPTH
    val expectedValues = RomTester.RomTestData.DATA_STRUCT_EXPECTED_VALUES
    // DataStruct ROM does not have an elaboration write in the tested component (RomTester)

    SimConfig.withFstWave.compile(new RomTester.RomTester()).doSim { dut =>
      println(s"Starting simulation for ${dut.getClass.getSimpleName}")

      // Helper to check all fields of DataStruct
      def checkData(addr: Int, expected: ExpectedDataStruct): Unit = {
        val actual = dut.data.toExpected
        assert(
          actual.bool == expected.bool,
          f"Bool mismatch at addr $addr%d: Expected ${expected.bool}, Got ${actual.bool}"
        )
        assert(
          actual.bits == expected.bits,
          f"Bits mismatch at addr $addr%d: Expected ${expected.bits}%x, Got ${actual.bits}%x"
        )
        assert(
          actual.uint == expected.uint,
          f"UInt mismatch at addr $addr%d: Expected ${expected.uint}%x, Got ${actual.uint}%x"
        )
        assert(
          actual.sint == expected.sint,
          f"SInt mismatch at addr $addr%d: Expected ${expected.sint}, Got ${actual.sint}"
        )
        assert(
          actual.enumeration == expected.enumeration,
          f"Enum mismatch at addr $addr%d: Expected ${expected.enumeration}, Got ${actual.enumeration}"
        )
      }

      // Test random addresses within the initialized range
      for (repeat <- 0 until RANDOM_CYCLES) {
        val randomAddr = scala.util.Random.nextInt(romDepth)
        dut.address #= randomAddr
        sleep(1)
        // Only check addresses within the initialized data range
        if (randomAddr < expectedValues.length) {
          checkData(randomAddr, expectedValues(randomAddr))
        }
        // Addresses outside the initialized range will read a default value (all zeros/false)
        // This is not explicitly checked against DATA_STRUCT_DEFAULT_VALUE here, but implied by hardware behavior.
      }

      // Test all addresses exhaustively
      for (addr <- 0 until romDepth) {
        dut.address #= addr
        sleep(1)
        // Only check addresses within the initialized data range
        if (addr < expectedValues.length) {
          checkData(addr, expectedValues(addr))
        }
        // Addresses outside the initialized range will read a default value (all zeros/false)
      }
    }
  }
}
