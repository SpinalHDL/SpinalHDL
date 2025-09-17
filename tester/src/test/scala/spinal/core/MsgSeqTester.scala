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

import spinal.lib._
import spinal.core.sim._
import spinal.tester.SpinalSimFunSuite
import spinal.tester.SpinalTesterCocotbBase

import scala.util.Random
import scala.collection.Seq
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import org.scalatest.tools.Runner
import java.io.File
import scala.tools.nsc.ScalaDoc.Command
import scala.sys.process.ProcessLogger

trait Formattable {
  def format(): Seq[Any]
}

case class DataPayload() extends Bundle with Formattable {
  val value = UInt(16 bits)
  val checksum = UInt(8 bits)

  override def format(): Seq[Any] = {
    Seq(L"DataPayload(value=0x${value}, checksum=0x${checksum})")
  }
}

case class PacketHeader() extends Bundle with Formattable {
  val packetLength = UInt(8 bits)
  val packetType = UInt(4 bits)
  val payload = DataPayload()

  override def format(): Seq[Any] = {
    Seq(
      L"PacketHeader(",
      L"packetLength=0x${packetLength},",
      L" packetType=0x${packetType},",
      L" payload=${payload.format}",
      L")"
    ).flatten
  }
}

case class PacketFrame() extends Bundle with Formattable {
  val controlSignal = in(Bool())
  val packetIn = in(PacketHeader())
  val processedValue = out(UInt(16 bits))
  val isValid = out(Bool())

  override def format(): Seq[Any] = {
    Seq(
      L"PacketFrame(",
      L"controlSignal=${controlSignal},",
      L" packetIn=${packetIn.format}",
      L" processedValue=0x${processedValue},",
      L" isValid=${isValid}",
      L")"
    ).flatten
  }
}
class MsgSeqTester() extends Component {
  val io = PacketFrame()

  io.processedValue := 0
  io.isValid := False

  val enable = io.controlSignal
  val length = io.packetIn.packetLength
  val pType = io.packetIn.packetType
  val dataValue = io.packetIn.payload.value
  val dataChecksum = io.packetIn.payload.checksum

  when(enable && pType === 1) {
    val calculatedChecksum = dataValue(7 downto 0)
    when(dataChecksum === calculatedChecksum) {
      io.processedValue := dataValue
      io.isValid := True
    } otherwise {
      io.processedValue := 0
      io.isValid := False
    }

    // Test print nested Seq[Any]
    report(io.format.toSeq)
  } otherwise {
    io.processedValue := 0
    io.isValid := False
  }

  // Test print scala primitive types
  report(L"${null}") // null
  report(L"${Byte.MinValue}")
  report(L"${Short.MaxValue}")
  report(L"${123}") // Int
  report(L"${123L}") // Long
  report(L"${3.14f}") // Float
  report(L"${3.1415926535}") // Double
  report(L"${'A'}") // Char
  report(L"${true}") // Boolean
  report(L"${BigInt(0x12)}")
  report(L"${BigDecimal("123.456")}")
}

class MsgSeqTesterSim extends SpinalSimFunSuite {

  test("MsgSeqTester simulation with three-level nested bundles") {
    SimConfig
      .doSim(new MsgSeqTester()) { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        dut.io.controlSignal #= false
        dut.io.packetIn.packetLength #= 0
        dut.io.packetIn.packetType #= 0
        dut.io.packetIn.payload.value #= 0
        dut.io.packetIn.payload.checksum #= 0

        dut.clockDomain.waitSampling()

        // Initial State Check
        assert(!dut.io.isValid.toBoolean, "Initially, isValid should be false")
        assert(dut.io.processedValue.toBigInt == 0, "Initially, processedValue should be 0")

        // Test Case: Control Signal Disabled
        dut.io.controlSignal #= false
        dut.io.packetIn.packetLength #= 10
        dut.io.packetIn.packetType #= 1
        dut.io.packetIn.payload.value #= 0xabcd
        dut.io.packetIn.payload.checksum #= 0xcd

        dut.clockDomain.waitSampling()
        assert(!dut.io.isValid.toBoolean, "isValid should be false when controlSignal is disabled")
        assert(dut.io.processedValue.toBigInt == 0, "processedValue should be 0 when controlSignal is disabled")

        // Test Case: Control Enabled, Type Mismatch
        dut.io.controlSignal #= true
        dut.io.packetIn.packetLength #= 10
        dut.io.packetIn.packetType #= 2 // Mismatch (expected 1)
        dut.io.packetIn.payload.value #= 0x1234
        dut.io.packetIn.payload.checksum #= 0x34

        dut.clockDomain.waitSampling()
        assert(!dut.io.isValid.toBoolean, "isValid should be false when packetType mismatches")
        assert(dut.io.processedValue.toBigInt == 0, "processedValue should be 0 when packetType mismatches")

        // Test Case: Valid Packet
        val validValue = 0x5678
        val validChecksum = 0x78
        dut.io.controlSignal #= true
        dut.io.packetIn.packetLength #= 12
        dut.io.packetIn.packetType #= 1 // Match
        dut.io.packetIn.payload.value #= validValue
        dut.io.packetIn.payload.checksum #= validChecksum

        dut.clockDomain.waitSampling()
        assert(dut.io.isValid.toBoolean, "isValid should be true for a valid packet")
        assert(
          dut.io.processedValue.toBigInt == validValue,
          "processedValue should match input value for a valid packet"
        )

        // Test Case: Checksum Mismatch
        val invalidValue = 0xabcd
        val wrongChecksum = 0xcc
        dut.io.controlSignal #= true
        dut.io.packetIn.packetLength #= 10
        dut.io.packetIn.packetType #= 1 // Match
        dut.io.packetIn.payload.value #= invalidValue
        dut.io.packetIn.payload.checksum #= wrongChecksum

        dut.clockDomain.waitSampling()
        assert(!dut.io.isValid.toBoolean, "isValid should be false when checksum mismatches")
        assert(dut.io.processedValue.toBigInt == 0, "processedValue should be 0 when checksum mismatches")
      }
  }
}
