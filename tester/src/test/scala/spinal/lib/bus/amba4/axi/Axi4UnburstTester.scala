package spinal.lib.bus.amba4.axi

import spinal.core.sim._
import sim.{Axi4ReadOnlyMasterAgent, Axi4ReadOnlyMonitor, Axi4ReadOnlySlaveAgent, Axi4WriteOnlyMasterAgent, Axi4WriteOnlyMonitor, Axi4WriteOnlySlaveAgent}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.sim.ScoreboardInOrder
import spinal.tester.SpinalAnyFunSuite

import scala.util.Random

class Axi4UnburstTester extends SpinalAnyFunSuite {

  def tester_readonly(dut: Axi4ReadOnlyUnburster): Unit = {
    dut.clockDomain.forkStimulus(10)

    val readScoreboard = ScoreboardInOrder[(BigInt, Byte, Int)]()
    val readRespScoreboard = ScoreboardInOrder[Byte]()

    new Axi4ReadOnlyMasterAgent(dut.io.input.ar, dut.io.input.r, dut.clockDomain) {
      override def genAddress(): BigInt = ((Random.nextInt(1 << 19)))// & 0xFFF00) | 6

      override def mappingAllocate(mapping: SizeMapping): Boolean = true

      override def mappingFree(mapping: SizeMapping): Unit = {}

      override val assertOkResp: Boolean = false
    }

    new Axi4ReadOnlyMonitor(dut.io.output.ar, dut.io.output.r, dut.clockDomain, withReadInterleaveInBurst = false) {
      override def onReadByte(address: BigInt, data: Byte, id: Int): Unit = {
        readScoreboard.pushDut(address, data, id)
      }

      override def onResponse(address: BigInt, id: Int, last: Boolean, resp: Byte): Unit = {
        readRespScoreboard.pushRef(resp)
      }
    }

    new Axi4ReadOnlyMonitor(dut.io.input.ar, dut.io.input.r, dut.clockDomain, withReadInterleaveInBurst = false) {
      override def onReadByte(address: BigInt, data: Byte, id: Int): Unit = {
        readScoreboard.pushRef(address, data, id)
      }

      override def onResponse(address: BigInt, id: Int, last: Boolean, resp: Byte): Unit = {
        readRespScoreboard.pushDut(resp)
      }
    }

    new Axi4ReadOnlySlaveAgent(dut.io.output.ar, dut.io.output.r, dut.clockDomain, withReadInterleaveInBurst = false, withArReordering = false)

    SimTimeout(100000000L)

    while(true) {
      dut.clockDomain.waitSampling()
      readRespScoreboard.check()
      readScoreboard.check()

      if (readRespScoreboard.matches >= 256 && readScoreboard.matches >= 1024) {
        simSuccess()
      }
    }
  }

  test("unburst_readonly_no_last_no_id") {
    SimConfig.compile(new Axi4ReadOnlyUnburster(Axi4Config(dataWidth = 32, addressWidth = 32, useId = false, useLast = false)))
      .doSim { dut => tester_readonly(dut) }
  }

  test("unburst_readonly_no_id") {
    SimConfig.compile(new Axi4ReadOnlyUnburster(Axi4Config(dataWidth = 32, addressWidth = 32, useId = false)))
      .doSim { dut => tester_readonly(dut) }
  }

  test("unburst_readonly_no_last") {
    SimConfig.compile(new Axi4ReadOnlyUnburster(Axi4Config(dataWidth = 32, addressWidth = 32, idWidth = 4, useLast = false)))
      .doSim { dut => tester_readonly(dut) }
  }

  test("unburst_readonly") {
    SimConfig.withFstWave.compile(new Axi4ReadOnlyUnburster(Axi4Config(dataWidth = 32, addressWidth = 32, idWidth = 4)))
      .doSim(seed = 1) {dut => tester_readonly(dut) }
  }

  def tester_writeonly(dut: Axi4WriteOnlyUnburster): Unit = {
    dut.clockDomain.forkStimulus(10)

    val writeScoreboard = ScoreboardInOrder[(BigInt, Byte)]()
    var writeRespRolling: Byte = 0
    val writeRespScoreboard = ScoreboardInOrder[Byte]()

    new Axi4WriteOnlyMasterAgent(dut.io.input.aw, dut.io.input.w, dut.io.input.b, dut.clockDomain) {
      override def genAddress(): BigInt = ((Random.nextInt(1 << 19))) // & 0xFFF00) | 6

      override def mappingAllocate(mapping: SizeMapping): Boolean = true

      override def mappingFree(mapping: SizeMapping): Unit = {}

      override val assertOkResp: Boolean = false
    }

    new Axi4WriteOnlyMonitor(dut.io.output.aw, dut.io.output.w, dut.io.output.b, dut.clockDomain) {
      override def onWriteByte(address: BigInt, data: Byte, id: Int): Unit = {
        writeScoreboard.pushDut(address, data)
      }

      override def onResponse(id: Int, resp: Byte): Unit = {
        if (writeRespRolling == 0 && resp != 0) {
          writeRespRolling = resp
        }
      }
    }

    new Axi4WriteOnlyMonitor(dut.io.input.aw, dut.io.input.w, dut.io.input.b, dut.clockDomain) {
      override def onWriteByte(address: BigInt, data: Byte, id: Int): Unit = {
        writeScoreboard.pushRef(address, data)
      }

      override def onResponse(id: Int, resp: Byte): Unit = {
        writeRespScoreboard.pushDut(resp)
        writeRespScoreboard.pushRef(writeRespRolling)
        writeRespRolling = 0
      }
    }

    new Axi4WriteOnlySlaveAgent(dut.io.output.aw, dut.io.output.w, dut.io.output.b, dut.clockDomain)

    SimTimeout(100000000L)

    while (true) {
      dut.clockDomain.waitSampling()
      writeScoreboard.check()
      writeRespScoreboard.check()

      if (writeRespScoreboard.matches >= 256 && writeScoreboard.matches >= 1024) {
        simSuccess()
      }
    }
  }

  test("unburst_writeonly_no_last_no_id") {
    SimConfig.compile(new Axi4WriteOnlyUnburster(Axi4Config(dataWidth = 32, addressWidth = 32, useId = false, useLast = false)))
      .doSim { dut => tester_writeonly(dut) }
  }

  test("unburst_writeonly_no_id") {
    SimConfig.compile(new Axi4WriteOnlyUnburster(Axi4Config(dataWidth = 32, addressWidth = 32, useId = false)))
      .doSim { dut => tester_writeonly(dut) }
  }

  test("unburst_writeonly_no_last") {
    SimConfig.compile(new Axi4WriteOnlyUnburster(Axi4Config(dataWidth = 32, addressWidth = 32, idWidth = 4, useLast = false)))
      .doSim { dut => tester_writeonly(dut) }
  }

  test("unburst_writeonly") {
    SimConfig.compile(new Axi4WriteOnlyUnburster(Axi4Config(dataWidth = 32, addressWidth = 32, idWidth = 4)))
      .doSim { dut => tester_writeonly(dut) }
  }
}
