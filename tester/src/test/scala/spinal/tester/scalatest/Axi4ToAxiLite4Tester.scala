package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba4.axi._
import spinal.lib._
import spinal.lib.bus.amba4.axi.sim.{Axi4ReadOnlyMasterAgent, Axi4ReadOnlyMonitor, Axi4WriteOnlyMasterAgent, Axi4WriteOnlyMonitor}
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.amba4.axilite.AxiLite4Utils.Axi4Rich
import spinal.lib.bus.amba4.axilite.sim.{AxiLite4ReadOnlyMonitor, AxiLite4ReadOnlySlaveAgent, AxiLite4WriteOnlyMonitor, AxiLite4WriteOnlySlaveAgent}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.sim.ScoreboardInOrder

import scala.collection.mutable
import scala.util.Random

class Axi4ToAxiLite4TesterComp(config: Axi4Config) extends Component {
  val io = new Bundle {
    val axi = slave(Axi4(config))
    val axilite = master(AxiLite4(AxiLite4Config(addressWidth = config.addressWidth, dataWidth = config.dataWidth)))
  }

  io.axi.toLite() >> io.axilite
}

class Axi4ToAxiLite4Tester extends AnyFunSuite {

  def tester(dut: Axi4ToAxiLite4TesterComp): Unit = {
    dut.clockDomain.forkStimulus(10)

    val writeScoreboard = ScoreboardInOrder[(BigInt, Byte)]()
    var writeRespRolling: Byte = 0
    val writeRespScoreboard = ScoreboardInOrder[Byte]()
    val readScoreboard = ScoreboardInOrder[(BigInt, Byte)]()
    val readRespScoreboard = ScoreboardInOrder[Byte]()

    def unburstTxnByteValid(addr: BigInt, size: Int, len: Int, burst: Int) = {
      val bytePerBeatLite = dut.io.axilite.config.bytePerWord
      val bytePerBeat = (1 << size)
      val bytes = (len + 1) * bytePerBeat

      for (beat <- 0 to len) yield {
        val beatAddress: BigInt = burst match {
          case 0 => addr
          case 1 => (addr + bytePerBeat * beat)
          case 2 => {
            val base = addr & ~BigInt(bytes - 1)
            (base + ((addr + bytePerBeat * beat) & BigInt(bytes - 1)))
          }
        }

        val accessAddress = beatAddress & ~BigInt(bytePerBeatLite - 1)

        val start = ((beatAddress & ~BigInt(bytePerBeat - 1)) - accessAddress).toInt
        val end = start + bytePerBeat

        for (i <- 0 until bytePerBeatLite) yield i >= start && i < end
      }
    }.flatten

    val writeUnburstBlanks = mutable.Queue[Boolean]()
    val readUnburstBlanks = mutable.Queue[Boolean]()
    val writeBytes = mutable.Queue[(BigInt, Byte, Boolean)]()
    val readBytes = mutable.Queue[(BigInt, Byte)]()

    def update(): Unit = {
      while(writeUnburstBlanks.nonEmpty && writeBytes.nonEmpty) {
        val (addr, data, strobe) = writeBytes.dequeue()
        val narrowBlank = writeUnburstBlanks.dequeue()
        if (narrowBlank && strobe) {
          writeScoreboard.pushDut((addr, data))
        }
      }

      while (readUnburstBlanks.nonEmpty && readBytes.nonEmpty) {
        val (addr, data) = readBytes.dequeue()
        val narrowBlank = readUnburstBlanks.dequeue()
        if (narrowBlank) {
          readScoreboard.pushDut((addr, data))
        }
      }
    }

    new Axi4WriteOnlyMasterAgent(dut.io.axi.aw, dut.io.axi.w, dut.io.axi.b, dut.clockDomain) {
      override def genAddress(): BigInt = ((Random.nextInt(1 << 19)))// & 0xFFF00) | 6

      override def mappingAllocate(mapping: SizeMapping): Boolean = true

      override def mappingFree(mapping: SizeMapping): Unit = {}

      override val assertOkResp: Boolean = false
    }

    new Axi4ReadOnlyMasterAgent(dut.io.axi.ar, dut.io.axi.r, dut.clockDomain) {
      override def genAddress(): BigInt = ((Random.nextInt(1 << 19)))// & 0xFFF00) | 6

      override def mappingAllocate(mapping: SizeMapping): Boolean = true

      override def mappingFree(mapping: SizeMapping): Unit = {}

      override val assertOkResp: Boolean = false
    }

    new AxiLite4WriteOnlyMonitor(dut.io.axilite.aw, dut.io.axilite.w, dut.io.axilite.b, dut.clockDomain) {
      override def onWriteStart(addr: BigInt): Unit = {}

      override def onWriteByteAlways(addr: BigInt, data: Byte, strobe: Boolean): Unit = {
        writeBytes += ((addr, data, strobe))
      }

      override def onWriteByte(addr: BigInt, data: Byte): Unit = {}

      override def onResponse(addr: BigInt, resp: Byte): Unit = {
        if (writeRespRolling == 0 && resp != 0) {
          writeRespRolling = resp
        }
      }
    }

    new AxiLite4ReadOnlyMonitor(dut.io.axilite.ar, dut.io.axilite.r, dut.clockDomain) {
      override def onReadStart(addr: BigInt): Unit = {}

      override def onReadByte(addr: BigInt, data: Byte): Unit = {
        readBytes += ((addr, data))
      }

      override def onResponse(addr: BigInt, resp: Byte): Unit = {
        readRespScoreboard.pushRef(resp)
      }
    }

    new Axi4WriteOnlyMonitor(dut.io.axi.aw, dut.io.axi.w, dut.io.axi.b, dut.clockDomain) {

      var activeWriteId: Int = -1

      override def onWriteStart(address: BigInt, id: Int, size: Int, len: Int, burst: Int): Unit = {
        assert(activeWriteId == -1)
        activeWriteId = id
        writeUnburstBlanks.clear()
        writeUnburstBlanks ++= unburstTxnByteValid(address, size, len, burst)
      }

      override def onWriteByte(address: BigInt, data: Byte, id: Int): Unit = {
        writeScoreboard.pushRef(address, data)
      }

      override def onResponse(id: Int, resp: Byte): Unit = {
        assert(id == activeWriteId)
        writeRespScoreboard.pushDut(resp)
        writeRespScoreboard.pushRef(writeRespRolling)
        activeWriteId = -1
        writeRespRolling = 0
      }
    }

    new Axi4ReadOnlyMonitor(dut.io.axi.ar, dut.io.axi.r, dut.clockDomain) {

      var activeReadId: Int = -1
      override val assertOkResp: Boolean = false

      override def onReadStart(address: BigInt, id: Int, size: Int, len: Int, burst: Int): Unit = {
        assert(activeReadId == -1)
        activeReadId = id
        readUnburstBlanks.clear()
        readUnburstBlanks ++= unburstTxnByteValid(address, size, len, burst)
      }

      override def onReadByte(address: BigInt, data: Byte, id: Int): Unit = {
        assert(activeReadId == id)
        readScoreboard.pushRef(address, data)
      }

      override def onResponse(address: BigInt, id: Int, last: Boolean, resp: Byte): Unit = {
        assert(id == activeReadId)
        readRespScoreboard.pushDut(resp)
        if (last)
          activeReadId = -1
      }
    }


    new AxiLite4WriteOnlySlaveAgent(dut.io.axilite.aw, dut.io.axilite.w, dut.io.axilite.b, dut.clockDomain)
    new AxiLite4ReadOnlySlaveAgent(dut.io.axilite.ar, dut.io.axilite.r, dut.clockDomain)

    SimTimeout(100000000L)

    while(true) {
      dut.clockDomain.waitSampling()
      writeRespScoreboard.check()
      readRespScoreboard.check()

      if (writeRespScoreboard.matches >= 200 && readRespScoreboard.matches >= 200) {
        simSuccess()
      }
    }
  }

  test("no_burst") {
    SimConfig.compile(new Axi4ToAxiLite4TesterComp(
      Axi4Config(addressWidth = 32,
        dataWidth = 32, useId = true, idWidth = 4, useBurst = false, useLen = false, useSize = false)))
      .doSim{dut => tester(dut)}
  }

  test("unburst") {
    SimConfig.compile(new Axi4ToAxiLite4TesterComp(
      Axi4Config(addressWidth = 32,
        dataWidth = 32, useId = true, idWidth = 4)))
      .doSim{dut => tester(dut)}
  }
}
