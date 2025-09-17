package spinal.core

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.{master, slave}

class Apb3ConnectionTester extends AnyFunSuite {
  class APB3Stage extends Component {
    val input = slave(Apb3(12, 8))
    val output = master(Apb3(12, 8))

    input.m2sPipe() >> output
  }

  def readRequest(cd: ClockDomain, bus: Apb3, addr: Long): Long = {
    bus.PSEL #= 1
    bus.PENABLE #= false
    bus.PADDR #= addr
    bus.PWRITE #= false
    cd.waitSampling()
    do {
      bus.PENABLE #= true
      cd.waitSampling()
    } while (!bus.PREADY.toBoolean)
    bus.PSEL #= 0
    bus.PRDATA.toLong
  }

  def serveRead(cd: ClockDomain, bus: Apb3, addr: Long, reply: Long): Unit = {
    cd.waitSamplingWhere(bus.PSEL.toInt != 0)
    assert(!bus.PENABLE.toBoolean)
    bus.PRDATA #= reply
    bus.PREADY #= true

    cd.waitSampling()
    assert(bus.PENABLE.toBoolean && bus.PSEL.toInt != 0)
    assert(bus.PADDR.toLong == addr)
  }

  def doRead(dut: APB3Stage, addr: Long, reply: Long): Unit = {
    fork {
      serveRead(dut.clockDomain, dut.output, addr, reply)
    }
    assert(readRequest(dut.clockDomain, dut.input, addr) == reply)
  }

  test("apb3stage") {
    val compiled = SpinalSimConfig().withWave.compile(new APB3Stage)
    compiled.doSim("apb3stage") { dut =>
      val cd = dut.clockDomain
      cd.forkStimulus(10)
      dut.input.PSEL #= 0
      dut.output.PREADY #= false

      cd.waitSampling()
      doRead(dut, 0x100, 0xfe)

      cd.waitSampling(10)
      doRead(dut, 0x78, 0xff)
      dut.input.PENABLE #= false
      dut.output.PREADY #= true  // hold PREADY into next test case (this should be OK because all-hi PREADY is)

      cd.waitSampling(10)
      doRead(dut, 0x12, 0xac)
      dut.output.PREADY #= false  // do not hold PREADY high any more
      dut.output.PENABLE #= true  // but set PENABLE high (this should be valid outside of PSEL)

      cd.waitSampling(10)
      doRead(dut, 0x50, 0x13)
      dut.output.PREADY #= true   // now hold both signals high
      dut.output.PENABLE #= true  // this should also be allowed

      // The following two waits are a regression test. With both PREADY and PENABLE high
      // the output PENABLE starts oscillating at f/2. By trying both an even and an odd
      // waiting time we are guaranteed to hit it both phases once.
      cd.waitSampling(10)
      doRead(dut, 0x25, 0x26)
      dut.output.PREADY #= true   // now hold both signals high
      dut.output.PENABLE #= true  // this should also be allowed

      cd.waitSampling(11)
      doRead(dut, 0x30, 0x3f)
      dut.output.PREADY #= true   // now hold both signals high
      dut.output.PENABLE #= true  // this should also be allowed

      doRead(dut, 0x31, 0x41)
      doRead(dut, 0x32, 0x42)

      cd.waitSampling(10)

      simSuccess()
    }
  }
}
