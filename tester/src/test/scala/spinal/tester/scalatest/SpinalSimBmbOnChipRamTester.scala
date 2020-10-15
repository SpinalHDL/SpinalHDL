package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core.sim._
import spinal.lib.sim._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbOnChipRam, BmbParameter}
import spinal.lib.bus.bmb.sim.{BmbMasterAgent, BmbMemoryAgent, BmbMemoryTester, BmbRegionAllocator}
import spinal.lib.bus.misc.SizeMapping

import scala.util.Random




class SpinalSimBmbOnChipRamTester extends SpinalSimFunSuite {
  test("test1") {
    val memInit = new Array[Byte](64 * 1024)
    Random.nextBytes(memInit)

    SimConfig.compile {
      val dut = BmbOnChipRam(
        p = BmbOnChipRam.busCapabilities(size = 64 KiB, dataWidth = 32).copy(
          sourceWidthMax = 4,
          contextWidthMax = 4
        ).toBmbParameter,
        size = 64 KiB
      )
      //Initialize the ram with memInit
      dut.rework(dut.ram.initBigInt(Seq.tabulate(dut.size.toInt / 4)(i => BigInt(((memInit(i * 4 + 0).toLong & 0xFF) << 0) | ((memInit(i * 4 + 1).toLong & 0xFF) << 8) | ((memInit(i * 4 + 2).toLong & 0xFF) << 16) | ((memInit(i * 4 + 3).toLong & 0xFF) << 24)))))
      dut
    } doSimUntilVoid { dut =>
      new BmbMemoryTester(
        bmb = dut.io.bus,
        cd = dut.clockDomain,
        rspCounterTarget = (30000*durationFactor).toInt) {
        for (i <- 0 until memInit.length) memory.setByte(i, memInit(i))
      }
    }
  }
}

