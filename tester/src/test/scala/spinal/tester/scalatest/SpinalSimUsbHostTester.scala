
package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bmb.sim.{BmbDriver, BmbMemoryAgent}
import spinal.lib.bus.bmb.{BmbAccessParameter, BmbParameter}
import spinal.lib.com.usb._
import spinal.lib.com.usb.ohci._
import spinal.lib.com.usb.phy.{UsbHubLsFs, UsbLsFsPhy}
import spinal.lib.eda.bench.{AlteraStdTargets, Bench, Rtl, XilinxStdTargets}
import spinal.lib.sim._

import scala.collection.mutable
import scala.util.Random

class SpinalSimUsbHostTester extends FunSuite{

  /*test("host")*/{
    val p = UsbOhciParameter(
      noPowerSwitching = true,
      powerSwitchingMode = true,
      noOverCurrentProtection = true,
      powerOnToPowerGoodTime = 10,
      fsRatio = 4,
      dataWidth = 32,
      portsConfig = List.fill(4)(OhciPortParameter())
    )

    SimConfig.withFstWave.compile(new UsbOhciTbTop(p)).doSim(seed = 42){dut =>
      val utils = new TesterUtils(dut)
      import utils._

      dut.clockDomain.forkSimSpeedPrinter()
      dut.clockDomain.waitSampling(2)

      val malloc = MemoryRegionAllocator(0, 1 << 30)


      val hcca = HCCA(malloc)
      hcca.save(ram)
      ctrl.write(hcca.address, hcHCCA)

      ctrl.write(BLF | CLF, hcCommand)
      dut.clockDomain.waitSampling(100)
      ctrl.write(USB_OPERATIONAL | BLE | CLE | PLE | 0x3, hcControl)

      dut.clockDomain.waitSampling(100)

      val p0 = fork {
        devices(0).connect(lowSpeed = false)
        waitConnected(0)
        setPortReset(0)
        waitPortReset(0)
      }

      val p1 = fork {
        devices(1).connect(lowSpeed = true)
        waitConnected(1)
        setPortReset(1)
        waitPortReset(1)
      }
      p0.join()
      p1.join()

      val td0 = TD(malloc)
      val td0Buffer = malloc.allocateAligned(256) //TODO should not be aligned
      td0.DP = 0
      td0.DI = 5
      td0.currentBuffer = td0Buffer.base.toInt
      td0.bufferEnd = td0Buffer.base.toInt + 0x1BF
      td0.nextTD = 0
      td0.save(ram)

      for((address, i) <- (td0.currentBuffer to td0.bufferEnd).zipWithIndex){
        ram.write(address, i.toByte)
      }


      val ed0 = ED(malloc)
      ed0.D = 0
      ed0.FA = 42
      ed0.EN = 6
      ed0.MPS = 64
      ed0.headP = td0.address
      ed0.nextED = 0
      ed0.save(ram)
      ctrl.write(ed0.address, hcBulkHeadED)

      for((group, groupId) <- (0 until 448).grouped(ed0.MPS).zipWithIndex) {
        scoreboards(0).pushRef(TockenKey(ed0.FA, ed0.EN, TOCKEN_SETUP), DataPacket(if(groupId % 2 == 0) DATA0 else DATA1, group.map(byteId => ram.readByteAsInt(td0.currentBuffer + byteId)))){
          portAgents(0).emitBytes(HANDSHAKE_ACK, List(), false, true)
        }
      }

      setBulkListFilled()

      sleep(12e-3*1e12)

      scoreboards.foreach(_.assertEmpty)
    }
  }
}



object UsbHostSynthesisTest {
  def main(args: Array[String]) {
    val rawrrr = new Rtl {
      override def getName(): String = "UsbOhci"
      override def getRtlPath(): String = "UsbOhci.v"
      SpinalVerilog({
        val p = UsbOhciParameter(
          noPowerSwitching = false,
          powerSwitchingMode = false,
          noOverCurrentProtection = false,
          powerOnToPowerGoodTime = 10,
          fsRatio = 4,
          dataWidth = 32,
          portsConfig = List.fill(1)(OhciPortParameter())
        )
        UsbOhci(p, BmbParameter(
          addressWidth = 12,
          dataWidth = 32,
          sourceWidth = 0,
          contextWidth = 0,
          lengthWidth = 2
        ))
      })
    }


    val rtls = List(rawrrr)

    val targets = XilinxStdTargets().take(1)

    Bench(rtls, targets)
  }
}
