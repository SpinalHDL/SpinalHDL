
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
      val td0Buffer = malloc.allocateAligned(256)
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
        scoreboards(0).pushRef(TockenKey(ed0.FA, ed0.EN, TOCKEN_SETUP), DataPacket(if(groupId % 2 == 0) DATA0 else DATA1, group.map(byteId => ram.readByteAsInt(td0.currentBuffer + byteId))))
      }
      //TODO missing end of packet ?

      setBulkListFilled()

      sleep(12e-3*1e12)

      scoreboards.foreach(_.assertEmpty)
    }
  }
}


//      val td3 = TD(0x7000)
//      td3.DP = 0
//      td3.DI = 4
//      td3.currentBuffer = 0x120000+8
//      td3.bufferEnd = 0x1201FF+8
//      td3.nextTD = 0
//      td3.save(ram)
//
//      for((address, i) <- (td3.currentBuffer to td3.bufferEnd).zipWithIndex){
//        ram.write(address, i.toByte)
//      }
//
//
//      val ed2 = ED(0x8000)
//      ed2.D = 0
//      ed2.MPS = 64
//      ed2.headP = td3.address
//      ed2.save(ram)
//      ctrl.write(ed2.address, hcControlHeadED)
//
//
//
//      val td2 = TD(0x5000)
//      td2.DP = 0
//      td2.DI = 7
//      td2.currentBuffer = 0x100000+8
//      td2.bufferEnd = 0x1000FF+8
//      td2.nextTD = 0
//      td2.save(ram)
//
//      for((address, i) <- (td2.currentBuffer to td2.bufferEnd).zipWithIndex){
//        ram.write(address, i.toByte)
//      }
//
//
//      val ed1 = ED(0x6000)
//      ed1.D = 0
//      ed1.MPS = 64
//      ed1.headP = td2.address
//      ed1.save(ram)
//
//
//
//
//      val td1 = TD(0x4000)
//      td1.DP = 0
//      td1.DI = 6
//      td1.currentBuffer = 0x110000+8
//      td1.bufferEnd = 0x1100FF+8
//      td1.save(ram)
//
//      for((address, i) <- (td1.currentBuffer to td1.bufferEnd).reverse.zipWithIndex){
//        ram.write(address, i.toByte)
//      }
//
//      val td0 = TD(0x3000)
//      td0.DP = 0
//      td0.DI = 5
//      td0.currentBuffer = 0x100000+8
//      td0.bufferEnd = 0x1001BF+8
//      td0.nextTD = td1.address
//      td0.save(ram)
//
//      for((address, i) <- (td0.currentBuffer to td0.bufferEnd).zipWithIndex){
//        ram.write(address, i.toByte)
//      }
//
//
//      val ed0 = ED(0x2000)
//      ed0.D = 0
//      ed0.MPS = 64
//      ed0.headP = td0.address
//      ed0.nextED = ed1.address
//      ed0.save(ram)
//      ctrl.write(ed0.address, hcBulkHeadED)
//
//
//
//
//
//
//      ctrl.write(BLF | CLF, hcCommand)
//      dut.clockDomain.waitSampling(100)
////      ctrl.write(USB_OPERATIONAL, hcControl)
//      ctrl.write(USB_OPERATIONAL | BLE | CLE | PLE | 0x3, hcControl)
//
//      dut.clockDomain.waitSampling(100)
//      sleep(12e-3*1e12)








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
