
package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bmb.sim.{BmbDriver, BmbMemoryAgent}
import spinal.lib.bus.bmb.{BmbAccessParameter, BmbParameter}
import spinal.lib.com.usb._
import spinal.lib.com.usb.ohci._
import spinal.lib.com.usb.phy.UsbLsFs.TxKind
import spinal.lib.eda.bench.{AlteraStdTargets, Bench, Rtl, XilinxStdTargets}
import spinal.lib.sim._

import scala.collection.mutable
import scala.util.Random

class SpinalSimUsbHostTester extends FunSuite{

  val hcControl = 0x04
  val hcCommand = 0x08
  val hcHCCA = 0x18
  val hcPeriodicStart = 0x40
  val hcFmInterval = 0x34
  val hcControlHeadED = 0x20
  val hcBulkHeadED = 0x28
  val hcFmNumber = 0x3C

  val USB_OPERATIONAL = 2 << 6
  val BLE = 1 << 5
  val BLF = 1 << 2
  val CLF = 1 << 1
  val CLE = 1 << 4
  val IE = 1 << 3
  val PLE = 1 << 2

  implicit class BooleanPimper(self : Boolean){
    def toInt = if(self) 1 else 0
  }

  case class HCCA(address : Int){
    var interrupts = Array.fill(32)(0)
    var frameNumber = 0
    var doneHead = 0

    def save(m : SparseMemory): Unit ={
      for(i <- 0 until 32) m.write(address + i*4, interrupts(i))
      m.write(address + 0x80, frameNumber)
      m.write(address + 0x84, doneHead)
    }
  }

  case class ED(address : Int){
    var FA,EN,D,MPS, tailP, headP, nextED = 0
    var C,H,F,K,S = false

    def save(m : SparseMemory): Unit ={
      m.write(address + 0x00, (FA << 0) | (EN << 7) | (D << 11) | (S.toInt << 13) | (K.toInt << 14) | (F.toInt << 15) | (MPS << 16))
      m.write(address + 0x04, tailP)
      m.write(address + 0x08, headP | (C.toInt << 1) | (H.toInt << 0))
      m.write(address + 0x0C, nextED)
    }
  }

  case class TD(address : Int){
    var CC,EC,T,DI,DP, currentBuffer, nextTD, bufferEnd = 0
    var R = false

    def save(m : SparseMemory): Unit ={
      m.write(address + 0x00, (CC << 28) | (EC << 26) | (T << 24) | (DI << 21) | (DP << 19) | (R.toInt << 18))
      m.write(address + 0x04, currentBuffer)
      m.write(address + 0x08, nextTD)
      m.write(address + 0x0C, bufferEnd)
    }
  }

  test("host"){
    val p = UsbOhciParameter(
      downstreamPorts = 1,
      noPowerSwitching = true,
      powerSwitchingMode = true,
      noOverCurrentProtection = true,
      overCurrentProtectionMode = true,
      powerOnToPowerGoodTime = 10,
      localPowerStatus = true,
      fsRatio = 4,
      dataWidth = 32
    )

    SimConfig.withFstWave.compile(
      UsbOhci(p, BmbParameter(
        addressWidth = 12,
        dataWidth = 32,
        sourceWidth = 0,
        contextWidth = 0,
        lengthWidth = 2
      ))
    ).doSim(seed = 42){dut =>
      dut.clockDomain.forkStimulus(20800)

      var txBusy = 0
      dut.clockDomain.onSamplings{
        if(txBusy == 0) {
          dut.io.phy.tx.ready #= false
          if (dut.io.phy.tx.kind.toEnum != TxKind.NONE) {
            txBusy = 1
          }
        } else {
          txBusy = txBusy + 1
          if(txBusy == 4*8){
            txBusy = 0
            dut.io.phy.tx.ready #= true
          }
        }
      }

      val memory = new BmbMemoryAgent()
      memory.addPort(dut.io.dma, 0, dut.clockDomain, true)
      def ram = memory.memory

      val ctrl = BmbDriver(dut.io.ctrl, dut.clockDomain)
      dut.clockDomain.waitSampling(10)



      val interval = 12000
      val intervalWithOverhead = (((interval - 210) * 6) / 7)
      ctrl.write((interval-1) | intervalWithOverhead << 16, hcFmInterval)
      ctrl.write(interval*9/10, hcPeriodicStart)
//      ctrl.write(0xFFFF, hcFmNumber)

      val hcca = HCCA(0x1000)
      hcca.save(ram)
      ctrl.write(hcca.address, hcHCCA)

      val td3 = TD(0x7000)
      td3.DP = 0
      td3.DI = 4
      td3.currentBuffer = 0x120000+8
      td3.bufferEnd = 0x1201FF+8
      td3.nextTD = 0
      td3.save(ram)

      for((address, i) <- (td3.currentBuffer to td3.bufferEnd).zipWithIndex){
        ram.write(address, i.toByte)
      }


      val ed2 = ED(0x8000)
      ed2.D = 0
      ed2.MPS = 64
      ed2.headP = td3.address
      ed2.save(ram)
      ctrl.write(ed2.address, hcControlHeadED)



      val td2 = TD(0x5000)
      td2.DP = 0
      td2.DI = 7
      td2.currentBuffer = 0x100000+8
      td2.bufferEnd = 0x1000FF+8
      td2.nextTD = 0
      td2.save(ram)

      for((address, i) <- (td2.currentBuffer to td2.bufferEnd).zipWithIndex){
        ram.write(address, i.toByte)
      }


      val ed1 = ED(0x6000)
      ed1.D = 0
      ed1.MPS = 64
      ed1.headP = td2.address
      ed1.save(ram)




      val td1 = TD(0x4000)
      td1.DP = 0
      td1.DI = 6
      td1.currentBuffer = 0x110000+8
      td1.bufferEnd = 0x1100FF+8
      td1.save(ram)

      for((address, i) <- (td1.currentBuffer to td1.bufferEnd).reverse.zipWithIndex){
        ram.write(address, i.toByte)
      }

      val td0 = TD(0x3000)
      td0.DP = 0
      td0.DI = 5
      td0.currentBuffer = 0x100000+8
      td0.bufferEnd = 0x1001BF+8
      td0.nextTD = td1.address
      td0.save(ram)

      for((address, i) <- (td0.currentBuffer to td0.bufferEnd).zipWithIndex){
        ram.write(address, i.toByte)
      }


      val ed0 = ED(0x2000)
      ed0.D = 0
      ed0.MPS = 64
      ed0.headP = td0.address
      ed0.nextED = ed1.address
      ed0.save(ram)
      ctrl.write(ed0.address, hcBulkHeadED)






      ctrl.write(BLF | CLF, hcCommand)
      dut.clockDomain.waitSampling(100)
      ctrl.write(USB_OPERATIONAL | BLE  | CLE | PLE | 0x3, hcControl)

      dut.clockDomain.waitSampling(100)
      sleep(12e-3*1e12)
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
          downstreamPorts = 1,
          noPowerSwitching = true,
          powerSwitchingMode = true,
          noOverCurrentProtection = true,
          overCurrentProtectionMode = true,
          powerOnToPowerGoodTime = 10,
          localPowerStatus = true,
          fsRatio = 4,
          dataWidth = 32
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
