
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
import scala.collection.mutable.ArrayBuffer
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

      val m = memory.memory

      dut.clockDomain.forkSimSpeedPrinter()
      dut.clockDomain.waitSampling(2)


      val hcca = HCCA(malloc)
      hcca.save(ram)
      ctrl.write(hcca.address, hcHCCA)

      ctrl.write(BLF | CLF, hcCommand)
      dut.clockDomain.waitSampling(100)
      ctrl.write(USB_OPERATIONAL | BLE | CLE | PLE | 0x3, hcControl)

      dut.clockDomain.waitSampling(100)

      val doneChecks = mutable.HashMap[Int, TD => Unit]()
      //Interrupt handler
      fork{
        ctrl.write(UsbOhci.MasterInterruptEnable | UsbOhci.WritebackDoneHead, UsbOhci.HcInterruptEnable)
        while(true) {
          dut.clockDomain.waitSamplingWhere(dut.irq.toBoolean)
          val flags = ctrl.read(UsbOhci.HcInterruptStatus)
          ctrl.write(0xFFFFFFFFl, UsbOhci.HcInterruptStatus)
          if((flags & UsbOhci.WritebackDoneHead) != 0){
            var ptr = memory.memory.readInt(hcca.address + 0x84) & ~1
            assert(ptr != 0)
            while(ptr != 0){
              val td = TD(ptr).load(memory.memory)
              doneChecks.remove(ptr).get.apply(td)
              ptr = td.nextTD
            }
          }
        }
      }

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

      def deviceDelayed(ls : Boolean)(body : => Unit){
        delayed(4*83333*(if(ls) 8 else 1)) {body} //TODO improve
      }



//      var fmNumberOld = ctrl.read(UsbOhci.HcFmNumber)
//      fork{
//        val fmNumber = ctrl.read(UsbOhci.HcFmNumber)
//        if(fmNumber == fmNumberOld + 1){
//
//        } else {
//          assert(fmNumber == fmNumberOld)
//        }
//      }

      def addTd(td : TD, ed: ED): Unit ={
        if(ed.tailP == ed.headP){
          ed.headP = td.address
        } else {
          val last = TD(ed.tailP).load(m)
          last.nextTD = td.address
        }
        ed.tailP = td.address
      }

      def initED(ed : ED): Unit ={
        val td = TD(malloc)
        ed.tailP = td.address
        ed.headP = td.address
      }

      def newTd(ed: ED) : TD = {
        ed.load(m)
        val td = TD(ed.tailP)
        val dummy = TD(malloc)
        ed.tailP = dummy.address
        td.nextTD = dummy.address
        ed.save(m)
        td
      }

      val bulksEd, controlsEd = ArrayBuffer[ED]()

      def addBulkEd(ed : ED): Unit = {
        if(bulksEd.isEmpty){
          ctrl.write(ed.address, hcBulkHeadED)
        } else {
          val edLast = bulksEd.last.load(m)
          edLast.nextED = ed.address
          edLast.save(m)
        }
        bulksEd += ed
      }

      val ed0 = ED(malloc)
      ed0.F = false
      ed0.D = 0
      ed0.FA = 42
      ed0.EN = 6
      ed0.MPS = 64
      initED(ed0)
      addBulkEd(ed0)
      ed0.save(ram)


      for(tdId <- 0 until 2) {
        val size = 448
        val td0Buffer = malloc.allocateAligned(size) //TODO should not be aligned
        val td0 = newTd(ed0)
        td0.DP = 0
        td0.DI = 5
        td0.T = 2
        td0.currentBuffer = td0Buffer.base.toInt
        td0.bufferEnd = td0Buffer.base.toInt + 0x1BF
        td0.CC == UsbOhci.ConditionCode.notAccessed
        td0.save(ram)
        for ((address, i) <- (td0.currentBuffer to td0.bufferEnd).zipWithIndex) {
          ram.write(address, i.toByte)
        }
        val groups = (0 until 448).grouped(ed0.MPS).zipWithIndex.toSeq
        for ((group, groupId) <- groups) {
          scoreboards(0).pushHcToUsb(TockenKey(ed0.FA, ed0.EN), DataPacket(if (groupId % 2 == 0) DATA0 else DATA1, group.map(byteId => ram.readByteAsInt(td0.currentBuffer + byteId)))) {
            portAgents(0).emitBytes(HANDSHAKE_ACK, List(), false, true)
            if (group == groups.last._1) {
              doneChecks(td0.address) = { td =>
                assert(td.currentBuffer == 0)
                assert(td.CC == UsbOhci.ConditionCode.noError)
                malloc.free(td0Buffer)
                malloc.free(td0.address)
              }
            }
          }
        }
        setBulkListFilled()
      }




//      val size = 32
//      val td0Malloc = malloc.allocateAligned(0x10)
//      val td0Buffer = malloc.allocateAligned(32) //TODO should not be aligned
//      val td0 = TD(td0Malloc.base.toInt)
//      td0.DP = 2
//      td0.DI = 5
//      td0.currentBuffer = td0Buffer.base.toInt
//      td0.bufferEnd = td0Buffer.base.toInt + 0x1F
//      td0.nextTD = 0
//      td0.CC == UsbOhci.ConditionCode.notAccessed
//      td0.save(ram)
//
//      val ed0 = ED(malloc)
//      ed0.D = 0
//      ed0.FA = 42
//      ed0.EN = 6
//      ed0.MPS = 64
//      ed0.headP = td0.address
//      ed0.nextED = 0
//      ed0.save(ram)
//      ctrl.write(ed0.address, hcBulkHeadED)
//
//      val groups = (0 until 32).grouped(ed0.MPS).zipWithIndex.toSeq
//      for((group, groupId) <- groups) {
//        scoreboards(0).pushUsbToHc(TockenKey(ed0.FA, ed0.EN)){
//          deviceDelayed(ls=false) {
//            portAgents(0).emitBytes(HANDSHAKE_ACK, 0 until 32, true, false)
//            if(group == groups.last._1){
//              doneChecks(td0.address) = {td =>
//                assert(td.currentBuffer == 0)
//                assert(td.CC == UsbOhci.ConditionCode.noError)
//                for(i <- 0 until size) {
//                  val offset = td0.currentBuffer + i //TODO manage pages jump
//                  assert(memory.memory.read(offset) == i)
//                }
//                malloc.free(td0Malloc)
//                malloc.free(td0Buffer)
//                println("DONNNNE")
//              }
//            }
//          }
//          true
//        }
//      }



      sleep(12e-3*1e12)

      scoreboards.foreach(_.assertEmpty)
      assert(ctrl.read(UsbOhci.HcDoneHead) == 0)
      assert(doneChecks.isEmpty)
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
