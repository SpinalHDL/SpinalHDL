
package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bmb.sim.{BmbDriver, BmbMemoryAgent}
import spinal.lib.bus.bmb.{BmbAccessParameter, BmbParameter, BmbSourceParameter}
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

      var activity = true

      dut.clockDomain.forkSimSpeedPrinter()
      dut.clockDomain.waitSampling(2)
      forkSimSporadicWave(
        captures = Seq(
          3e-3 -> 9e-3
//          140e-3 -> 50e-3
        )
      )


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
      def edTargetConcflicting(ed : ED) = (bulksEd ++ controlsEd).exists(e => e.FA == ed.FA && e.EN == ed.EN)

      def addBulkEd(ed : ED): Unit = {
        if(bulksEd.isEmpty){
          bulksEd += ed
          ctrl.write(ed.address, hcBulkHeadED)
        } else {
          val edLast = bulksEd.last.load(m)
          edLast.nextED = ed.address
          edLast.save(m)
          bulksEd += ed
        }
      }
      def addControlEd(ed : ED): Unit = {
        if(controlsEd.isEmpty){
          controlsEd += ed
          ctrl.write(ed.address, hcControlHeadED)
        } else {
          val edLast = controlsEd.last.load(m)
          edLast.nextED = ed.address
          edLast.save(m)
          controlsEd += ed
        }
      }


      var totalBytes = 0
      val edCount = 40 //XXX
      for(edId <- 0 until edCount) {
        val CONTROL = 0
        val BULK = 1
        var edKind = List(CONTROL, BULK).randomPick()
//        edKind = BULK //XXX
        val ed0 = ED(malloc)
        do {
          ed0.F = false
          ed0.D = 0
          ed0.FA = Random.nextInt(128)
          ed0.EN = Random.nextInt(16)
          ed0.MPS = 64
        } while(edTargetConcflicting(ed0))
        initED(ed0)
        edKind match {
          case BULK => addBulkEd(ed0)
          case CONTROL => addControlEd(ed0)
        }
        ed0.save(ram)


        var dataPhase = 0

        def dataPhaseIncrement() = {
          dataPhase += 1
          dataPhase %= 2
        }

        def listRefilled(): Unit = edKind match {
          case BULK => setBulkListFilled()
          case CONTROL => setControlListFilled()
        }
        def unhaltEndpoint(ed: ED): Unit = fork{
          sleep(Random.nextInt(5)*1e9)
          ed.H = false
          ed.save(m)
          listRefilled()
        }

        fork {
          for (tdId <- 0 until 10) { //XXX
            var size = if (Random.nextDouble() < 0.1) {
              Random.nextInt(8192 + 1)
            } else if (Random.nextDouble() < 0.05) {
              0
            } else if (Random.nextDouble() < 0.05) {
              8192
            } else {
              Random.nextInt(256 + 1)
            }
            //        size = 256 //XXX

            var p0, p1, p0Offset = 0
            var p0Used, p1Used = 0
            var success = false
            while (!success) {
              success = true
              p0 = Random.nextInt(128 * 1024) * 4096
              p1 = Random.nextInt(128 * 1024) * 4096
              p0Offset = Random.nextInt((8192 - size + 1).min(4096))
              //          p0Offset = 0 //XXX
              p0Used = (4096 - p0Offset).min(size)
              p1Used = size - p0Used
              if (malloc.isAllocated(p0 + p0Offset, p0Used)) success = false
              if (p1Used != 0 && malloc.isAllocated(p1, p1Used)) success = false
            }
            totalBytes += size

            def byteToAddress(i: Int) = if (i < p0Used) p0 + p0Offset + i else p1 + i - p0Used

            if (p0Used != 0) malloc.allocateOn(p0 + p0Offset, p0Used)
            if (p1Used != 0) malloc.allocateOn(p1, p1Used)

            val td0 = newTd(ed0)

            td0.DP = Random.nextInt(3)
            //        td0.DP = UsbOhci.DP.IN //XXX
            td0.DI = 5
            td0.T = 0
            if (Random.nextDouble() < 0.5) {
              dataPhase = Random.nextInt(2)
              td0.T = 2 | dataPhase
            }
            td0.R = Random.nextBoolean()
            td0.currentBuffer = if (size == 0) 0 else p0 + p0Offset
            td0.bufferEnd = if (p1Used != 0) p1 + p1Used - 1 else p0 + p0Offset + p0Used - 1
            td0.CC == UsbOhci.CC.notAccessed
            td0.save(ram)


            def tdCompletion() {
              td0.load(m)
              if (td0.CC != UsbOhci.CC.noError) {
                assert(ed0.H)
                unhaltEndpoint(ed0)
              }

              if (p0Used != 0) malloc.free(p0 + p0Offset)
              if (p1Used != 0) malloc.free(p1)
              malloc.free(td0.address)
              println("DONNNNE")
            }

            var doOverflow = td0.DP == UsbOhci.DP.IN && Random.nextDouble() < 0.05
            var doUnderflow = td0.DP == UsbOhci.DP.IN && !doOverflow && size != 0 && Random.nextDouble() < 0.05
            var doDataToggleMismatch = td0.DP == UsbOhci.DP.IN && Random.nextDouble() < 0.05
            var doStall = Random.nextDouble() < 0.05
            var doTransmissionError = Random.nextDouble() < 0.05

            //        doOverflow = false //XXX
            //        doUnderflow = false //XXX
            //        doStall = false //XXX
            //        doTransmissionError = true //XXX
            //        doDataToggleMismatch = false //XXX

            //        val refData = (0 until size) //XXX
            val refData = Array.fill(size)(Random.nextInt(256))
            if (td0.DP != UsbOhci.DP.IN) for (i <- 0 until size) {
              val address = byteToAddress(i)
              ram.write(address, refData(i).toByte)
            }

            var groups: Seq[Seq[Int]] = (0 until size).grouped(ed0.MPS).toSeq
            if (groups.isEmpty) groups = List(Nil)
            val disruptAt = Random.nextInt(groups.size)

            var errorCounter = 0
            var groupIdCounter = 0
            var continue = true


            while (continue && groupIdCounter < groups.size) {
              val groupId = groupIdCounter
              val group = groups(groupId)
              val groupLast = groupId == groups.size - 1
              val errorCount = errorCounter
              val groupAddressHead = if (size == 0) 0 else byteToAddress(group.head)
              errorCounter = 0

              val dataPhasePid = dataPhase match {
                case 0 => UsbPid.DATA0
                case 1 => UsbPid.DATA1
              }
              val dataPhasePidWrong = dataPhase match {
                case 0 => UsbPid.DATA1
                case 1 => UsbPid.DATA0
              }


              td0.DP match {
                case UsbOhci.DP.SETUP | UsbOhci.DP.OUT => {
                  val pushInterface = if (td0.DP == UsbOhci.DP.SETUP) scoreboards(0).pushSetup _ else scoreboards(0).pushOut _

                  def push(body: => Unit) = pushInterface(TockenKey(ed0.FA, ed0.EN), DataPacket(dataPhasePid, group.map(byteId => refData(byteId)))) {
                    activity = true;
                    body
                  }

                  if (Random.nextDouble() < 0.05) { //doNack
                    push {
                      portAgents(0).emitBytes(HANDSHAKE_NACK, List(), false, true)
                    }
                    errorCounter = errorCount
                  } else if (doTransmissionError && Random.nextDouble() < 0.5) { //XXX
                    errorCounter = errorCount + 1
                    val retire = errorCounter == 3

                    push {
                      //                  val expectedCc = 6 match {
                      val expectedCc = Random.nextInt(7) match { //XXX
                        case 0 => UsbOhci.CC.deviceNotResponding
                        case 1 => portAgents(0).emitBytes(List(HANDSHAKE_ACK), false, true); UsbOhci.CC.pidCheckFailure
                        case 2 => portAgents(0).emitBytes(HANDSHAKE_NACK, List(42), false, true); UsbOhci.CC.pidCheckFailure
                        case 3 => portAgents(0).emitBytes(List(), false, true); UsbOhci.CC.pidCheckFailure
                        case 4 => portAgents(0).emitBytes(DATA0, List(), false, true); UsbOhci.CC.unexpectedPid
                        case 5 => portAgents(0).emitBytes(List(Random.nextInt(256)), false, true, stuffingError = true); UsbOhci.CC.bitStuffing
                        case 6 => portAgents(0).emitBytes(List(Random.nextInt(256)), false, true, eopError = true); UsbOhci.CC.bitStuffing
                      }
                      if (retire) {
                        doneChecks(td0.address) = { td =>
                          ed0.load(m)
                          assert(td.CC == expectedCc)
                          assert(td.currentBuffer == groupAddressHead)
                          assert(ed0.H)
                          assert(td.EC == 3)
                          tdCompletion()
                        }
                      }
                    }
                    if (retire) {
                      continue = false
                    }
                  } else if (doStall && disruptAt == groupId) { //stall
                    push {
                      portAgents(0).emitBytes(HANDSHAKE_STALL, List(), false, true)
                      doneChecks(td0.address) = { td =>
                        ed0.load(m)
                        assert(td.CC == UsbOhci.CC.stall)
                        assert(td.currentBuffer == groupAddressHead)
                        assert(ed0.H)
                        tdCompletion()
                      }
                    }
                    continue = false
                  } else {
                    push {
                      portAgents(0).emitBytes(HANDSHAKE_ACK, List(), false, true)
                      if (groupLast) {
                        doneChecks(td0.address) = { td =>
                          ed0.load(m)
                          assert(td.currentBuffer == 0)
                          assert(td.CC == UsbOhci.CC.noError)
                          assert(td.EC == 0)
                          tdCompletion()
                        }
                      }
                    }
                    groupIdCounter += 1
                    dataPhaseIncrement()
                  }
                }
                case UsbOhci.DP.IN => {
                  def checkDataUntil(up: Int): Unit = {
                    for (i <- 0 until up) {
                      val address = byteToAddress(i)
                      assert(m.read(address) == refData(i).toByte, f"[$i] => ${m.read(address)}%x != ${refData(i).toByte}%x")
                    }
                  }

                  def push(body: => Boolean) = scoreboards(0).pushIn(TockenKey(ed0.FA, ed0.EN)) {
                    activity = true;
                    body
                  }

                  if (Random.nextDouble() < 0.05) { //doNak
                    push {
                      deviceDelayed(ls = false) {
                        portAgents(0).emitBytes(UsbPid.NAK, Nil, false, false)
                      }
                      false
                    }
                    errorCounter = errorCount
                  } else if (doTransmissionError && Random.nextDouble() < 0.5) { //XXX
                    errorCounter = errorCount + 1
                    val retire = errorCounter == 3
                    push {
                      //                  val expectedCc = 5 match { //XXX
                      val expectedCc = Random.nextInt(7) match {
                        case 0 => UsbOhci.CC.deviceNotResponding
                        case 1 => deviceDelayed(ls = false) {
                          portAgents(0).emitBytes(12 +: group.map(refData), true, false)
                        };
                          UsbOhci.CC.pidCheckFailure
                        case 2 => deviceDelayed(ls = false) {
                          portAgents(0).emitBytes(List(), false, true)
                        };
                          UsbOhci.CC.pidCheckFailure
                        case 3 => deviceDelayed(ls = false) {
                          portAgents(0).emitBytes(UsbPid.PING, group.map(refData), true, false)
                        };
                          UsbOhci.CC.unexpectedPid
                        case 4 => deviceDelayed(ls = false) {
                          portAgents(0).emitBytes((dataPhasePid | (~dataPhasePid << 4)) +: group.map(refData), true, false, stuffingError = true)
                        };
                          UsbOhci.CC.bitStuffing
                        case 5 => deviceDelayed(ls = false) {
                          portAgents(0).emitBytes((dataPhasePid | (~dataPhasePid << 4)) +: group.map(refData), true, false, crcError = true)
                        };
                          UsbOhci.CC.crc
                        case 6 => deviceDelayed(ls = false) {
                          portAgents(0).emitBytes((dataPhasePid | (~dataPhasePid << 4)) +: group.map(refData), true, false, eopError = true)
                        };
                          UsbOhci.CC.bitStuffing
                      }

                      if (retire) {
                        doneChecks(td0.address) = { td =>
                          ed0.load(m)
                          assert(td.CC == expectedCc)
                          assert(td.currentBuffer == groupAddressHead)
                          if (size != 0) checkDataUntil(group.head)
                          assert(ed0.H)
                          tdCompletion()
                        }
                      }
                      false
                    }
                    if (retire) {
                      continue = false
                    }
                  } else if (doStall && disruptAt == groupId) { //stall
                    push {
                      deviceDelayed(ls = false) {
                        portAgents(0).emitBytes(UsbPid.STALL, Nil, false, false)
                        doneChecks(td0.address) = { td =>
                          ed0.load(m)
                          assert(td.CC == UsbOhci.CC.stall)
                          assert(td.currentBuffer == groupAddressHead)
                          assert(ed0.H)
                          tdCompletion()
                        }
                      }
                      false
                    }
                    continue = false
                  } else if (doOverflow && groupId == disruptAt) {
                    push {
                      deviceDelayed(ls = false) {
                        portAgents(0).emitBytes(dataPhasePid, group.map(refData) ++ List.fill(Random.nextInt(4) + 1)(Random.nextInt(256)), true, false)
                        doneChecks(td0.address) = { td =>
                          ed0.load(m)
                          assert(td.CC == UsbOhci.CC.dataOverrun)
                          if (size != 0) {
                            assert(td.currentBuffer == byteToAddress(group.head))
                            checkDataUntil(group.last + 1)
                          } else {
                            assert(td.currentBuffer == 0)
                          }
                          assert(ed0.H)
                          tdCompletion()
                        }
                      }
                      false
                    }
                    continue = false
                  } else if (doUnderflow && groupId == disruptAt) {
                    push {
                      deviceDelayed(ls = false) {
                        val finalTransferSize = Random.nextInt(group.size)
                        portAgents(0).emitBytes(dataPhasePid, group.map(refData).take(finalTransferSize), true, false)
                        doneChecks(td0.address) = { td =>
                          ed0.load(m)
                          if (!td0.R) {
                            assert(td.CC == UsbOhci.CC.dataUnderrun)
                            assert(td.currentBuffer == byteToAddress(group.head))
                            checkDataUntil(group.head + finalTransferSize)
                            assert(ed0.H)
                          } else {
                            assert(td.CC == UsbOhci.CC.noError)
                            assert(td.currentBuffer == 0)
                            if (size != 0) checkDataUntil(group.head + finalTransferSize)
                          }
                          tdCompletion()
                        }
                      }
                      td0.R
                    }
                    if (td0.R) dataPhaseIncrement()
                    continue = false
                  } else if (doDataToggleMismatch && groupId == disruptAt) {
                    push {
                      deviceDelayed(ls = false) {
                        portAgents(0).emitBytes(dataPhasePidWrong, group.map(refData), true, false)
                        doneChecks(td0.address) = { td =>
                          ed0.load(m)
                          assert(td.CC == UsbOhci.CC.dataToggleMismatch)
                          assert(td.currentBuffer == groupAddressHead)
                          if (size != 0) checkDataUntil(group.head)
                          assert(ed0.H)
                          tdCompletion()
                        }
                      }
                      false
                    }
                    continue = false
                  } else {
                    push {
                      deviceDelayed(ls = false) {
                        portAgents(0).emitBytes(dataPhasePid, group.map(refData), true, false)
                        if (groupLast) {
                          doneChecks(td0.address) = { td =>
                            ed0.load(m)
                            assert(td.CC == UsbOhci.CC.noError)
                            assert(td.currentBuffer == 0)
                            if (size != 0) checkDataUntil(group.head + group.size)
                            tdCompletion()
                          }
                        }
                      }
                      true
                    }
                    groupIdCounter += 1
                    dataPhaseIncrement()
                  }
                }
              }
            }
            listRefilled()
          }
        }
      }







      while(activity){
        activity = false
        sleep(10e-3*1e12)
      }

      dut.clockDomain.waitSampling()
      scoreboards.foreach(_.assertEmpty)
      assert(ctrl.read(UsbOhci.HcDoneHead) == 0)
      assert(doneChecks.isEmpty)
      println(totalBytes)
      println(simTime())
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


