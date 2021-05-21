
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
      dataWidth = 64,
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
//          0e-3 -> 15e-3
//            400e-3 -> 750e-3
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
      val doneChecksIso = mutable.HashMap[Int, TDI => Unit]()
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

              if(doneChecks.contains(ptr)) {
                val td = TD(ptr).load(memory.memory)
                doneChecks.remove(ptr).get.apply(td)
                ptr = td.nextTD
              }
              else if(doneChecksIso.contains(ptr)) {
                val td = TDI(ptr).load(memory.memory)
                doneChecksIso.remove(ptr).get.apply(td)
                ptr = td.nextTD
              }
              else ???
            }
          }
        }
      }

      val ports = for(i <- 0 until 4) yield fork{
        devices(i).connect(lowSpeed = i%2 == 1)
        waitConnected(i)
        setPortReset(i)
        waitPortReset(i)
      }

      ports.foreach(_.join())


      def connectedFs = devices.filter(e => e.connected && !e.lowSpeed)



      def addTd(td : TD, ed: ED): Unit ={
        if(ed.tailP == ed.headP){
          ed.headP = td.address
        } else {
          val last = TD(ed.tailP).load(m)
          last.nextTD = td.address
        }
        ed.tailP = td.address
      }

      def initED(ed : ED, isIso : Boolean): Unit ={
        if(isIso) {
          val td = TDI(malloc)
          ed.tailP = td.address
          ed.headP = td.address
        } else {
          val td = TD(malloc)
          ed.tailP = td.address
          ed.headP = td.address
        }
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


      def newTdi(ed: ED) : TDI = {
        ed.load(m)
        val td = TDI(ed.tailP)
        val dummy = TDI(malloc)
        ed.tailP = dummy.address
        td.nextTD = dummy.address
        ed.save(m)
        td
      }


      val bulksEd, controlsEd, interruptsEd, isochonousEd = ArrayBuffer[ED]()
      def edTargetConcflicting(ed : ED) = (bulksEd ++ controlsEd ++ interruptsEd ++ isochonousEd).exists(e => e.FA == ed.FA && e.EN == ed.EN)

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

      //Cluncky but functional
      val interruptsOccupancy = Array.fill(32)(0)
      def addInterruptEd(ed : ED, rate : Int, phase : Int): Unit = {
        ed.nextED = m.readInt(hcca.address + phase*4)
        for(i <- phase until 32 by rate){
          val head = hcca.address + i*4
          m.writeInt(head, ed.address)
          interruptsOccupancy(i) += ed.MPS + 5 + 4 + 2
        }
        interruptsEd += ed
      }

      val isoOccupancy = Array.fill(0x10000)(0)
      def addIsochronous(ed : ED): Unit ={
        for(i <- 0 until 32){
          var ptr = hcca.address + i*4
          var next = 0
          var skip, done = false
          //Walk from hcca(i) to its last element before the insertion
          do{
            next = m.readInt(ptr)
            if(next == ed.address){
              skip = true
            } else if(next == 0){
              done = true
            } else {
              ptr = next + 0xC
            }
          } while(!skip && !done)
          if(!skip){
            m.writeInt(ptr, ed.address)
          }
        }
        isochonousEd += ed
      }

      def currentFrameNumber = dut.ohci.reg.hcFmNumber.FN.toInt
      var totalBytes = 0
      val edCount = 20 //XXX
      for(edId <- 0 until edCount) {
        val CONTROL = 0
        val BULK = 1
        val INTERRUPT = 2
        val ISOCHRONOUS = 3
        var edKind = List(CONTROL, BULK, INTERRUPT, ISOCHRONOUS).randomPick()
        //        var edKind = List(CONTROL, BULK, ISOCHRONOUS).randomPick()  //XXX
//        edKind = ISOCHRONOUS //XXX
        def isIso = edKind == ISOCHRONOUS
        val ed0 = ED(malloc)
        do {
          ed0.F = isIso
          ed0.S = edKind != BULK && edKind != ISOCHRONOUS && Random.nextDouble() < 0.4
          //          ed0.S = true //XXX
          ed0.D = if(isIso) 1 + Random.nextInt(2) else 0
          //          ed0.D = if(isIso) 1  else 0 //XXX
//          ed0.D = 2 //XXX
          ed0.FA = Random.nextInt(128)
          ed0.EN = Random.nextInt(16)
          ed0.MPS = ed0.S match {
            case false => 16 + Random.nextInt(if(isIso) 8 else 48+1)
            case true =>  1  + Random.nextInt(if(isIso) 4 else 8)
          }
        } while(edTargetConcflicting(ed0))
        def isIsoOut : Boolean = ed0.D == 1
        initED(ed0, isIso)
        edKind match {
          case BULK => addBulkEd(ed0)
          case CONTROL => addControlEd(ed0)
          case INTERRUPT => addInterruptEd(ed0, rate = 4, phase = interruptsEd.size % 4)
          case ISOCHRONOUS => addIsochronous(ed0)
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
          case INTERRUPT =>
        }
        def unhaltEndpoint(ed: ED): Unit = fork{
          sleep(Random.nextInt(5)*1e9)
          ed.H = false
          ed.save(m)
          listRefilled()
        }
        def deviceDelayed(body : => Unit){
          delayed(4*83333*(if(ed0.S) 8 else 1)) {body} //TODO improve
        }

        fork {
          var busyUntilFrame = currentFrameNumber + 1
          for (tdId <- 0 until 10) { //XXX
            var size = if (edKind != INTERRUPT && Random.nextDouble() < 0.1) {
              Random.nextInt(8192 + 1)
            } else if (Random.nextDouble() < 0.05) {
              0
            } else if (edKind != INTERRUPT && Random.nextDouble() < 0.05) {
              8192
            } else {
              Random.nextInt(256 + 1)
            }
            if(ed0.S) size /= 8
            if (isIso) size = size.min(1023 * 8)
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

            var groups = ArrayBuffer[Seq[Int]]()
            if((currentFrameNumber+1 - busyUntilFrame).toShort > 0) busyUntilFrame = (currentFrameNumber + 1) & 0xFFFF
            if (isIso) {
              val td = newTdi(ed0)
              td.CC = UsbOhci.CC.notAccessed
              td.DI = 3
              td.SF = (busyUntilFrame + Random.nextInt(3)) & 0xFFFF
              td.currentBuffer = p0 + p0Offset
              td.bufferEnd = if (p1Used != 0) p1 + p1Used - 1 else p0 + p0Offset + p0Used - 1
              do {
                td.FC = Random.nextInt(8) max((size+1022)/1023-1)
                var offset = p0Offset
                var byteId = 0
                for (i <- 0 to td.FC) {
                  val bytesLeft = (size - (offset - p0Offset))
                  val transferLeft = (td.FC + 1 - i)
                  val averageSize = (bytesLeft + transferLeft - 1) / transferLeft
                  val minimalSize = (bytesLeft - (transferLeft - 1) * 1023) max (0)
                  val maximalSize = bytesLeft min 1023
                  val tSize = (minimalSize + Random.nextInt((averageSize - minimalSize) * 2 + 1)) min maximalSize

                  td.offsets(i) = offset
                  groups += (byteId until byteId + tSize)
                  offset += tSize
                  byteId += tSize
                }
              } while((0 to td.FC).map(td.offsets(_)).exists(_ >= 8192))

              val isoOverhead = 1+4+4
              var scheduleOverrun = false
              do{
                //                println("\n")
                scheduleOverrun = (0 to td.FC).exists{i =>
                  val frameId = (td.SF + i) & 0xFFFF
                  //                  println(isoOccupancy(frameId) + groups(i).size)
                  interruptsOccupancy(frameId & 0x1F) +  isoOccupancy(frameId) + groups(i).size + isoOverhead> 1023+64+64+64+64+64
                }
                if(scheduleOverrun) td.SF = (td.SF + 1 + Random.nextInt(2)) & 0xFFFF
              } while(scheduleOverrun)

              td.save(ram)

              busyUntilFrame = td.SF + td.FC + 1
              busyUntilFrame &= 0xFFFF

              for (i <- 0 until td.SF - currentFrameNumber) {
                isoOccupancy((currentFrameNumber+i) & 0xFFFF) += 1
              }
              for (i <- 0 to td.FC) {
                val frameId = (td.SF + i) & 0xFFFF
                isoOccupancy(frameId) += groups(i).size + isoOverhead
              }


              val reportName = f"${td.address}%08x => ${groups.map(_.size).mkString(", ")}"
              println(reportName)
              val stimWaitCompletion = Random.nextDouble() < 0.3
              val completionMutex = SimMutex()
              var pendingCompletion = 0

              if (stimWaitCompletion) completionMutex.lock()

              def tdCompletion() {
                if (p0Used != 0) malloc.free(p0 + p0Offset)
                if (p1Used != 0) malloc.free(p1)
                malloc.free(td.address)
                println("DONNNNE")
                for (i <- 0 until td.SF - currentFrameNumber) {
                  isoOccupancy((currentFrameNumber+i) & 0xFFFF) -= 1
                }
                for (i <- 0 to td.FC) {
                  val frameId = (td.SF + i) & 0xFFFF
                  isoOccupancy(frameId) -= groups(i).size + isoOverhead
                }
                if (stimWaitCompletion) {
                  delayed(Random.nextDouble() * 2e9 toInt) {
                    completionMutex.unlock()
                    println("unlock at " + simTime())
                  }
                }
              }

              val refData = Array.fill(size)(Random.nextInt(256))
              if (isIsoOut) for (i <- 0 until size) {
                val address = byteToAddress(i)
                ram.write(address, refData(i).toByte)
              }
              val refCc = Array.fill(groups.size)(UsbOhci.CC.noError)
              val refPsw = groups.map(_.size)
              val refCheckData = Array.fill(groups.size)(true)

              var doTransmitionError = Random.nextDouble() < 0.4
              var doUnderflow = Random.nextDouble() < 0.4
              var doOverflow = Random.nextDouble() < 0.4

//              doTransmitionError = true //XXX
//              doUnderflow = true //XXX
//              doOverflow = true //XXX


              val agent = portAgents.filter(a => a.connected && (!a.lowSpeed || ed0.S)).randomPick()
              var groupIdCounter = 0
              var continue = true
              while (continue && groupIdCounter < groups.size) {
                val groupId = groupIdCounter
                val group = groups(groupId)
                val groupLast = groupId == groups.size - 1
                //                val groupAddressHead = if (size == 0) 0 else byteToAddress(group.head)
                if (isIsoOut) {
                  for(portId <- 0 until dut.p.portCount if portAgents(portId).connected && (!portAgents(portId).lowSpeed || ed0.S)){
                    scoreboards(portId).pushOut(TockenKey(ed0.FA, ed0.EN), DataPacket(UsbPid.DATA0, group.map(byteId => refData(byteId)))) {
                      activity = true;
                      assert(currentFrameNumber-td.SF == groupId)
                      if (groupLast && portAgents(portId) == agent) {
                        doneChecksIso(td.address) = { td =>
                          ed0.load(m)
                          assert(td.CC == UsbOhci.CC.noError)
                          for(i <- 0 to td.FC){
                            assert(td.offsets(i) == 0)
                          }
                          tdCompletion()
                        }
                      }
                    }
                  }

                  groupIdCounter += 1
                } else { // ISO IN
                  def push(body: => Unit) = {
                    for(portId <- 0 until dut.p.portCount if portAgents(portId).connected && (!portAgents(portId).lowSpeed || ed0.S)) {
                      scoreboards(portId).pushIn(TockenKey(ed0.FA, ed0.EN)) {
                        activity = true
                        assert(currentFrameNumber - td.SF == groupId)
                        if(portAgents(portId) == agent) body
                        false
                      }
                    }
                  }

                  def lastCheck(): Unit ={
                    if (groupLast) {
                      doneChecksIso(td.address) = { td =>
                        println(s"Check ${reportName}")
                        ed0.load(m)
                        assert(td.CC == UsbOhci.CC.noError)
                        for(i <- 0 until groups.size) {
                          val cc = (td.offsets(i) >> 12) & 0xF
                          val bytes = td.offsets(i) & 0xFFF
                          assert(cc == refCc(i))
                          if(refPsw(i) != -1) {
                            assert(bytes == refPsw(i))
                          }
                          if(refCheckData(i)) {
                            for (i <- groups(i).take(bytes)) {
                              val address = byteToAddress(i)
                              assert(m.read(address) == refData(i).toByte, f"[$i] => ${m.read(address)}%x != ${refData(i).toByte}%x")
                            }
                          }
                        }
                        tdCompletion()
                      }
                    }
                  }
                  if(doTransmitionError && Random.nextDouble() < 0.5){//XXX
                    push {
//                      val errorAt = Random.nextInt(group.size)

                      val (expectedCc, expectedBytes, checkData)  = Random.nextInt(7) match {
//                      val (expectedCc, expectedBytes, checkData) = 5 match { //XXX
                        case 0 => (UsbOhci.CC.deviceNotResponding, 0, false)
                        case 1 => deviceDelayed { agent.emitBytes(12 +: group.map(refData), true, false, ls=ed0.S) }; (UsbOhci.CC.pidCheckFailure, group.size, true)
                        case 2 => deviceDelayed { agent.emitBytes(List(), false, true, ls=ed0.S) }; (UsbOhci.CC.pidCheckFailure, 0, false)
                        case 3 => deviceDelayed { agent.emitBytes(UsbPid.PING, group.map(refData), true, false, ls=ed0.S) }; (UsbOhci.CC.unexpectedPid, group.size, true)
                        case 4 => deviceDelayed { agent.emitBytes((DATA0 | (~DATA0 << 4)) +: group.map(refData), true, false, ls=ed0.S, stuffingError = true, errorAt = -1)};(UsbOhci.CC.bitStuffing, -1, false)
                        case 5 => deviceDelayed { agent.emitBytes((DATA0 | (~DATA0 << 4)) +: group.map(refData), true, false, ls=ed0.S, crcError = true) }; (UsbOhci.CC.crc, group.size, false)
                        case 6 => deviceDelayed { agent.emitBytes((DATA0 | (~DATA0 << 4)) +: group.map(refData), true, false, ls=ed0.S, eopError = true) }; (UsbOhci.CC.bitStuffing, -1, false)
                      }
                      refCc(groupId) = expectedCc
                      refPsw(groupId) = expectedBytes
                      refCheckData(groupId) = checkData
                      lastCheck()
                    }
                  } else if(doOverflow && Random.nextDouble() < 0.4) { //XXX
                    push {
                      deviceDelayed {
                        agent.emitBytes(UsbPid.DATA0, group.map(refData) :+ Random.nextInt(256), true, false, ls=ed0.S)
                        lastCheck()
                      }
                      refCc(groupId) = UsbOhci.CC.dataOverrun
                    }
                  } else if(doUnderflow && group.size != 0 && Random.nextDouble() < 0.4) { //XXX
                    push {
                      val byteCount = Random.nextInt(group.size)
                      deviceDelayed {
                        agent.emitBytes(UsbPid.DATA0, group.map(refData).take(byteCount), true, false, ls=ed0.S)
                        lastCheck()
                      }
                      refCc(groupId) = UsbOhci.CC.dataUnderrun
                      refPsw(groupId) = byteCount
                    }
                  } else {
                    push {
                      deviceDelayed {
                        agent.emitBytes(if(Random.nextBoolean()) UsbPid.DATA0 else UsbPid.DATA1, group.map(refData), true, false, ls=ed0.S)
                        lastCheck()
                      }
                    }
                  }

                  groupIdCounter += 1
                }
              }

              completionMutex.lock()
              completionMutex.unlock()
            } else {
              val td0 = newTd(ed0)

              td0.DP = Random.nextInt(4).min(2)
              //              td0.DP = UsbOhci.DP.IN //XXX
              td0.DI = 5
              td0.T = 0
              if (Random.nextDouble() < 0.5) {
                dataPhase = Random.nextInt(2)
                td0.T = 2 | dataPhase
              }
              td0.R = Random.nextBoolean()
              td0.currentBuffer = if (size == 0) 0 else p0 + p0Offset
              td0.bufferEnd = if (p1Used != 0) p1 + p1Used - 1 else p0 + p0Offset + p0Used - 1
              td0.CC = UsbOhci.CC.notAccessed
              td0.save(ram)

              val stimWaitCompletion = Random.nextDouble() < 0.3
              val completionMutex = SimMutex()

              if (stimWaitCompletion) completionMutex.lock()

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
                if (stimWaitCompletion) {
                  delayed(Random.nextDouble() * 2e9 toInt) {
                    completionMutex.unlock()
                    println("unlock at " + simTime())
                  }
                }
              }

              var doOverflow = td0.DP == UsbOhci.DP.IN && Random.nextDouble() < 0.05
              var doUnderflow = td0.DP == UsbOhci.DP.IN && !doOverflow && size != 0 && Random.nextDouble() < 0.10
              var doStall = Random.nextDouble() < 0.05
              var doTransmissionError = Random.nextDouble() < 0.05

//                            doOverflow = false //XXX
//                            doUnderflow = true //XXX
//                            doStall = false //XXX
//                            doTransmissionError = false //XXX

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
              //            println(s"!! ${ed0.EN} ${ed0.FA}")

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

                val agent = portAgents.filter(a => a.connected && (!a.lowSpeed || ed0.S)).randomPick()
                td0.DP match {
                  case UsbOhci.DP.SETUP | UsbOhci.DP.OUT => {

                    def push(body: => Unit) = for(portId <- 0 until dut.p.portCount if portAgents(portId).connected && (!portAgents(portId).lowSpeed || ed0.S)){
                      val pushInterface = if (td0.DP == UsbOhci.DP.SETUP) scoreboards(portId).pushSetup _ else scoreboards(portId).pushOut _
                      pushInterface(TockenKey(ed0.FA, ed0.EN), DataPacket(dataPhasePid, group.map(byteId => refData(byteId)))) {
                        activity = true;
                        if(portAgents(portId) == agent) body
                      }
                    }

                    if (Random.nextDouble() < 0.05) { //doNack
                      push {
                        agent.emitBytes(HANDSHAKE_NACK, List(), false, true, ls=ed0.S)
                      }
                      errorCounter = errorCount
                    } else if (doTransmissionError && Random.nextDouble() < 0.5) { //XXX
                      errorCounter = errorCount + 1
                      val retire = errorCounter == 3

                      push {
                        //                  val expectedCc = 6 match {
                        val expectedCc = Random.nextInt(7) match { //XXX
                          case 0 => UsbOhci.CC.deviceNotResponding
                          case 1 => agent.emitBytes(List(HANDSHAKE_ACK), false, true, ls=ed0.S); UsbOhci.CC.pidCheckFailure
                          case 2 => agent.emitBytes(HANDSHAKE_NACK, List(42), false, true, ls=ed0.S); UsbOhci.CC.pidCheckFailure
                          case 3 => agent.emitBytes(List(), false, true, ls=ed0.S); UsbOhci.CC.pidCheckFailure
                          case 4 => agent.emitBytes(DATA0, List(), false, true, ls=ed0.S); UsbOhci.CC.unexpectedPid
                          case 5 => agent.emitBytes(List(Random.nextInt(256)), false, true, ls=ed0.S, stuffingError = true); UsbOhci.CC.bitStuffing
                          case 6 => agent.emitBytes(List(Random.nextInt(256)), false, true, ls=ed0.S, eopError = true); UsbOhci.CC.bitStuffing
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
                        agent.emitBytes(HANDSHAKE_STALL, List(), false, true, ls=ed0.S)
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
                        agent.emitBytes(HANDSHAKE_ACK, List(), false, true, ls=ed0.S)
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

                    def push(hcAck : Boolean)(body: => Unit) = {
                      for(portId <- 0 until dut.p.portCount if portAgents(portId).connected && (!portAgents(portId).lowSpeed || ed0.S)) {
                        scoreboards(portId).pushIn(TockenKey(ed0.FA, ed0.EN)) {
                          activity = true;
                          if(portAgents(portId) == agent) body
                          hcAck
                        }
                      }
                    }

                    if (Random.nextDouble() < 0.05) { //doNak
                      push(false) {
                        deviceDelayed {
                          agent.emitBytes(UsbPid.NAK, Nil, false, false, ls=ed0.S)
                        }
                      }
                      errorCounter = errorCount
                    } else if (doTransmissionError && Random.nextDouble() < 0.5) { //XXX
                      errorCounter = errorCount + 1
                      val retire = errorCounter == 3

                      val errorKind = Random.nextInt(8)
                      //                      val errorKind = 7
                      push(errorKind == 7) {
                        val expectedCc = errorKind match {
                          case 0 => UsbOhci.CC.deviceNotResponding
                          case 1 => deviceDelayed { agent.emitBytes(12 +: group.map(refData), true, false, ls=ed0.S) }; UsbOhci.CC.pidCheckFailure
                          case 2 => deviceDelayed { agent.emitBytes(List(), false, true, ls=ed0.S) }; UsbOhci.CC.pidCheckFailure
                          case 3 => deviceDelayed { agent.emitBytes(UsbPid.PING, group.map(refData), true, false, ls=ed0.S) }; UsbOhci.CC.unexpectedPid
                          case 4 => deviceDelayed { agent.emitBytes((dataPhasePid | (~dataPhasePid << 4)) +: group.map(refData), true, false, ls=ed0.S, stuffingError = true)};UsbOhci.CC.bitStuffing
                          case 5 => deviceDelayed { agent.emitBytes((dataPhasePid | (~dataPhasePid << 4)) +: group.map(refData), true, false, ls=ed0.S, crcError = true) }; UsbOhci.CC.crc
                          case 6 => deviceDelayed { agent.emitBytes((dataPhasePid | (~dataPhasePid << 4)) +: group.map(refData), true, false, ls=ed0.S, eopError = true) }; UsbOhci.CC.bitStuffing
                          case 7 => deviceDelayed { agent.emitBytes(dataPhasePidWrong, group.map(refData), true, false, ls=ed0.S)} ; UsbOhci.CC.dataToggleMismatch
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
                      }
                      if (retire) {
                        continue = false
                      }
                    } else if (doStall && disruptAt == groupId) { //stall
                      push(false) {
                        deviceDelayed {
                          agent.emitBytes(UsbPid.STALL, Nil, false, false, ls=ed0.S)
                          doneChecks(td0.address) = { td =>
                            ed0.load(m)
                            assert(td.CC == UsbOhci.CC.stall)
                            assert(td.currentBuffer == groupAddressHead)
                            assert(ed0.H)
                            tdCompletion()
                          }
                        }
                      }
                      continue = false
                    } else if (doOverflow && groupId == disruptAt) {
                      push(true) {
                        deviceDelayed {
                          agent.emitBytes(dataPhasePid, group.map(refData) ++ List.fill(Random.nextInt(4) + 1)(Random.nextInt(256)), true, false, ls=ed0.S)
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
                      }
                      dataPhaseIncrement()
                      continue = false
                    } else if (doUnderflow && groupId == disruptAt) {
                      push(true) {
                        deviceDelayed {
                          val finalTransferSize = Random.nextInt(group.size)
                          agent.emitBytes(dataPhasePid, group.map(refData).take(finalTransferSize), true, false, ls=ed0.S)
                          doneChecks(td0.address) = { td =>
                            ed0.load(m)
                            if (!td0.R) {
                              assert(td.CC == UsbOhci.CC.dataUnderrun)
                              assert(td.currentBuffer == byteToAddress(group.head + finalTransferSize))
                              checkDataUntil(group.head + finalTransferSize)
                              assert(ed0.H)
                            } else {
                              assert(td.CC == UsbOhci.CC.noError)
                              assert(td.currentBuffer == byteToAddress(group.head + finalTransferSize))
                              if (size != 0) checkDataUntil(group.head + finalTransferSize)
                            }
                            tdCompletion()
                          }
                        }
                      }
                      dataPhaseIncrement()
                      continue = false
                    } else {
                      push(true) {
                        deviceDelayed {
                          agent.emitBytes(dataPhasePid, group.map(refData), true, false, ls=ed0.S)
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
                      }
                      groupIdCounter += 1
                      dataPhaseIncrement()
                    }
                  }
                }
              }
              listRefilled()

              completionMutex.lock()
              completionMutex.unlock()
            }
          }
        }
      }







      while(activity){
        activity = false
        sleep(14e-3*1e12)
      }

      dut.clockDomain.waitSampling()
      scoreboards.foreach(_.assertEmpty)
      assert(ctrl.read(UsbOhci.HcDoneHead) == 0)
      assert(doneChecks.isEmpty)
      assert(doneChecksIso.isEmpty)
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

    val targets = XilinxStdTargets().take(2)

    Bench(rtls, targets)
  }
}


