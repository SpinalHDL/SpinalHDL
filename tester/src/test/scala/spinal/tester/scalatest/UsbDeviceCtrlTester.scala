package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.bmb.BmbParameter
import spinal.lib.bus.bmb.sim.BmbDriver
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.com.usb.ohci.UsbPid
import spinal.lib.com.usb.phy.UsbDevicePhyNative
import spinal.lib.com.usb.sim.UsbLsFsPhyAbstractIoAgent
import spinal.lib.com.usb.udc.UsbDeviceCtrl.{Regs, Status}
import spinal.lib.com.usb.udc.{UsbDeviceCtrl, UsbDeviceCtrlParameter}
import spinal.lib.sim.MemoryRegionAllocator

import scala.collection.mutable
import scala.util.Random

case class UsbDeviceCtrlTesterTop() extends Component {
  val ctrlCd = ClockDomain.external("ctrlCd", frequency = FixedFrequency(100 MHz))
  val phyCd = ClockDomain.external("phyCd", frequency = FixedFrequency(48 MHz))
//  val phyCd = ctrlCd

  val ctrl = ctrlCd on new UsbDeviceCtrl(
    p = UsbDeviceCtrlParameter(
      addressWidth = 14
    ),
    bmbParameter = BmbParameter(
      addressWidth = UsbDeviceCtrl.ctrlAddressWidth,
      dataWidth = 32,
      sourceWidth = 0,
      contextWidth = 0,
      lengthWidth = 2
    )
  )

  val phy = phyCd on new UsbDevicePhyNative(sim = true)
  ctrl.io.phy.cc(ctrlCd, phyCd) <> phy.io.ctrl

  val bmb = ctrl.io.ctrl.toIo()
  val usb = phy.io.usb.toIo()
  val power = phy.io.power.toIo()
  val pullup = phy.io.pullup.toIo()
  val interrupts = ctrl.io.interrupt.toIo()

  ctrl.regs.address.value.simPublic()
  ctrl.regs.address.enable.simPublic()
}

class UsbDeviceCtrlTester extends AnyFunSuite{
  for(i <- 43 to 44) test("miaou" + i){
    SimConfig.compile(UsbDeviceCtrlTesterTop()).doSim(seed = i){ dut =>
      dut.ctrlCd.forkStimulus(1e12/dut.ctrlCd.frequency.getValue.toDouble toLong)
      dut.phyCd.forkStimulus(1e12/dut.phyCd.frequency.getValue.toDouble  toLong)
      val usbAgent = new UsbLsFsPhyAbstractIoAgent(dut.usb, dut.phyCd, dut.phy.fsRatio)

      val ctrl = BmbDriver(dut.bmb, dut.ctrlCd)

      val deviceAddress = 0x53
      var frameCounter = 0x566

      def newFrame(): Unit ={
        frameCounter += 1
        usbAgent.emitBytes(List(UsbPid.SOF | (~UsbPid.SOF << 4), frameCounter & 0xFF, frameCounter >> 8), crc16 = false, turnaround = true, ls = false, crc5 = true)
        usbAgent.waitDone()
      }

      for(i <- 0 until 16){
        ctrl.write(0, i*4)
      }
      ctrl.write(deviceAddress | 0x100, UsbDeviceCtrl.Regs.ADDRESS)
      ctrl.write(0xFFFFFFFFl, UsbDeviceCtrl.Regs.INTERRUPT)
      ctrl.write(0x5, UsbDeviceCtrl.Regs.CONFIG)
      assert(dut.ctrl.regs.address.value.toInt == deviceAddress)

      dut.power #= true
      dut.phyCd.waitSampling(100)
      usbAgent.connect(false)
      dut.phyCd.waitSampling(100)
      usbAgent.emitReset()
      usbAgent.waitDone()
      assert(dut.ctrl.regs.address.enable.toBoolean == false)
      dut.phyCd.waitSampling(100)
      assert(ctrl.read(UsbDeviceCtrl.Regs.INTERRUPT) == 1 << 16)
      ctrl.write(0xFFFFFFFFl, UsbDeviceCtrl.Regs.INTERRUPT)

      println(s"suspend started at $simTime")
      usbAgent.emitSuspend()
      usbAgent.waitDone()
      dut.phyCd.waitSampling(100)
      assert(ctrl.read(UsbDeviceCtrl.Regs.INTERRUPT) == 1 << 18)
      ctrl.write(0xFFFFFFFFl, UsbDeviceCtrl.Regs.INTERRUPT)


      println(s"resume started at $simTime")
      usbAgent.emitResume()
      usbAgent.waitDone()
      dut.phyCd.waitSampling(100)
      assert(ctrl.read(UsbDeviceCtrl.Regs.INTERRUPT) == 1 << 19)
      ctrl.write(0xFFFFFFFFl, UsbDeviceCtrl.Regs.INTERRUPT)

      newFrame()
      assert(ctrl.read(UsbDeviceCtrl.Regs.FRAME) == frameCounter)
      ctrl.write(deviceAddress | 0x100, UsbDeviceCtrl.Regs.ADDRESS)


      val schedules = mutable.LinkedHashMap[Int, mutable.Queue[() => Unit]]()
      def schedule(at : Int)(body : => Unit): Unit ={
        schedules.getOrElseUpdate(at, mutable.Queue[() => Unit]()) += (() => body)
      }

      val lock = SimMutex()
      val alloc = MemoryRegionAllocator(0, 1 << dut.ctrl.p.addressWidth)
      alloc.allocateOn(0, 16*4+8)

      class Descriptor(val address : Int){
        var offset = 0
        var code = 0
        var next = 0
        var length = 0
        var direction = false
        var interrupt = false
        var completionOnFull = false
        var frame = 0

        def write(): Unit ={
          ctrl.write((offset << 0) | (code << 16), address + 0)
          ctrl.write((next << 4) | (length << 16), address + 4)
          ctrl.write((frame << 0) | (direction.toInt << 16) | (interrupt.toInt << 17) | (completionOnFull.toInt << 18), address + 8)
        }

        def writeW1(): Unit ={
          ctrl.write((next << 4) | (length << 16), address + 4)
        }

        def read(): Unit ={
          val v = ctrl.read(address + 0).toInt
          offset = v & 0xFFFF
          code = (v >> 16) & 0xF
        }


        def readData(head : Int, count : Int) : Seq[Int] = {
          val ret = Array.fill(count)(0)
          for(w <- head/4 to (head+count+3)/4){
            val value = ctrl.read(address + 12 + w*4).toInt
            for(b <- 0 to 3 if w*4+b >= head && w*4+b < head + count){
              ret(w*4+b-head) = (value >> b*8) & 0xFF
            }
          }
          ret
        }
        val checks = mutable.Queue[() => Unit]()
        def addCheck(body : => Unit) = checks += (() => body)
      }

      val descQueues = Array.fill(16)(mutable.Queue[Descriptor]())
      var setupCheck : () => Unit = null
      fork {
        while(true){
          dut.ctrlCd.waitSamplingWhere(dut.interrupts.toBoolean)
          println("INT")
          var mask = ctrl.read(UsbDeviceCtrl.Regs.INTERRUPT).toInt
          ctrl.write(mask, UsbDeviceCtrl.Regs.INTERRUPT)

          while(mask != 0) {
            val int = java.lang.Integer.numberOfTrailingZeros(mask)
            int match {
              case descriptorId if int < 16 => {
                val desc = descQueues(descriptorId).head
                desc.read()
                if(desc.code != UsbDeviceCtrl.Code.NONE){
                  desc.checks.foreach(_.apply())
                  descQueues(descriptorId).dequeue()
                }
              }
              case 16 => println("USB RESET")
              case 18 => println("USB SUSPEND")
              case 19 => println("USB RESUME")
              case 17 => {
                println("EP SETUP")
                setupCheck()
                setupCheck = null
              }
            }
            mask &= ~(1 << int)
          }
        }
      }

      //TODO
      val transferPerEndpoint = 10 //TODO
      val endpointCount = 16 //TODO
      val endpoints = for(endpointId <- 0 until endpointCount) yield fork {
        val maxPacketSize = Random.nextInt(56)+8//List(8,16,32,64).randomPick()
        var frameCurrent = frameCounter
        var descs = mutable.Queue[Descriptor]()
        val isochronous = Random.nextDouble() < 0.3  && endpointId != 0//TODO
        var phase = UsbPid.DATA0



        def newDescriptor(size : Int) = {
          var m : SizeMapping = null
          while(m == null) {
            m = alloc.allocateAligned(12 + size, 16)
            if(m == null) dut.ctrlCd.waitSampling(Random.nextInt(1000))
          }
          println(s"MEMORY FREE : ${alloc.freeSize}")
          val d = new Descriptor(m.base.toInt)
          descQueues(endpointId) += d
          d
        }

        def freeDescriptor(desc : Descriptor) = {
          alloc.free(desc.address)
          assert(descs.head == desc)
          descs.dequeue()
        }

        def push(desc : Descriptor): Unit ={
          println(s"push at ${simTime()}")
          if(descs.nonEmpty){
            descs.last.next = desc.address >> 4
            descs.last.writeW1()
          }

          var status = ctrl.read(endpointId*4).toInt
          if((status & 0xFFF0) == 0) {
            status = status & ~0xFFF0
            status |= desc.address
            ctrl.write(status, endpointId*4)
          }

          descs += desc
        }



        ctrl.write((maxPacketSize << 22) | (isochronous.toInt << 16) | 1, endpointId*4)

        def readData(head : Int, count : Int) : Seq[Int] = {
          val ret = Array.fill(count)(0)
          for(w <- head/4 to (head+count+3)/4){
            val value = ctrl.read(0 + w*4).toInt
            for(b <- 0 to 3 if w*4+b >= head && w*4+b < head + count){
              ret(w*4+b-head) = (value >> b*8) & 0xFF
            }
          }
          ret
        }

        val endpointThread = simThread
        for(_ <- 0 until transferPerEndpoint) {

          def phaseToggle() = phase = if (phase == UsbPid.DATA0) UsbPid.DATA1 else UsbPid.DATA0

          def frameCurrentUpdate(): Unit = {
            frameCurrent = frameCurrent.max(frameCounter + 1)
            if (Random.nextDouble() < 0.3) frameCurrent += Random.nextInt(4) + 1
          }
          val setup = endpointId == 0 &&  Random.nextDouble() < 0.4 //TODO
//          val setup = endpointId == 0 && true
          if (setup) {
            val length = Random.nextInt(9)
            val dataRef = List.fill(length)(Random.nextInt(256))

            frameCurrentUpdate()
            def task(): Unit ={
              phase = UsbPid.DATA0
              usbAgent.emitBytes(List(UsbPid.SETUP | (~UsbPid.SETUP << 4), deviceAddress | (endpointId << 7), endpointId >> 1), crc16 = false, turnaround = true, ls = false, crc5 = true)
              usbAgent.waitDone()
              usbAgent.emitBytes(List(phase | (~phase << 4)) ++ dataRef, crc16 = true, turnaround = true, ls = false, crc5 = false)
              usbAgent.waitDone()
              val (pid, payload) = usbAgent.rxBlocking()
              assert(pid == UsbPid.ACK && payload.isEmpty)
              phaseToggle()
            }
            schedule(frameCurrent)(task())
            setupCheck = () => {
              val dataDut = readData(0x40, length)
              assert(dataDut == dataRef)
              endpointThread.resume()
            }
            endpointThread.suspend()
          } else {
            val length = Random.nextInt(maxPacketSize * 4)
            var left = length
            var notEnough = true

            while (notEnough) {
              var descLength = (maxPacketSize * (1 + Random.nextInt(4)))
              val direction = Random.nextBoolean() //TODO
              //            val direction = true
              if (direction) descLength = descLength min left
              val desc = newDescriptor(descLength)
              var descOffset = 0
              val sleepOnCompletion = true//Random.nextDouble() < 0.2 //TODO
              var descNotDone = true
              val descUsefullLength = descLength min left

              desc.offset = descOffset
              desc.code = 0xF
              desc.next = 0
              desc.length = descLength
              desc.direction = direction
              desc.interrupt = true //TODO
              desc.completionOnFull = descLength < left
              desc.frame = 0

              println(s"Descriptor at ${desc.address}")

              val data = List.fill(descUsefullLength)(Random.nextInt(256))

              def dataAt(idx: Int) = if (idx < data.length) data(idx) else 0

              if (desc.direction) {
                for (i <- 0 until descUsefullLength by 4) {
                  ctrl.write(dataAt(i) | (dataAt(i + 1) << 8) | (dataAt(i + 2) << 16) | (dataAt(i + 3).toLong << 24), desc.address + 12 + i)
                }
              }

              desc.write()
              push(desc)

              while (descNotDone) {
                val descOffsetCpy = descOffset
                val packetLength = left min maxPacketSize
                val withError = Random.nextDouble() < 0.3 //TODO
                //              val withError = false
                val tokenError = withError && (Random.nextBoolean() || isochronous && desc.direction) //TODO
                val dataError = withError && !tokenError
                val handshakeError = withError && !tokenError
                val dataRef = data.slice(descOffsetCpy, descOffsetCpy + packetLength)

                frameCurrentUpdate()

                if (!desc.direction) {
                  desc.addCheck {
                    val dataDut = desc.readData(descOffsetCpy, packetLength)
                    assert(dataDut == dataRef)
                  }
                }

                def packetTask(): Unit = {
                  println(s"packetTask $tokenError $dataError $handshakeError")
                  if (!desc.direction) {
                    println(s"OUT ${simTime()} ${desc.address} $packetLength")
                    val pidRight = UsbPid.OUT
                    val pidWrong = UsbPid.allButSetup.filter(e => e != pidRight).randomPick()
                    if (!tokenError) {
                      usbAgent.emitBytes(List(pidRight | (~pidRight << 4), deviceAddress | (endpointId << 7), endpointId >> 1), crc16 = false, turnaround = true, ls = false, crc5 = true)
                      usbAgent.waitDone()
                    } else {
                      //                    4 match {
                      Random.nextInt(5) match { //TODO
                        case 0 => //PID check error
                          usbAgent.emitBytes(List(pidRight | (pidRight << 4), deviceAddress | (endpointId << 7), endpointId >> 1), crc16 = false, turnaround = true, ls = false, crc5 = true)
                        case 1 => //To many bytes
                          usbAgent.emitBytes(List(pidRight | (~pidRight << 4), deviceAddress | (endpointId << 7), endpointId >> 1, 0), crc16 = false, turnaround = true, ls = false, crc5 = true)
                        case 2 => //Wrong PID
                          usbAgent.emitBytes(List(pidWrong | (~pidWrong << 4), deviceAddress | (endpointId << 7), endpointId >> 1), crc16 = false, turnaround = true, ls = false, crc5 = true)
                        case 3 => //crc error
                          usbAgent.emitBytes(List(pidRight | (~pidRight << 4), deviceAddress | (endpointId << 7), endpointId >> 1), crc16 = false, turnaround = true, ls = false, crc5 = true, crcError = true)
                        case 4 => //wrong address
                          usbAgent.emitBytes(List(pidRight | (~pidRight << 4), deviceAddress + 1 | (endpointId << 7), endpointId >> 1), crc16 = false, turnaround = true, ls = false, crc5 = true, crcError = true)
                      }
                      usbAgent.waitDone()
                      return
                    }

                    if (!dataError) {
                      usbAgent.emitBytes(List(phase | (~phase << 4)) ++ dataRef, crc16 = true, turnaround = true, ls = false, crc5 = false)
                      usbAgent.waitDone()
                    } else {
                      //                    4 match {
                      Random.nextInt(5) match { //todo
                        case 0 => // wrong PID
                          val pidWrong = UsbPid.allButSetup.filter(e => (e & 7) != 3).randomPick(); usbAgent.emitBytes(List(pidWrong | (~pidWrong << 4)) ++ dataRef, crc16 = true, turnaround = true, ls = false, crc5 = false)
                        case 1 => // wrong phase
                          val pidWrong = phase ^ 0x8
                          usbAgent.emitBytes(List(pidWrong | (~pidWrong << 4)) ++ dataRef, crc16 = true, turnaround = true, ls = false, crc5 = false)
                          usbAgent.waitDone()
                          if (!isochronous) {
                            val (pid, payload) = usbAgent.rxBlocking()
                            assert(pid == UsbPid.ACK && payload.isEmpty)
                          }
                          return
                        case 2 => // pid error
                          usbAgent.emitBytes(List(phase | (phase << 4)) ++ dataRef, crc16 = true, turnaround = true, ls = false, crc5 = false)
                        case 3 => // crc error
                          usbAgent.emitBytes(List(phase | (~phase << 4)) ++ dataRef, crc16 = true, turnaround = true, ls = false, crc5 = false, crcError = true)
                        case 4 => // to much data
                          usbAgent.emitBytes(List(phase | (~phase << 4)) ++ List.fill((maxPacketSize min (descLength - descOffsetCpy)) + Random.nextInt(5) + 1)(Random.nextInt(256)), crc16 = true, turnaround = true, ls = false, crc5 = false)

                      }
                      usbAgent.waitDone()
                      return
                    }
                    if (!isochronous) {
                      val (pid, payload) = usbAgent.rxBlocking()
                      assert(pid == UsbPid.ACK && payload.isEmpty)
                    }
                    phaseToggle()
                    println(s"bye ${simTime()}")
                  } else {
                    println(s"IN ${simTime()} ${desc.address} $packetLength")

                    val pidRight = UsbPid.IN
                    val pidWrong = UsbPid.allButSetup.filter(_ != pidRight).randomPick()

                    if (!tokenError) {
                      usbAgent.emitBytes(List(pidRight | (~pidRight << 4), deviceAddress | (endpointId << 7), endpointId >> 1), crc16 = false, turnaround = true, ls = false, crc5 = true)
                      usbAgent.waitDone()
                    } else {
                      //                    1 match {
                      Random.nextInt(2) match { //TODO
                        case 0 => // pid error
                          usbAgent.emitBytes(List(pidRight | (pidRight << 4), deviceAddress | (endpointId << 7), endpointId >> 1), crc16 = false, turnaround = true, ls = false, crc5 = true)
                        case 1 => // wrong pid
                          usbAgent.emitBytes(List(pidWrong | (~pidWrong << 4), deviceAddress | (endpointId << 7), endpointId >> 1), crc16 = false, turnaround = true, ls = false, crc5 = true)
                      }
                      usbAgent.waitDone()
                      return
                    }
                    val (pid, payload) = usbAgent.rxBlocking()
                    assert(pid == phase)
                    assert(payload == dataRef)
                    if (!isochronous) {
                      if (!handshakeError) {
                        usbAgent.emitBytes(List(UsbPid.ACK | (~UsbPid.ACK << 4)), crc16 = false, turnaround = true, ls = false, crc5 = false)
                        usbAgent.waitDone()
                      } else {
                        //                      3 match {
                        Random.nextInt(4) match { //TODO
                          case 0 => //Wrong PID
                            val pidWrong = UsbPid.anyBut(UsbPid.ACK); usbAgent.emitBytes(List(pidWrong | (~pidWrong << 4)), crc16 = false, turnaround = true, ls = false, crc5 = false)
                          case 1 => //pid error
                            usbAgent.emitBytes(List(UsbPid.ACK | (UsbPid.ACK << 4)), crc16 = false, turnaround = true, ls = false, crc5 = false)
                          case 2 => // too much data
                            usbAgent.emitBytes(List(UsbPid.ACK | (~UsbPid.ACK << 4), Random.nextInt(256)), crc16 = false, turnaround = true, ls = false, crc5 = false)
                          case 3 => // no data
                            usbAgent.emitBytes(List(), crc16 = false, turnaround = true, ls = false, crc5 = false)

                        }
                        usbAgent.waitDone()
                        return
                      }
                    }
                    phaseToggle()
                  }
                }

                schedule(frameCurrent)(packetTask())

                if (!withError) {
                  left -= packetLength
                  descOffset += packetLength
                  if (packetLength != maxPacketSize) {
                    notEnough = false
                    descNotDone = false
                  }
                  if (desc.completionOnFull && descOffset == descLength) {
                    descNotDone = false
                  }
                }
              }
              desc.addCheck {
                assert(desc.code == 0)
                freeDescriptor(desc)

                if (sleepOnCompletion) {
                  for (i <- 0 to Random.nextInt(3)) {
                    frameCurrentUpdate()
                    schedule(frameCurrent) {
                      val doStall = Random.nextBoolean()

                      if (doStall) {
                        ctrl.write(endpointId | 0x10, Regs.HALT)
                        while ((ctrl.read(Regs.HALT) & 0x20) == 0) {}
                        val status = ctrl.read(endpointId * 4)
                        ctrl.write(status | (1 << Status.STALL), endpointId * 4)
                        ctrl.write(0, Regs.HALT)
                      }


                      Random.nextInt(2) match {
                        case 0 => {
                          usbAgent.emitBytes(List(UsbPid.IN | (~UsbPid.IN << 4), deviceAddress | (endpointId << 7), endpointId >> 1), crc16 = false, turnaround = true, ls = false, crc5 = true);
                          usbAgent.waitDone()
                          if (!isochronous) {
                            if (doStall) usbAgent.assertRxStall() else usbAgent.assertRxNak()
                          }
                        }
                        case 1 => {
                          val pid = UsbPid.OUT
                          val phaseTx = if (Random.nextBoolean()) UsbPid.DATA0 else UsbPid.DATA1
                          usbAgent.emitBytes(List(pid | (~pid << 4), deviceAddress | (endpointId << 7), endpointId >> 1), crc16 = false, turnaround = true, ls = false, crc5 = true);
                          usbAgent.waitDone()
                          usbAgent.emitBytes(List(phaseTx | (~phaseTx << 4)) ++ List.fill(Random.nextInt(65))(Random.nextInt(256)), crc16 = true, turnaround = true, ls = false, crc5 = false);
                          usbAgent.waitDone()
                          if (!isochronous) {
                            if (doStall) usbAgent.assertRxStall()
                            else if (phase != phaseTx) usbAgent.assertRxAck()
                            else usbAgent.assertRxNak()
                          }
                        }
                      }
                      if (doStall) {
                        val status = ctrl.read(endpointId * 4)
                        ctrl.write(status & ~(1 << Status.STALL), endpointId * 4)
                      }
                    }
                  }
                  frameCurrentUpdate()
                  schedule(frameCurrent) {
                    endpointThread.resume()
                  }
                }
              }
              if (sleepOnCompletion) endpointThread.suspend()
            }
          }
        }

        println(s"DONE Endpoint $endpointId")
      }

      dut.phyCd.waitSampling(10)

      def randomDelay() = dut.phyCd.waitSampling(Random.nextInt(100))
      while(schedules.nonEmpty || endpoints.exists(!_.isDone) || descQueues.exists(_.nonEmpty)){
        schedules.get(frameCounter) match {
          case Some(tasks) => {
            tasks.foreach(_.apply())
          }
          case None =>
        }
        schedules.remove(frameCounter)
        randomDelay()
        lock.lock()
        newFrame()
        lock.unlock()
        randomDelay()
      }

      dut.phyCd.waitSampling(100)
    }
  }
}
