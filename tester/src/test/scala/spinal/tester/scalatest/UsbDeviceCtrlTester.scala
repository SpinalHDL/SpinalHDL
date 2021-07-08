package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.bmb.BmbParameter
import spinal.lib.bus.bmb.sim.BmbDriver
import spinal.lib.com.usb.ohci.UsbPid
import spinal.lib.com.usb.phy.UsbDevicePhyNative
import spinal.lib.com.usb.sim.UsbLsFsPhyAbstractIoAgent
import spinal.lib.com.usb.udc.{UsbDeviceCtrl, UsbDeviceCtrlParameter}
import spinal.lib.sim.MemoryRegionAllocator

import scala.collection.mutable
import scala.util.Random

case class UsbDeviceCtrlTesterTop(fsRatio : Int) extends Component {
  val ctrl = new UsbDeviceCtrl(
    p = UsbDeviceCtrlParameter(
      addressWidth = 15
    ),
    bmbParameter = BmbParameter(
      addressWidth = 17,
      dataWidth = 32,
      sourceWidth = 0,
      contextWidth = 0,
      lengthWidth = 2
    )
  )

  val phy = new UsbDevicePhyNative(fsRatio = fsRatio, sim = true)
  ctrl.io.phy <> phy.io.ctrl

  val bmb = ctrl.io.ctrl.toIo()
  val usb = phy.io.usb.toIo()
  val power = phy.io.power.toIo()
  val interrupts = ctrl.io.interrupt.toIo()

  ctrl.regs.address.simPublic()
}

class UsbDeviceCtrlTester extends AnyFunSuite{
  test("miaou"){
    SimConfig.withFstWave.compile(UsbDeviceCtrlTesterTop(
      fsRatio = 4
    )).doSim(seed = 42){ dut =>
      dut.clockDomain.forkStimulus(1e12/12e6/dut.fsRatio toLong)
      val usbAgent = new UsbLsFsPhyAbstractIoAgent(dut.usb, dut.clockDomain, dut.fsRatio)

      val ctrl = BmbDriver(dut.bmb, dut.clockDomain)

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
      ctrl.write(deviceAddress, UsbDeviceCtrl.Regs.ADDRESS)
      assert(dut.ctrl.regs.address.toInt == deviceAddress)

      dut.power #= true
      dut.clockDomain.waitSampling(100)
      usbAgent.connect(false)
      dut.clockDomain.waitSampling(100)
      usbAgent.emitReset()
      usbAgent.waitDone()
      assert(dut.ctrl.regs.address.toInt == 0)


      dut.clockDomain.waitSampling(100)
      newFrame()
      assert(ctrl.read(UsbDeviceCtrl.Regs.FRAME) == frameCounter)
      ctrl.write(deviceAddress, UsbDeviceCtrl.Regs.ADDRESS)


      val schedules = mutable.LinkedHashMap[Int, mutable.Queue[() => Unit]]()
      def schedule(at : Int)(body : => Unit): Unit ={
        schedules.getOrElseUpdate(at, mutable.Queue[() => Unit]()) += (() => body)
      }

      val lock = SimMutex()
      val alloc = MemoryRegionAllocator(0, 0x7FFF)
      alloc.allocateOn(0, 16*4)

      class Descriptor(val address : Int){
        var offset = 0
        var code = 0
        var next = 0
        var length = 0
        var direction = false
        var interrupt = false
        var withZeroLengthEnd = false
        var frame = 0

        def write(): Unit ={
          ctrl.write((offset << 0) | (code << 16), address + 0)
          ctrl.write((next << 0) | (length << 16), address + 4)
          ctrl.write((frame << 0) | (direction.toInt << 16) | (interrupt.toInt << 17) | (withZeroLengthEnd.toInt << 18), address + 8)
        }

        def writeW1(): Unit ={
          ctrl.write((next << 0) | (length << 16), address + 4)
        }

        def read(): Unit ={
          val v = ctrl.read(0).toInt
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

      fork {
        while(true){
          dut.clockDomain.waitSamplingWhere(dut.interrupts.toBoolean)
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
            }
            mask &= ~(1 << int)
          }
        }
      }

      //TODO
      val transferPerEndpoint = 10
      val endpointCount = 16
      val endpoints = for(endpointId <- 0 until endpointCount) yield fork {
        val maxPacketSize = Random.nextInt(56)+8//List(8,16,32,64).randomPick()
        var frameCurrent = frameCounter
        var descs = mutable.Queue[Descriptor]()



        def newDescriptor(size : Int) = {
          val m = alloc.allocateAligned(12+size, 16)
          println(s"MEMORY FREE : ${alloc.freeSize}")
          val d = new Descriptor(m.base.toInt)
          descQueues(endpointId) += d
          d
        }

        def freeDescriptor(desc : Descriptor) = { //TODO use me
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



        ctrl.write((maxPacketSize << 22) | 1, endpointId*4)

        val endpointThread = simThread
        for(_ <- 0 until transferPerEndpoint) {
          var phase = UsbPid.DATA0

          def phaseToggle() = phase = if (phase == UsbPid.DATA0) UsbPid.DATA1 else UsbPid.DATA0
          def frameCurrentUpdate(): Unit ={
            frameCurrent = frameCurrent.max(frameCounter + 1)
            if (Random.nextDouble() < 0.3) frameCurrent += Random.nextInt(4) + 1
          }
          val length = Random.nextInt(maxPacketSize*4)
          var left = length
          var notEnough = true

          while (notEnough) {
            val descLength = (maxPacketSize * (1 + Random.nextInt(4))) min left
            val desc = newDescriptor(descLength)
            var descOffset = 0
            val sleepOnCompletion = Random.nextDouble() < 0.2 //TODO

            desc.offset = descOffset
            desc.code = 0xF
            desc.next = 0
            desc.length = descLength
            desc.direction = Random.nextBoolean()
            desc.interrupt = true //TODO
            desc.withZeroLengthEnd = desc.length == left && left % maxPacketSize == 0
            desc.frame = 0

            println(s"Descriptor at ${desc.address}")

            val data = List.fill(descLength)(Random.nextInt(256))
            def dataAt(idx : Int) = if(idx < data.length) data(idx) else 0
            if(desc.direction) {
              for (i <- 0 until descLength + 3 by 4) {
                ctrl.write(dataAt(i) | (dataAt(i + 1) << 8) | (dataAt(i + 2) << 16) | (dataAt(i + 3).toLong << 24), desc.address + 12 + i)
              }
            }

            desc.write()
            push(desc)

            while ((descOffset == 0 || descOffset != descLength) && notEnough) {
              val descOffsetCpy = descOffset
              val packetLength = left min maxPacketSize
              left -= packetLength

              lock.lock()
              frameCurrentUpdate()

              val dataRef = data.slice(descOffsetCpy, descOffsetCpy + packetLength)
              if(!desc.direction) {
                desc.addCheck {
                  val dataDut = desc.readData(descOffsetCpy, packetLength)
                  assert(dataDut == dataRef)
                }
                schedule(frameCurrent) {
                  println(s"Hi ${simTime()}")
                  usbAgent.emitBytes(List(UsbPid.SETUP | (~UsbPid.SETUP << 4), deviceAddress | (endpointId << 7), endpointId >> 1), crc16 = false, turnaround = true, ls = false, crc5 = true)
                  usbAgent.waitDone()
                  usbAgent.emitBytes(List(phase | (~phase << 4)) ++ dataRef, crc16 = true, turnaround = true, ls = false, crc5 = false)
                  usbAgent.waitDone()
                  val (pid, payload) = usbAgent.rxBlocking()
                  assert(pid == UsbPid.ACK && payload.isEmpty)
                  phaseToggle()
                  println(s"bye ${simTime()}")
                }
              } else {
              schedule(frameCurrent) {
                  usbAgent.emitBytes(List(UsbPid.IN | (~UsbPid.IN << 4), deviceAddress | (endpointId << 7), endpointId >> 1), crc16 = false, turnaround = true, ls = false, crc5 = true)
                  usbAgent.waitDone()
                  val (pid, payload) = usbAgent.rxBlocking()
                  assert(payload == dataRef)
                  usbAgent.emitBytes(List(UsbPid.ACK | (~UsbPid.ACK << 4)), crc16 = false, turnaround = true, ls = false, crc5 = false)
                  usbAgent.waitDone()
                  phaseToggle()
                }
              }
              lock.unlock()


              descOffset += packetLength
              if(packetLength != maxPacketSize){
                notEnough = false
              }
            }
            desc.addCheck{
              assert(desc.code == 0)
              freeDescriptor(desc)

              if(sleepOnCompletion){
                for(i <- 0 to Random.nextInt(3)) {
                  lock.lock()
                  frameCurrentUpdate()
                  schedule(frameCurrent) {
                    println("MASDASDASD")
                    Random.nextInt(2) match {
                      case 0 => {
                        usbAgent.emitBytes(List(UsbPid.IN | (~UsbPid.IN << 4), deviceAddress | (endpointId << 7), endpointId >> 1), crc16 = false, turnaround = true, ls = false, crc5 = true); usbAgent.waitDone()
                        usbAgent.assertRxNak()
                      }
                      case 1 => {
                        val pid = if(Random.nextBoolean()) UsbPid.SETUP else UsbPid.OUT
                        val phase = if(Random.nextBoolean()) UsbPid.DATA0 else UsbPid.DATA1
                        usbAgent.emitBytes(List(pid | (~pid << 4), deviceAddress | (endpointId << 7), endpointId >> 1), crc16 = false, turnaround = true, ls = false, crc5 = true); usbAgent.waitDone() }
                        usbAgent.emitBytes(List(phase | (~phase << 4)) ++ List.fill(Random.nextInt(65))(Random.nextInt(256)), crc16 = true, turnaround = true, ls = false, crc5 = false); usbAgent.waitDone()
                        usbAgent.assertRxNak()
                    }
                  }
                  lock.unlock()
                }
                lock.lock()
                frameCurrentUpdate()
                schedule(frameCurrent) {
                  endpointThread.resume()
                }
                lock.unlock()
              }
            }
            if(sleepOnCompletion) endpointThread.suspend()
          }
        }

        println(s"DONE Endpoint $endpointId")
      }

      dut.clockDomain.waitSampling(10)

      def randomDelay() = dut.clockDomain.waitSampling(Random.nextInt(100))
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

      dut.clockDomain.waitSampling(100)
    }
  }
}
