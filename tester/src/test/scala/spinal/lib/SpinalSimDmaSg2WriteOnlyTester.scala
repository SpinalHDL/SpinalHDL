package spinal.lib

import spinal.core.ClockDomain
import spinal.core.sim._
import spinal.lib.bus.bsb.sim.{BsbDriver, BsbPacket}
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.Opcode
import spinal.lib.bus.tilelink.sim.{MonitorSubscriber, TransactionA}
import spinal.lib.sim.{MemoryRegionAllocator, StreamMonitor, StreamReadyRandomizer}
import spinal.lib.system.dma.sg2
import spinal.lib.system.dma.sg2._
import spinal.lib.system.dma.sg2.sim.{SimCtrl, SimReadOnlyDescriptor, SimWriteOnlyDescriptor}
import spinal.tester.code.SpinalAnyFunSuite
import scala.collection.Seq
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Queue}

class SpinalSimDmaSg2WriteOnlyTester extends SpinalAnyFunSuite{
  test("default"){
    val p = DmaSgWriteOnlyParam(
      addressWidth = 32,
      dataWidth = 64,
      blockSize = 64,
      bufferBytes = 128,
      bsbDataBytes = 4,
      pendingSlots = 3
    )
    val ctrlParam = p.getCtrlParam()

    val compiled = SimConfig.compile(
      new DmaSgWriteOnlyComp(p, ctrlParam)
    )
    compiled.doSim(seed = 42){dut =>
      SimTimeout(10000000*10)
      val cd = dut.clockDomain
      cd.forkStimulus(10)

      def log(that : String) = print(that)
      val mem = new tilelink.sim.MemoryAgent(bus = dut.io.mem, cd)(null)
      val ctrlAgent = new tilelink.sim.MasterAgent(dut.io.ctrl, cd)(null)
      val ctrl = new SimCtrl(ctrlAgent, 0x0)
      ctrl.enableIrq()
      ctrl.setIrq(idle = true)

      val expectedWrites = mutable.LinkedHashMap[Long, Byte]()
      val allowedWrites = mutable.LinkedHashSet[Long]()
      mem.monitor.add(new MonitorSubscriber {
        override def onA(a: TransactionA) = {
          a.opcode match {
            case Opcode.A.PUT_FULL_DATA | Opcode.A.PUT_PARTIAL_DATA => {
              val base = a.address.toInt
              for(i <- a.data.indices if a.mask(i)){
                val addr = base + i
                expectedWrites.get(addr) match {
                  case Some(ref) => assert(a.data(i) == ref); expectedWrites.remove(addr)
                  case None => assert(allowedWrites.contains(addr), "Undesired write"); allowedWrites.remove(addr)
                }
              }
            }
            case _ =>
          }
        }
      })

      case class Task(descriptor : SimWriteOnlyDescriptor, data : Array[Byte], last : Boolean, address : Long){
        var ptr = 0
        def popData(): Byte = {
          ptr += 1
          data(ptr-1)
        }
        def isEmpty = ptr == data.size
      }

      val tasks = Queue[Task]()
      val bsbDriver = new BsbDriver(dut.io.bsb, cd)
      periodicaly(10000){
        bsbDriver.sd.setFactor(simRandom.nextFloat())
        mem.driver.driver.randomizeStallRate()
      }

      val allocator = new MemoryRegionAllocator(0, 0x1000000)
      for(_ <- 0 until 1000) {
        val chain = ArrayBuffer[sg2.sim.SimWriteOnlyDescriptor]()
//        for (i <- 0 until simRandom.nextInt(10)) {
        val descriptorCount = simRandom.nextInt(10)
        val packets = ArrayBuffer[BsbPacket]()
        log("Ref :\n")
        if(simRandom.nextFloat() < 0.1){ //Overflow
          val d = new SimWriteOnlyDescriptor(allocator.allocateAligned(32, 32).base.toLong)
          val bufferSize = simRandom.nextInt(256) + 1
          val buffer = allocator.allocate(bufferSize)
          d.to = buffer.base.toLong
          d.controlBytes = buffer.size.toInt
          val size = bufferSize+1+simRandom.nextInt(32)
          val data = Array.fill[Byte](size)(simRandom.nextInt().toByte)
          val dataPartial = data.take(bufferSize)
          val task = new Task(d, dataPartial, false, d.to)
          for (i <- dataPartial.indices) expectedWrites(d.to + i) = data(i)
          for (i <- 0 to 3) allowedWrites += d.physicalAddress + DmaSgWriteOnly.statusAt + i
          tasks += task
          chain += d
          log(data.size + " -> overflow\n")
          packets += new BsbPacket(0, 0, data, true)
        } else {
          if(simRandom.nextFloat() < 1.1){
            bsbDriver.push(new BsbPacket(0, 0, Array.fill[Byte](1 + simRandom.nextInt(16))(simRandom.nextInt().toByte), false))
            while(bsbDriver.sources(0).nonEmpty) cd.waitSampling(10)
            bsbDriver.push(new BsbPacket(0, 0, Array.fill[Byte](simRandom.nextInt(64))(simRandom.nextInt().toByte), true))
          }

          val packetData = ArrayBuffer[Byte]()
          for (i <- 0 until descriptorCount) { //Regular
            val d = new SimWriteOnlyDescriptor(allocator.allocateAligned(32, 32).base.toLong)
            val bufferSize = simRandom.nextInt(256) + 1
            val buffer = allocator.allocate(bufferSize) //allocator.allocate(simRandom.nextInt(256)+1)
            d.to = buffer.base.toLong
            d.controlBytes = buffer.size.toInt
            d.controlIrqLast = true
            val isLast = i == descriptorCount - 1 || simRandom.nextBoolean()
            val size = if (isLast) simRandom.nextInt(bufferSize) else bufferSize
            val data = Array.fill[Byte](size)(simRandom.nextInt().toByte)
            val task = new Task(d, data, isLast, d.to)
            for (i <- data.indices) expectedWrites(d.to + i) = data(i)
            for (i <- 0 to 3) allowedWrites += d.physicalAddress + DmaSgWriteOnly.statusAt + i
            if (size == 0) {
              //Needed to ensure we do not merge zero byte packets
              packets += new BsbPacket(0, 0, packetData.clone(), false)
              packetData.clear()
            }
            packetData ++= data
            tasks += task
            chain += d
            log(data.size + " -> ")
            if (isLast) {
              log("last\n")
              packets += new BsbPacket(0, 0, packetData.clone(), isLast)
              packetData.clear()
            }
          }
        }

        val tail = new SimWriteOnlyDescriptor(allocator.allocateAligned(32, 32).base.toLong)
        chain += tail

        for ((self, next) <- (chain, chain.tail).zipped) {
          self.next = next.physicalAddress
          self.statusCompleted = false
        }
        chain.last.statusCompleted = true

        for (d <- chain) {
          d.write(mem.mem)
        }

        ctrl.setNext(chain.head.physicalAddress)
        ctrl.start()
        packets.foreach(bsbDriver.push)


//        dut.clockDomain.waitSampling(200)
//        ctrl.stop()
//        println("Stop done")
//        dut.clockDomain.waitSampling(100)
//        simSuccess()

        cd.waitSamplingWhere(dut.io.interrupt.toBoolean)
//        if(dut.io.interrupt.toBoolean){
        ctrl.setIrq(idle=true)
//        }
        log("Got :\n")
        for(d <- chain.dropRight(1)){
          d.read(mem.mem)
          log(d.statusBytes + " -> ")
          if(d.statusLast) log(" last\n")
          val t = tasks.dequeue()
          assert(t.descriptor == d)
          assert(t.last == d.statusLast)
          assert(t.data.sameElements(mem.mem.readBytes(d.to, t.data.size)))
          assert(d.statusCompleted)
          allocator.free(d.physicalAddress)
          if(d.controlBytes != 0) allocator.free(d.to)
        }
        log("\n\n")
        while(bsbDriver.sources(0).nonEmpty) cd.waitSampling(10)
//        cd.waitSampling(1000)
      }
      println(simTime())
      cd.waitSampling(10000)
//      while(tasks.nonEmpty){
//        cd.waitSampling(100)
//      }

      cd.waitSampling(1000)
      assert(expectedWrites.isEmpty)
      assert(allowedWrites.isEmpty)
      assert(tasks.isEmpty)
    }
  }
}
