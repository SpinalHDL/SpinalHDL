package spinal.lib

import spinal.core.ClockDomain
import spinal.lib.system.dma.sg2._
import spinal.lib.system.dma.sg2
import spinal.tester.code.SpinalAnyFunSuite
import spinal.lib.bus.tilelink
import spinal.core.sim._
import spinal.lib.bus.tilelink.DebugId
import spinal.lib.bus.tilelink.sim.IdAllocator
import spinal.lib.sim.{MemoryRegionAllocator, StreamMonitor, StreamReadyRandomizer}
import spinal.lib.system.dma.sg2.sim.{SimCtrl, SimReadOnlyDescriptor}

import java.nio.ByteBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer

class SpinalSimDmaSg2ReadOnlyTester extends SpinalAnyFunSuite{
  test("default"){
    val p = DmaSgReadOnlyParam(
      addressWidth = 32,
      dataWidth = 64,
      blockSize = 64,
      bufferBytes = 512,
      pendingSlots = 4
    )
    val ctrlParam = p.getCtrlParam()

    val compiled = SimConfig.withFstWave.compile(
      new DmaSgReadOnlyComp(p, ctrlParam, ClockDomain.external("push"), ClockDomain.external("pop"))
    )
    compiled.doSim(seed = 42){dut =>
      SimTimeout(10000000*10)
      dut.pushCd.forkStimulus(10)
      dut.popCd.forkStimulus(21)

      val mem = new tilelink.sim.MemoryAgent(bus = dut.io.mem, dut.pushCd)(null)
      val ctrlAgent = new tilelink.sim.MasterAgent(dut.io.ctrl, dut.pushCd)(null)
      val ctrl = new SimCtrl(ctrlAgent, 0x0)
      ctrl.setIrq(idle = true)

      case class Task(descriptor : SimReadOnlyDescriptor, data : Array[Byte], last : Boolean){
        var ptr = 0
        def popData(): Byte = {
          ptr += 1
          data(ptr-1)
        }
        def isEmpty = ptr == data.size
      }

      val tasks = Queue[Task]()
      val bspReady = StreamReadyRandomizer(dut.io.bsb, dut.popCd)
      periodicaly(10000){
        bspReady.setFactor(simRandom.nextFloat())
      }
      val bsb = StreamMonitor(dut.io.bsb, dut.popCd){d =>
        val mask = d.mask.toInt
        val data = d.data.toBytes
        var gotLast = false
        for(i <- 0 until p.dataBytes){
          if((mask & (1 << i)) != 0){
            val ref = tasks.head.popData()
            val dut = data(i)
            assert(dut == ref)
          }
        }
        if(tasks.head.isEmpty) {
          assert(d.last.toBoolean == tasks.head.last)
          tasks.dequeue()
        } else {
          assert(d.last.toBoolean == false)
        }
      }


      val allocator = new MemoryRegionAllocator(0, 0x10000)
      for(_ <- 0 until 100) {
        val chain = ArrayBuffer[sg2.sim.SimReadOnlyDescriptor]()
        for (i <- 0 until simRandom.nextInt(10)) {
          val d = new SimReadOnlyDescriptor(allocator.allocateAligned(32, 32).base.toLong)
          val buffer = allocator.allocate(simRandom.nextInt(256)+1)
          d.from = buffer.base.toLong
          d.controlBytes = buffer.size.toInt
          d.controlLast = simRandom.nextBoolean()
          d.controlIrq = simRandom.nextBoolean()
          val data = Array.fill[Byte](d.controlBytes)(simRandom.nextInt().toByte)
          mem.mem.write(d.from, data)
          val task = new Task(d, data, d.controlLast)
          tasks += task
          chain += d
        }

        val tail = new SimReadOnlyDescriptor(allocator.allocateAligned(32, 32).base.toLong)
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

        while (ctrl.busy()) {}
        if(dut.io.interrupt.toBoolean){
          ctrl.setIrq(delay = Some(1))
        }
        for(d <- chain){
          d.read(mem.mem)
          assert(d.statusCompleted)
          allocator.free(d.physicalAddress)
          if(d != chain.last) allocator.free(d.from)
        }
//        dut.pushCd.waitSampling(1000)
      }
      println(simTime())
      dut.pushCd.waitSampling(10000)
      while(tasks.nonEmpty){
        dut.pushCd.waitSampling(100)
      }

      dut.pushCd.waitSampling(1000)

    }
  }
}
