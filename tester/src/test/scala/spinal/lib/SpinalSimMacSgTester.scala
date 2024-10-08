package spinal.lib

import spinal.core.ClockDomain
import spinal.core.sim._
import spinal.lib.bus.tilelink
import spinal.lib.com.eth.{PhyParameter, SpinalSimMacTester}
import spinal.lib.com.eth.sg.{MacSg, MacSgParam}
import spinal.lib.sim.{MemoryRegionAllocator, StreamMonitor, StreamReadyRandomizer}
import spinal.lib.system.dma.sg2
import spinal.lib.system.dma.sg2._
import spinal.lib.system.dma.sg2.sim.{SimCtrl, SimReadOnlyDescriptor}
import spinal.tester.code.SpinalAnyFunSuite

import scala.collection.mutable.{ArrayBuffer, Queue}

class SpinalSimMacSgTester extends SpinalAnyFunSuite{
  test("default"){
    val phyParam = new PhyParameter(
      txDataWidth = 8,
      rxDataWidth = 8
    )
    val txDmaParam = DmaSgReadOnlyParam(
      addressWidth = 32,
      dataWidth = 64,
      blockSize = 64,
      bufferBytes = 2048,
      pendingSlots = 4
    )
    val ctrlParam = MacSg.getCtrlParam()
    val compiled = SimConfig.withFstWave.compile(
      new MacSg(
        p = new MacSgParam(
          phyParam   = phyParam,
          txDmaParam = txDmaParam,
          txBufferBytes = 2000
        ),
        ctrlParam  = ctrlParam,
        txMemParam = txDmaParam.getM2sParameter(null).toBusParameter(),
        ctrlCd     = ClockDomain.external("ctrl"),
        txCd       = ClockDomain.external("tx"),
        rxCd       = ClockDomain.external("rx")
      )
    )
    compiled.doSim(seed = 42){dut =>
      SimTimeout(100000*10)
      dut.ctrlCd.forkStimulus(10)
      dut.txCd.forkStimulus(21)
//      dut.rxCd.forkStimulus(26)

      dut.txCd.onSamplings {
        dut.io.phy.tx.ready #= simRandom.nextBoolean()
      }

      val mem = new tilelink.sim.MemoryAgent(bus = dut.io.txMem, dut.ctrlCd)(null)
      val ctrlAgent = new tilelink.sim.MasterAgent(dut.io.ctrl, dut.ctrlCd)(null)
      val ctrl = new SimCtrl(ctrlAgent, 0x100)

      case class TxFragment(data : Byte, error : Boolean)
      case class TxPacket(data : Array[Byte]){
        val ref = Queue[TxFragment]()
        def push(data : Byte) = ref += TxFragment(data.toByte, false)
        val frame = SpinalSimMacTester.dataToFrameByte(data)
        frame.foreach(push)
//        for(i <- frame.size until Math.max(frame.size, 512+8)) ref += TxFragment(0x0F, true)
      }

      val tasks = Queue[TxPacket]()

      var txBusy = false
      dut.txCd.onSamplings{
        val tx = dut.io.phy.tx
        if(tx.valid.toBoolean && tx.ready.toBoolean){
          val data = tx.data.toInt.toByte
//          val error = tx.error.toBoolean
          val ref = tasks.head.ref.dequeue()
          assert(data == ref.data)
//          assert(error == ref.error)
          if(tasks.head.ref.isEmpty){
            tasks.dequeue()
          }
        }
      }


      val allocator = new MemoryRegionAllocator(0, 0x10000)
      for(_ <- 0 until 10) {
        val chain = ArrayBuffer[sg2.sim.SimReadOnlyDescriptor]()
        val descriptorSizeMax = 1500
        val packetSize = simRandom.nextInt(1000)+1
        var left = packetSize
        val dataBuffer = ArrayBuffer[Byte]()
        while(left != 0){
          val d = new SimReadOnlyDescriptor(allocator.allocateAligned(32, 32).base.toLong)
          val size = (simRandom.nextInt(descriptorSizeMax) + 1) min left
          left -= size
          val buffer = allocator.allocate(size)
          d.from = buffer.base.toLong
          d.controlBytes = size
          d.controlLast = left == 0
          val data = Array.fill[Byte](size)(simRandom.nextInt().toByte)
          mem.mem.write(d.from, data)
          dataBuffer ++= data
          chain += d
        }

        val task = new TxPacket(dataBuffer.toArray)
        tasks += task

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
        for(d <- chain){
          allocator.free(d.physicalAddress)
          if(d != chain.last) allocator.free(d.from)
        }
//        dut.pushCd.waitSampling(10000)
      }
      dut.ctrlCd.waitSampling(10000)
      while(tasks.nonEmpty){
        dut.ctrlCd.waitSampling(100)
      }

      dut.ctrlCd.waitSampling(1000)

    }
  }
}
