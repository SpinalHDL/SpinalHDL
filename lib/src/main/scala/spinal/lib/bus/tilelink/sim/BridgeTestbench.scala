package spinal.lib.bus.tilelink.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink._
import spinal.lib.sim._

class BridgeTestbench(m : Bus, s : Bus, cd : ClockDomain) {
  cd.forkStimulus(10)
  implicit val idAllocator = new IdAllocator(DebugId.width)
  implicit val idCallback = new IdCallback

  val outputAgent = new MemoryAgent(s, cd)
  val globalMem = SparseMemory(outputAgent.mem.seed)

  val inputMapping = Endpoint(
    chunks = List(
      Chunk(s.p.node.m.toSupport().transfers, SizeMapping(0, 1l << m.p.addressWidth), 0)
    ),
    model = globalMem
  )
  val inputSpec = MasterSpec(
    bus = m,
    cd = cd,
    endpoints = Seq(inputMapping)
  )

  val inputAgent = new MasterAgent(inputSpec.bus, inputSpec.cd)
  val inputTester = new MasterTester(inputSpec, inputAgent)

  def testPerSource(count : Int): this.type ={
    inputTester.startPerSource(count)
    inputTester.join()
    this
  }
}