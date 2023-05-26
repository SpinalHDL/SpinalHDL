package spinal.lib.bus.tilelink.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink._
import spinal.lib.sim._

class BridgeTestbench(m : Bus, s : Bus, cd : ClockDomain) {
  cd.forkStimulus(10)
  val outputAgent = new SlaveRam(s, cd)
  val globalMem = SparseMemory(outputAgent.mem.seed)

  val inputMapping = Mapping(
    allowed = s.p.node.m.toSupport(),
    mapping = List(SizeMapping(0, 1l << m.p.addressWidth)),
    model = globalMem
  )
  val inputSpec = MasterSpec(
    bus = m,
    cd = cd,
    mapping = Seq(inputMapping)
  )

  val inputAgent = new MasterAgent(inputSpec.bus, inputSpec.cd)
  val inputTester = new MasterTester(inputSpec, inputAgent)

  def testPerSource(count : Int): this.type ={
    inputTester.startPerSource(count)
    inputTester.join()
    this
  }
}