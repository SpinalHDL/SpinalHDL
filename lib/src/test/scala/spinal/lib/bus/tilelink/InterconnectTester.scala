package spinal.lib.bus.tilelink

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import spinal.core.fiber.{Elab, hardFork}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.sim._
import spinal.lib._
import spinal.lib.sim.SparseMemory

class MasterBus(p : M2sParameters)(implicit ic : Interconnect) extends Area{
  val node = ic.createMaster()

  val logic = Elab build new Area{
    node.m2s.parameters.load(p)
    node.m2s.setProposedFromParameters()
    slave(node.bus)
  }
}

class SlaveBus(m2sSupport : M2sSupport)(implicit ic : Interconnect) extends Area{
  val node = ic.createSlave()
  node.s2m.parameters.load(
    S2mParameters.simple(this)
  )
  node.m2s.supported.loadAsync(
    m2sSupport
  )
  hardFork(master(node.bus))
}



class InterconnectTester extends AnyFunSuite{
  test("Simple"){
    SimConfig.withFstWave.compile(new Component{
      implicit val interconnect = new Interconnect()

      val m0 = new MasterBus(
        M2sParameters(
        addressWidth = 32,
        dataWidth    = 32,
        masters = List(M2sAgent(
          name = this,
          mapping = List(M2sSource(
            id = SizeMapping(0, 4),
            emits = M2sTransfers(
              get = SizeRange.upTo(0x40),
              putFull = SizeRange.upTo(0x40)
            )
          ))
        ))
      ))

      val b0,b1 = interconnect.createNode()
      b0 << m0.node
      b1 at(0x30000, 0x1000) of b0

      val s0,s1 = new SlaveBus(
        M2sSupport(
          transfers = M2sTransfers(
            get     = SizeRange.upTo(0x1000),
            putFull = SizeRange.upTo(0x1000)
          ),
          dataWidth    = 32,
          addressWidth = 8,
          allowExecute = false
        )
      )
      s0.node at 0x200 of b1
      s1.node at 0x400 of b1

    }).doSim{dut =>
      dut.clockDomain.forkStimulus(10)
      val m0 = new MasterAgent(dut.m0.node.bus, dut.clockDomain)
      for(node <- List(dut.s0.node, dut.s1.node)) new SlaveAgent(node.bus, dut.clockDomain){
        val mem = SparseMemory()
        override def onGet(source: Int, address: Long, bytes: Int) = {
          accessAckData(source, mem.readBytes(address, bytes))
        }
      }
      val data = m0.get(1, 0x30410, 4)()
    }
  }
}
