package spinal.lib.misc.pipeline

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.pipeline._

class TopLevel extends Component {
  val io = new Bundle{
    val up = slave Stream (UInt(16 bits))
    val down = master Stream (UInt(16 bits))
  }

  // Let's define 3 Nodes for our pipeline
  val n0, n1, n2 = Node()

  // Let's connect those nodes by using simples registers
  val s01 = StageConnector(n0, n1)
  val s12 = StageConnector(n1, n2)

  // Let's define a few stageable things that can go through the pipeline
  val VALUE = Stageable(UInt(16 bits))
  val RESULT = Stageable(UInt(16 bits))

  // Let's bind io.up to n0
  io.up.ready := n0.ready
  n0.valid := io.up.valid
  n0(VALUE) := io.up.payload

  // Let's do some processing on n1
  n1(RESULT) := n1(VALUE) + 0x1200

  // Let's bind n2 to io.down
  n2.ready := io.down.ready
  io.down.valid := n2.valid
  io.down.payload := n2(RESULT)

  // Let's ask the builder to generate all the required hardware
  Builder(s01, s12)
}

class TopLevel2 extends Component {
  val VALUE = Stageable(UInt(16 bits))

  val io = new Bundle{
    val up = slave Stream(VALUE)  //VALUE can also be used as a HardType
    val down = master Stream(VALUE)
  }

  // Let's define 3 Nodes for our pipeline
  val n0, n1, n2 = Node()

  // Let's connect those nodes by using simples registers
  val s01 = StageConnector(n0, n1)
  val s12 = StageConnector(n1, n2)

  // Let's bind io.up to n0
  n0.arbitrateFrom(io.up)
  n0(VALUE) := io.up.payload

  // Let's do some processing on n1
  val RESULT = n1.insert(n1(VALUE) + 0x1200)

  // Let's bind n2 to io.down
  n2.arbitrateTo(io.down)
  io.down.payload := n2(RESULT)

  // Let's ask the builder to generate all the required hardware
  Builder(s01, s12)
}


object PipelineDemo1 extends App {
  SimConfig.withFstWave.compile(new TopLevel2).doSim{ dut =>
    dut.clockDomain.forkStimulus(10)
    dut.io.down.ready #= true
    dut.clockDomain.waitSampling(5)

    // Push one transaction
    dut.io.up.valid #= true
    dut.io.up.payload #= 0x0042
    dut.clockDomain.waitSamplingWhere(dut.io.up.ready.toBoolean)
    dut.io.up.valid #= false

    dut.clockDomain.waitSampling(10)
    simSuccess()
  }
}
