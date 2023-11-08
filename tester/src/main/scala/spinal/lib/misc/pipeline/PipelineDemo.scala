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


class TopLevel3 extends Component {
  val ADDRESS = Stageable(UInt(8 bits))

  val io = new Bundle{
    val up = slave Flow(ADDRESS)  //VALUE can also be used as a HardType
  }

  // Let's define 3 Nodes for our pipeline
  val c0, c1, c2 = CtrlConnector()

  // Let's connect those nodes by using simples registers
  val s01 = StageConnector(c0.down, c1.up)
  val s12 = StageConnector(c1.down, c2.up)

  c0.up.arbitrateFrom(io.up)
  c0(ADDRESS) := io.up.payload
  val ram = Mem.fill(256)(UInt(16 bits))
  val READ_DATA = c1.up.insert(ram.readSync(c0(ADDRESS), c0.down.isFiring, readUnderWrite = writeFirst))
  val WRITE_DATA = c2.insert(c2(READ_DATA)+1)
  ram.write(c2(ADDRESS), c2(WRITE_DATA), c2.down.isFiring)
  ram.generateAsBlackBox()
  c2.down.setAlwaysReady()

  for(c <- List(c1, c2)){
    c0.haltWhen(c.up.isValid && c(ADDRESS) === c0(ADDRESS))
  }
//  val bypass = new Area{
//    val data = RegNext(c2(WRITE_DATA))
//
//    def addBypassOn(addressCtrl : Node, dataCtrl: CtrlConnector): Unit = {
//      val hit = RegNext(c2.down.isValid && addressCtrl(ADDRESS) === c2(ADDRESS)) init(False)
//      when(hit) {
//        dataCtrl.bypass(READ_DATA) := data
//      }
//    }
//
//    addBypassOn(c0.down, c1)
//    addBypassOn(c1.down, c2)
//  }



  // Let's ask the builder to generate all the required hardware
  Builder(s01, s12, c0, c1, c2)
}


object PipelineDemo1 extends App {
  SimConfig.withFstWave.compile(new TopLevel3).doSim{ dut =>
    dut.clockDomain.forkStimulus(10)
//    dut.io.down.ready #= true
//    dut.clockDomain.waitSampling(5)
//
//    // Push one transaction
//    dut.io.up.valid #= true
//    dut.io.up.payload #= 0x0042
//    dut.clockDomain.waitSamplingWhere(dut.io.up.ready.toBoolean)
//    dut.io.up.valid #= false

    dut.clockDomain.waitSampling(10)
    simSuccess()
  }
}
