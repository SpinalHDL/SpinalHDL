package spinal.lib

import spinal.core._

import spinal.tester.{SpinalTesterCocotbBase, SpinalTesterGhdlBase}

object StreamTester{
  case class BundleA(aaa : Int) extends Bundle{
    val a = UInt(8 bit)
    val b = Bool()
  }
}

import StreamTester._

class StreamTester extends Component {
  val io = new Bundle {
    val slave0 = slave Stream new BundleA(8)
    val master0 = master Stream new BundleA(8)
    val fifo0_occupancy = out UInt()
  }

  val fifo0 = new StreamFifo(new BundleA(8),16)
  fifo0.io.push << io.slave0
  fifo0.io.pop >/-> io.master0
  io.fifo0_occupancy := fifo0.io.occupancy

  assert(3 == LatencyAnalysis(io.slave0.a,io.master0.a))
  assert(2 == LatencyAnalysis(io.master0.ready,io.slave0.ready))



  val forkInput = slave Stream(Bits(8 bits))
  val forkOutputs = Vec(master Stream(Bits(8 bits)),3)
  (forkOutputs , StreamFork(forkInput,3)).zipped.foreach(_ << _)

  val dispatcherInOrderInput = slave Stream(Bits(8 bits))
  val dispatcherInOrderOutputs = Vec(master Stream(Bits(8 bits)),3)
  (dispatcherInOrderOutputs , StreamDispatcherSequencial(dispatcherInOrderInput,3)).zipped.foreach(_ << _)

  val streamFlowArbiterInputStream = slave Stream(Bits(8 bits))
  val streamFlowArbiterInputFlow = slave Flow(Bits(8 bits))
  val streamFlowArbiterOutput = master Flow(Bits(8 bits))
  streamFlowArbiterOutput << StreamFlowArbiter(streamFlowArbiterInputStream,streamFlowArbiterInputFlow)

  val arbiterInOrderInputs =  Vec(slave Stream(Bits(8 bits)),3)
  val arbiterInOrderOutput =  master Stream(Bits(8 bits))
  arbiterInOrderOutput << StreamArbiterFactory.sequentialOrder.on(arbiterInOrderInputs)

  val arbiterLowIdPortFirstInputs =  Vec(slave Stream(Bits(8 bits)),3)
  val arbiterLowIdPortFirstOutput =  master Stream(Bits(8 bits))
  arbiterLowIdPortFirstOutput << StreamArbiterFactory.lowerFirst.on(arbiterLowIdPortFirstInputs)

  val arbiterRoundRobinInputs =  Vec(slave Stream(Bits(8 bits)),3)
  val arbiterRoundRobinOutput =  master Stream(Bits(8 bits))
  arbiterRoundRobinOutput << StreamArbiterFactory.roundRobin.on(arbiterRoundRobinInputs)


  val arbiterLowIdPortFirstNoLockInputs =  Vec(slave Stream(Bits(8 bits)),3)
  val arbiterLowIdPortFirstNoLockOutput =  master Stream(Bits(8 bits))
  arbiterLowIdPortFirstNoLockOutput << StreamArbiterFactory.lowerFirst.noLock.on(arbiterLowIdPortFirstNoLockInputs)

  val arbiterLowIdPortFirstFragmentLockInputs =  Vec(slave Stream(Fragment(Bits(8 bits))),3)
  val arbiterLowIdPortFirstFragmentLockOutput =  master Stream(Fragment(Bits(8 bits)))
  arbiterLowIdPortFirstFragmentLockOutput << StreamArbiterFactory.lowerFirst.fragmentLock.on(arbiterLowIdPortFirstFragmentLockInputs)


  //  val muxSelect = in UInt(2 bits)
//  val muxInputs = Vec(slave Stream(Bits(8 bits)),3)
//  val muxOutput = master Stream(Bits(8 bits))
//  muxOutput << StreamMux(muxSelect,muxInputs)

//  val joinInputs = Vec(slave Stream(Bits(8 bits)),3)
//  val joinOutput = master.Event
//  joinOutput << StreamJoin(joinInputs)
}



class StreamTesterGhdlBoot extends SpinalTesterGhdlBase {
  override def getName: String = "StreamTester"
  override def createToplevel: Component = new StreamTester
}

class StreamTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "StreamTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/StreamTester"
  override def createToplevel: Component = new StreamTester
  override def noVhdl = true
}

object StreamTester2 {
  case class BundleA() extends Bundle{
    val a = UInt(8 bit)
    val b = Bool()
  }

  class StreamTester2 extends Component{
    val fifoA = new StreamFifo(BundleA(),16)
    val fifoAPush = slave(cloneOf(fifoA.io.push))
    val fifoAPop = master(cloneOf(fifoA.io.pop))
    val fifoAOccupancy = out(cloneOf(fifoA.io.occupancy))
    fifoA.io.push << fifoAPush
    fifoA.io.pop >> fifoAPop
    fifoA.io.occupancy <> fifoAOccupancy
    assert(2 == LatencyAnalysis(fifoAPush.a,fifoAPop.a))
    assert(1 == LatencyAnalysis(fifoAPop.ready,fifoAPush.ready))


    val fifoB = new StreamFifoLowLatency(BundleA(),16)
    val fifoBPush = slave(cloneOf(fifoB.io.push))
    val fifoBPop = master(cloneOf(fifoB.io.pop))
    val fifoBOccupancy = out(cloneOf(fifoB.io.occupancy))
    fifoB.io.push << fifoBPush
    fifoB.io.pop >> fifoBPop
    fifoB.io.occupancy <> fifoBOccupancy
    assert(0 == LatencyAnalysis(fifoBPush.a,fifoBPop.a))
    assert(1 == LatencyAnalysis(fifoBPop.ready,fifoBPush.ready))
  }
}

class StreamTester2CocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "StreamTester2"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/StreamTester2"
  override def createToplevel: Component = new StreamTester2.StreamTester2
  override def backendConfig(config: SpinalConfig): SpinalConfig = config
}
