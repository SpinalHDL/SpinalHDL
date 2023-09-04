package spinal.lib

import spinal.core._
import spinal.tester.SpinalTesterCocotbBase


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