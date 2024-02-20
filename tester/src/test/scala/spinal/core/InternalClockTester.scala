package spinal.core

import spinal.lib._
import spinal.tester.SpinalTesterCocotbBase


object InternalClockTester{
  class ClockGeneratorSub extends Component{
    val io = NoData

    val internalClock = CounterFreeRun(16)(0)

    val initCounter = RegInit(U(7))
    val srcReset = initCounter =/= U(0)
    when(srcReset){
      initCounter := initCounter - U(1)
    }
    val internalClk = ClockDomain(internalClock)
    val internalReset = Bool()

    val resetBuffer = new ClockingArea(internalClk){
      val internalReset = BufferCC(srcReset)
    }
    internalReset := resetBuffer.internalReset

    val internalClockDomain = ClockDomain(internalClock,internalReset)
  }

  class ClockGenerator extends Component{
    val io = NoData
    val sub = new ClockGeneratorSub
  }

  class CounterComponentSub extends Component{
    val io = new Bundle{
      val counter = out UInt(8 bit)
    }
    val counter = Counter(256)
    counter.increment()
    io.counter := counter
  }

  class CounterComponent extends Component{
    val io = new Bundle{
      val counter = out UInt(8 bit)
    }
    val sub = new CounterComponentSub
    io <> sub.io
  }
}

import InternalClockTester._
class InternalClockTester extends Component {
  val io = new Bundle {
    val internalClkCounter = out UInt(8 bit)
  }
  val clockGenerator = new ClockGenerator

  val internClockDomain = new ClockingArea(clockGenerator.sub.internalClockDomain){
    val counterComponent = new CounterComponent
    io.internalClkCounter := counterComponent.io.counter
  }
}



//class InternalClockTesterGhdlBoot extends SpinalTesterGhdlBase {
//  override def getName: String = "InternalClockTester"
//  override def createToplevel: Component = new InternalClockTester
//}

class InternalClockTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "InternalClockTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/InternalClockTester"
  override def createToplevel: Component = new InternalClockTester
}