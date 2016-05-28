package spinal.tester.code

import net.liftweb.json.DefaultFormats
import net.liftweb.json.Extraction._
import net.liftweb.json.JsonAST._
import net.liftweb.json.Printer._
import spinal.core._
import spinal.debugger.LogicAnalyserBuilder
import spinal.demo.mandelbrot._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3SlaveController, Apb3Slave, Apb3Config}
import spinal.lib.com.uart.{UartCtrlConfig, Uart, UartCtrl, UartCtrlTx}

import scala.util.Random

object C0 {

  class MyComponent extends Component {
    val io = new Bundle {
      val a = in Bool
      val output = out Bool
    }

    io.output := io.a
  }

}

object C1 {

  class MyComponent extends Component {
    val io = new Bundle {
      val a = in Bool
      val b = in Bool
      val c = in Bool
      val output = out Bool
    }

    io.output := (io.a & io.b) | (!io.c)
  }

}

object C2 {

  class MyComponent extends Component {
    val io = new Bundle {
      val a = in Bool
      val b = in Bool
      val c = in Bool
      val output = out Bool
    }

    io.output := (io.a & io.b) | (!io.c)
  }

}

object C3 {

  class MyComponent extends Component {
    val io = new Bundle {
      val a = in Bool
      val b = in Bool
      val c = in Bool
      val output = out Bool
    }
    val a_and_b = io.a & io.b
    val not_c = !io.c
    io.output := a_and_b | not_c
  }

  def main(args: Array[String]) {
    println("START")
    SpinalVhdl(new MyComponent)
    println("DONE")
  }

}

object C4 {

  class MyComponent extends Component {
    val io = new Bundle {
      val a = in Bool
    }

    val reg1 = Reg(Bool)
    val reg2 = Reg(Bool) init (False)
    val reg3 = RegInit(False)
    val reg4 = RegNext(io.a)
  }

  class MyTopLevel extends Component {
    val io = new Bundle {
      val coreClk = in Bool
      val coreReset = in Bool
      val peripheralClk = in Bool
      val peripheralReset = in Bool
    }
    val coreClockDomain = ClockDomain(io.coreClk, io.coreReset)
    val peripheralClockDomain = ClockDomain(io.peripheralClk, io.peripheralReset)

    val coreArea = new ClockingArea(coreClockDomain) {
      val myCoreClockedRegister = Reg(UInt(4 bit))
    }

    val peripheralArea = new ClockingArea(peripheralClockDomain) {
      val myPeripheralClockedRegister = Reg(UInt(4 bit))
      myPeripheralClockedRegister := coreArea.myCoreClockedRegister //Not allowed
      myPeripheralClockedRegister := BufferCC(coreArea.myCoreClockedRegister)
    }
  }

  def main(args: Array[String]) {
    println("START")
    SpinalVhdl(new MyTopLevel)
    println("DONE")
  }
}

object C5 {

  class MySubComponent extends Component {
    val io = new Bundle {
      val subIn = in Bool
      val subOut = out Bool
    }
    //...
  }

  class MyComponent extends Component {
    val io = new Bundle {
      val a = in Bool
      val b = in Bool
      val output = out Bool
    }

    val compInstance = new MySubComponent

    compInstance.io.subIn := io.a
    io.output := compInstance.io.subOut | io.b
  }

}


object C6 {

  class MyComponent extends Component {
    val io = new Bundle {
      val conds = in Vec(Bool, 2)
      val output = out UInt (4 bit)
    }

    when(io.conds(0)) {
      io.output := 2
      when(io.conds(1)) {
        io.output := 1
      }
    } otherwise {
      io.output := 0
    }
  }

}

object C7 {

  class CarryAdder(size: Int) extends Component {
    val io = new Bundle {
      val a = in UInt (size bit)
      val b = in UInt (size bit)
      val result = out UInt (size bit)
    }

    var c = False
    for (i <- 0 until size) {
      val a = io.a(i)
      val b = io.b(i)

      io.result(i) := a ^ b ^ c
      c = (a & b) | (a & c) | (b & c);
    }
  }

}

object C8 {

  case class Color(channelWidth: Int) extends Bundle {
    val r = UInt(channelWidth bit)
    val g = UInt(channelWidth bit)
    val b = UInt(channelWidth bit)
  }

  class MyColorSelector(sourceCount: Int, channelWidth: Int) extends Component {
    val io = new Bundle {
      val sel = in UInt (log2Up(sourceCount) bit)
      val sources = in Vec(Color(channelWidth), sourceCount)
      val result = out Bits (3 * channelWidth bit)
    }
    val selectedSource = io.sources(io.sel)
    io.result := asBits(selectedSource)
  }


  def main(args: Array[String]) {
    println("START")
    SpinalVhdl(new MyColorSelector(4, 8))
    println("DONE")
  }
}

object C9 {

  case class Color(channelWidth: Int) extends Bundle {
    val r = UInt(channelWidth bit)
    val g = UInt(channelWidth bit)
    val b = UInt(channelWidth bit)

    def +(that: Color): Color = {
      assert(that.channelWidth == this.channelWidth)

      val result = cloneOf(this)
      result.r := channelAdd(this.r, that.r)
      result.g := channelAdd(this.g, that.g)
      result.b := channelAdd(this.b, that.b)

      def channelAdd(left: UInt, right: UInt): UInt = {
        val (value, carry) = adderAndCarry(right, left)
        return Mux(carry, left.maxValue, value)
      }

      return result
    }
  }

  class MyColorSummer(sourceCount: Int, channelWidth: Int) extends Component {
    val io = new Bundle {
      val sources = in Vec(Color(channelWidth), sourceCount)
      val result = out(Color(channelWidth))
    }

    var sum = io.sources(0)
    for (i <- 0 until sourceCount) {
      sum \= sum + io.sources(i)
    }
    io.result := sum

    // But you can do all this stuff by this way, balanced is bonus :
    //io.result := io.sources.reduceBalancedSpinal(_ + _)
  }


  def main(args: Array[String]) {
    println("START")
    SpinalVhdl(new MyColorSummer(4, 8))
    println("DONE")
  }
}

object C10_1 {

  case class Flow[T <: Data](dataType: T) extends Bundle {
    val valid = Bool
    val data: T = cloneOf(dataType)
    //..
  }

  case class Stream[T <: Data](dataType: T) extends Bundle {
    val valid = Bool
    val ready = Bool
    val data: T = cloneOf(dataType)
    //..
  }

  case class Fragment[T <: Data](dataType: T) extends Bundle {
    val last = Bool
    val data: T = cloneOf(dataType)
    //..
  }

}

object C10_2 {

  class StreamFifo[T <: Data](dataType: T, depth: Int) extends Component {
    val io = new Bundle {
      val pushPort = slave Stream (dataType)
      val popPort = master Stream (dataType)
      val occupancy = out UInt (log2Up(depth + 1) bit)
    } //...
  }

  class StreamArbiter[T <: Data](dataType: T, val portCount: Int) extends Component {
    val io = new Bundle {
      val inputs = Vec(slave Stream (dataType), portCount)
      val output = master Stream (dataType)
      val chosen = out UInt (log2Up(portCount) bit)
    } //...
  }

  class StreamFork[T <: Data](dataType: T, portCount: Int) extends Component {
    val io = new Bundle {
      val input = slave Stream (dataType)
      val output = Vec(master Stream (dataType), portCount)
    } //...
  }

  case class LogicAnalyserConfig() extends Bundle {
    val trigger = new Bundle {
      val delay = UInt(32 bit) //...
    }
    val logger = new Bundle {
      val samplesLeftAfterTrigger = UInt(8 bit) //...
    }
  }

  val myBits = Bits(4 bit)
  val myUInt = UInt(4 bit)


  class LogicAnalyser extends Component {
    val io = new Bundle {
      val cfgPort = slave Flow Fragment(Bits(8 bit))
    }
    val waitTrigger = io.cfgPort filterHeader (0x01) toRegOf (Bool) init (False)
    val userTrigger = io.cfgPort pulseOn (0x02)
    val configs = io.cfgPort filterHeader (0x0F) toRegOf (LogicAnalyserConfig())
  }


  case class Color(channelWidth: Int) extends Bundle {
    val r = UInt(channelWidth bit)
    val g = UInt(channelWidth bit)
    val b = UInt(channelWidth bit)
  }

  class ColorDMA extends Component {
    val io = new Bundle {
      val run = slave Event

      val memoryReadAddress = master Stream (Bits(32 bit))
      val memoryReadData = slave Stream (Bits(32 bit))

      val colorStream = master Stream (Color(8))
    }

    io.run.ready := False
    val addressCounter = RegInit(U"x0000")
    when(io.memoryReadAddress.fire) {
      addressCounter := addressCounter + 1
      when(addressCounter === U"xFFFF") {
        addressCounter := 0
        io.run.ready := True
      }
    }

    io.memoryReadAddress.valid := io.run.valid
    io.memoryReadAddress.payload := asBits(addressCounter)

    io.colorStream.translateFrom(io.memoryReadData)(_.assignFromBits(_))
  }

  def main(args: Array[String]) {
    println("START")
    SpinalVhdl(new ColorDMA)
    println("DONE")
  }
}

object C10_removed {

  case class Flow[T <: Data](dataType: T) extends Bundle with IMasterSlave {
    val valid = Bool
    val data: T = cloneOf(dataType)

    override def asMaster(): this.type = out(this)

    override def asSlave(): this.type = in(this)
  }

  abstract case class Stream[T <: Data](dataType: T) extends Bundle with IMasterSlave {
    val valid = Bool
    val ready = Bool
    val data: T = cloneOf(dataType)

    override def asMaster(): this.type = {
      out(valid, data)
      in(ready)
      this
    }

    override def asSlave(): this.type = asMaster().flip()


    //...

    def <<(that: Stream[T]): Unit = {
      this.valid := that.valid
      that.ready := this.ready
      this.data := that.data
    }

    def <-<(that: Stream[T])

    def </<(that: Stream[T])

    def &(cond: Bool): Stream[T]

    def queue(size: Int): Stream[T]

    def fire: Bool

    def throwWhen(cond: Bool)
  }


  //Memory of 1024 Bool
  val mem = Mem(Bool, 1024)

  //Write it
  mem(5) := True

  //Read it
  val read0 = mem.readAsync(4)
  val read1 = mem.readSync(6)

}

object C11 {

  //  val cond = Bool
  //  val inPort = Stream(Bits(32 bit))
  //  val outPort = Stream(Bits(32 bit))
  //
  //  outPort << inPort
  //  outPort <-< inPort
  //  outPort </< inPort
  //  outPort <-/< inPort
  //  val haltedPort = inPort.haltWhen(cond)
  //  val filteredPort = inPort.throwWhen(inPort.data === 0)
  //  val outPortWithMsb = inPort.translateWith(inPort.data.msb)
  //
  //  val mem = Mem(Bool, 1024)
  //  val memReadCmd = Stream(UInt(10 bit))
  //  val memReadPort = mem.streamReadSync(memReadCmd, memReadCmd.data)
  //  memReadPort.valid //arbitration
  //  memReadPort.ready //arbitration
  //  memReadPort.data.value //Readed value
  //  memReadPort.data.linked //Linked value (memReadCmd.data)


  object somewhere {

    object inThe {

      object hierarchy {
        val trigger = True
        val signalA = True
        val signalB = True
      }

    }

    val signalC = True
  }

  val logicAnalyser = LogicAnalyserBuilder()
    .setSampleCount(256)
    .exTrigger(somewhere.inThe.hierarchy.trigger)
    .probe(somewhere.inThe.hierarchy.signalA)
    .probe(somewhere.inThe.hierarchy.signalB)
    .probe(somewhere.signalC)
    .build

//    val uartCtrl = new UartCtrl()
//    uartCtrl.io.read >> logicAnalyser.io.slavePort
//    uartCtrl.io.write << logicAnalyser.io.masterPort
}

object C12 {

  class MyComponentWithLatencyAssert extends Component {
    val io = new Bundle {
      val slavePort = slave Stream (UInt(8 bit))
      val masterPort = master Stream (UInt(8 bit))
    }

    //These 3 line are equivalent to io.slavePort.queue(16) >/-> io.masterPort
    val fifo = new StreamFifo((UInt(8 bit)), 16)
    fifo.io.push << io.slavePort
    fifo.io.pop >/-> io.masterPort

    assert(3 == LatencyAnalysis(io.slavePort.payload, io.masterPort.payload))
    assert(2 == LatencyAnalysis(io.masterPort.ready, io.slavePort.ready))
  }

}


object C13 {

  object MyEnum extends SpinalEnum {
    val state0, state1, anotherState = newElement()
  }

  abstract class MyComponent extends Component {
    val logicOfA = new Area {
      val flag = Bool
      val logic = Bool
    }
    val fsm = new Area {

      import MyEnum._

      val state = Reg(MyEnum()) init (state0)
      switch(state) {
        is(state0) {
          when(logicOfA.flag) {
            state := state1
          }
        }
        default {
        }
      }
    }
  }

}

object C14 {
  //Create a timeout, he become asserted if the function clear
  //was not called in the last 1000 cycles
  val timeout = Timeout(1000)
  when(timeout) {
    //implicit conversion to Bool
    timeout.clear() //Clear the flag and the internal counter
  }

  //Create a counter of 10 states (0 to 9)
  //  val counter = Counter(10)
  //  counter.value     //current value
  //  counter.valueNext //Next value
  //  counter.reset     //When called it reset the counter. It's not a flag
  //  counter.inc       //When called it increment the counter. It's not a flag
  //  counter.overflow  //Flag that indicate if the counter overflow this cycle
  //  when(counter === 5){ }  //counter is implicitly its value


}


object C15 {

  import spinal.core._

  //Define a Stream interface that use the AXI-like valid/ready handshake
  //Transaction's data is a parameterizable type
  //This Stream class is already defined into spinal.lib._   It's just to show as a example
  case class Stream[T <: Data](dataType: T) extends Bundle with IMasterSlave {
    val valid = Bool
    val ready = Bool
    val data: T = cloneOf(dataType)

    //Equivalent to SystemVerilog modport
    override def asMaster(): this.type = {
      out(valid)
      in(ready)
      out(data)
      this
    }

    override def asSlave(): this.type = asMaster().flip() //.flip reverse all signal direction
  }

  //Define a RGB color data type with parameterizable channel width
  case class RGB(rWidth: Int, gWidth: Int, bWidth: Int) extends Bundle {
    val r = UInt(rWidth bit)
    val g = UInt(gWidth bit)
    val b = UInt(bWidth bit)

    //Define the + operator to allow the summation of 2 RGB
    def +(that: RGB): RGB = {
      val result = cloneOf(this)
      result.r := this.r + that.r
      result.g := this.g + that.g
      result.b := this.b + that.b
      result
    }
  }

  // Define a component that take "srcCount" slave Stream of RGB
  // and product an "sumPort" that is the summation of all "srcPort" with the correct arbitration
  case class StreamRgbAdder(rgbType: RGB, srcCount: Int) extends Component {
    val io = new Bundle {
      val srcPort = Vec(slave(Stream(rgbType)), srcCount)
      val sumPort = master(Stream(rgbType))
    }
    val transactionOccure = io.sumPort.valid && io.sumPort.ready
    io.sumPort.valid := io.srcPort.map(_.valid).reduce(_ && _) //Take all srcPort.valid bits and "AND" them
    io.sumPort.data := io.srcPort.map(_.data).reduce(_ + _) //Take all srcPort.data (RGB) and "SUM" them by using the overloaded + operator from RGB class
    io.srcPort.foreach(_.ready := transactionOccure) //For each srcPort.ready assign the transactionOccure
  }


  def main(args: Array[String]) {
    SpinalVhdl(StreamRgbAdder(RGB(5, 6, 5), 4)) //Generate the VHDL for a 4 srcPort and a RGB config of 5,6,5 bits
  }

}


object T1 {
  val mySignal = Bool
  val myRegister = Reg(UInt(4 bit))
  val myRegisterWithInit = Reg(UInt(4 bit)) init (3)

  mySignal := False
  when(???) {
    mySignal := True
    myRegister := myRegister + 1
  }
}


object T2 {

  val valid = Bool
  val regA = Reg(UInt(4 bit))

  def doSomething(value: Int) = {
    valid := True
    regA := value
  }

  when(???) {
    doSomething(4)
  }
}

object T3a {

  case class Color(channelWidth: Int) extends Bundle {
    val red = UInt(channelWidth bit)
    val green = UInt(channelWidth bit)
    val blue = UInt(channelWidth bit)
  }

  case class Stream[T <: Data](dataType: T) extends Bundle {
    val valid = Bool
    val ready = Bool
    val data: T = cloneOf(dataType)
  }

  val myStreamOfColor = Stream(Color(8))
}

object T3b {

  case class Stream[T <: Data](dataType: T) extends Bundle {
    val valid = Bool
    val ready = Bool
    val data: T = cloneOf(dataType)

    def transferOccure: Bool = valid & ready

    def connectFrom(that: Stream[T]) = {
      this.valid := that.valid
      that.ready := this.ready
      this.data := that.data
    }

    def <<(that: Stream[T]) = connectFrom(that)
  }

  val myStreamA, myStreamB = Stream(UInt(8 bit))
  myStreamA << myStreamB
  when(myStreamA.transferOccure) {
    ???
  }

}

object T3c {

  case class Stream[T <: Data](dataType: T) extends Bundle {
    // ...
    def connectFrom(that: Stream[T]) = {
      // some connections
    }

    def m2sPipe(): Stream[T] = {
      val outputStage = cloneOf(this)
      val validReg = RegInit(False)
      val dataReg = Reg(dataType)
      // some logic
      return outputStage
    }

    def <<(that: Stream[T]) = this.connectFrom(that)

    def <-<(that: Stream[T]) = this << that.m2sPipe()
  }

  val myStreamA, myStreamB = Stream(UInt(8 bit))
  myStreamA <-< myStreamB
}

object T4 {


  case class Stream[T <: Data](dataType: T) extends Bundle with IMasterSlave {
    val valid = Bool
    val ready = Bool
    val data: T = cloneOf(dataType)

    override def asMaster(): this.type = {
      out(valid)
      in(ready)
      out(data)
      this
    }

    override def asSlave(): this.type = asMaster().flip()
  }

  class Fifo[T <: Data](dataType: T, depth: Int) extends Component {
    val io = new Bundle {
      val pushPort = slave Stream (dataType)
      val popPort = master Stream (dataType)
      val occupancy = out UInt (log2Up(depth + 1) bit)
    } //...
  }

  class Arbiter[T <: Data](dataType: T, portCount: Int) extends Component {
    val io = new Bundle {
      val inputs = Vec(slave(Stream(dataType)), portCount)
      val output = master(Stream(dataType))
    }
    //...
  }

}


object T5 {


  class SubComponent extends Component {
    val condA, condB = in Bool
    val result = out Bool
    // ...
  }

  class TopComponent extends Component {
    // ...
    val subA = new SubComponent
    val subB = new SubComponent
    subB.condA := subA.result
    // ...
  }

}


object t6 {

  class UartCtrlTx extends Component {
    val io = new Bundle {
      // io definition
    }
    val timer = new Area {
      // emit a pulse that is used as time reference
    }
    val stateMachine = new Area {
      // some logic
    }
  }

}

object t7 {
  val timer = new Area {
    val counter = Reg(UInt(8 bit))
    val reset = False
    val tick = counter === 192

    counter := counter + 1
    when(tick || reset) {
      counter := 0
    }
  }

  val stateMachine = new Area {
    // some logic
    when(timer.tick) {
      // do something
    }
  }

}


object t8_a {
  //Stream => valid, ready, data
  //Flow   => valid, data
  //APB    => Advanced Peripheral Bus from arm

  case class UartCtrlConfig() extends Bundle {
    val clockDivider = UInt(16 bit)
    val parityKind = Bits(2 bit)
    val stopBitKind = Bits(2 bit)
  }

  class UartCtrl extends Component {
    val io = new Bundle {
      val config = in(UartCtrlConfig())
      val write = slave Stream (Bits(8 bit))
      val read = master Flow (Bits(8 bit))
      val uart = master(Uart())
    }
    // some logic ..
  }

  class ApbUartCtrl(apbConfig: Apb3Config) extends Component {
    val io = new Bundle {
      val bus = slave(new Apb3Slave(apbConfig))
      val uart = master(Uart())
    }
    val busCtrl = new Apb3SlaveController(io.bus) //This is a APB3 slave controller builder tool

    val config = busCtrl.writeOnlyRegOf(UartCtrlConfig(), 0x10) //Create a write only configuration register at address 0x10
    val writeStream = busCtrl.writeStreamOf(Bits(8 bit), 0x20)
    val readStream = busCtrl.readStreamOf(Bits(8 bit), 0x30)

    val uartCtrl = new UartCtrl()
    uartCtrl.io.config := config
    uartCtrl.io.write <-< writeStream //Pipelined connection
    uartCtrl.io.read.toStream.queue(16) >> readStream  //Queued connection
    uartCtrl.io.uart <> io.uart
  }
}


object t8_B2 {

  class ApbUartCtrl(apbConfig: Apb3Config) extends Component {
    val io = new Bundle {
      val apb = slave(new Apb3Slave(apbConfig))
      val uart = master(Uart())
    }
    val uartCtrl = new UartCtrl()
    uartCtrl.io.uart <> io.uart

    val busCtrl = new Apb3SlaveController(io.apb)
    busCtrl.writeOnlyRegOf(uartCtrl.io.config, 0x10)
    busCtrl.writeStream(uartCtrl.io.write, 0x20)
    busCtrl.readStream(uartCtrl.io.read.toStream.queue(16), 0x30)
  }

}


object t9 {

  class FrameTaskSolver(p: MandelbrotCoreParameters) extends Component {
    val io = new Bundle {
      val frameTask = slave Stream FrameTask(p)
      val pixelResult = master Stream Fragment(PixelResult(p))
    }

    val pixelTaskGenerator = new PixelTaskGenerator(p)
    val pixelTaskDispatcher = new DispatcherInOrder(Fragment(PixelTask(p)), p.pixelTaskSolverCount)
    val pixelTaskSolver = for (i <- 0 until p.pixelTaskSolverCount) yield new PixelTaskSolver(p)
    val pixelResultArbiter = StreamArbiter.inOrder.build(Fragment(PixelResult(p)), p.pixelTaskSolverCount)

    pixelTaskGenerator.io.frameTask << io.frameTask
    pixelTaskDispatcher.io.input <-/< pixelTaskGenerator.io.pixelTask
    for (solverId <- 0 until p.pixelTaskSolverCount) {
      pixelTaskSolver(solverId).io.pixelTask <-/< pixelTaskDispatcher.io.outputs(solverId)
      pixelResultArbiter.io.inputs(solverId) </< pixelTaskSolver(solverId).io.pixelResult
    }
    io.pixelResult <-< pixelResultArbiter.io.output
  }

}


object t10 {

  val arrayOfBool = Vec(Bool, 5)
  val orOfBool = arrayOfBool.reduce(_ || _)


  val arrayOfUInt = Vec(UInt(5 bit), 16)
  val sumOfUInt = arrayOfUInt.fold(U(0, 9 bit))(_ + _)

  val arrayOfStreamA = Vec(Stream(Bool), 10)
  val arrayOfStreamB = Vec(Stream(Bool), 10)
  (arrayOfStreamA, arrayOfStreamB).zipped.foreach(_ >> _)

  for (i <- 0 to 10) {
    arrayOfStreamA(i) >> arrayOfStreamB(i)
  }


  //  val arrayOfArrayOfBool = Vec(Vec(Bool, 5), 10)
  //  val orReduce2D = arrayOfArrayOfBool.map(_.reduce(_ || _)).reduce(_ || _)


}


object t11 {
  val waveform = for (i <- 0 to 1023) yield S((Math.pow(Math.sin(i * 2 * Math.PI / 1024.0), 1) * 32767).toInt)
  val rom = Mem(SInt(16 bit), waveform)
  val animatedWaveform = rom.readSync(CounterFreeRun(1024))
}


object t12 {

  val coreClk = in Bool
  val coreReset = in Bool
  val coreClockDomain = ClockDomain(coreClk, coreReset)

  val coreClockedArea = new ClockingArea(coreClockDomain) {
    val counter = Reg(UInt(8 bit))

  }
}


object t13 {
  val normalVec = Vec(UInt(4 bit), 10)
//  val variableVec = Vec(UInt(5 bit), UInt(9 bit), UInt(16 bit))

  val containZero = normalVec.sContains(0)
  val existOne = normalVec.sExists(_ === 1)
  val (firstTwoValid, firstTwoIndex) = normalVec.sFindFirst(_ === 2)
  val toto = normalVec.map(_ === 3)


  val targetAddress = UInt(32 bit)

  val lines = new Area {
    class tag extends Bundle {
      val valid = Bool
      val address = UInt(32 bit)
      val dirty = Bool

      def hit(targetAddress: UInt): Bool = valid && address === targetAddress
    }

    val tags = Vec(new tag, 8)
    val hits = tags.map(tag => tag.hit(targetAddress))
    val hitValid = hits.reduce((a, b) => a || b)
    val hitIndex = OHToUInt(hits)
  }
}





object t14{

  case class MandelbrotCoreParameters(iterationLimit: Int,
                                      pixelTaskSolverCount: Int,
                                      screenResX: Int,
                                      screenResY: Int,
                                      fixExp: Int,
                                      fixWidth: Int) {
    val uid : Int = Random.nextInt()
  }
  case class MandelbrotJsonReport(p : MandelbrotCoreParameters,
                                  uid : String,
                                  clazz : String = "uidPeripheral",
                                  kind : String = "mandelbrotCore")
  class MandelbrotCore(p: MandelbrotCoreParameters) extends Component {
    // some logic
    val json = decompose(MandelbrotJsonReport(p, p.uid.toString))(DefaultFormats)
    GlobalData.get.addJsonReport(pretty(render(json)))
  }

}




















































import spinal.core._

class AND_Gate extends Component {

  /**
   * This is the component definition that corresponds to
   * the VHDL entity of the component
   */
  val io = new Bundle {
    val a = in Bool
    val b = in Bool
    val c = out Bool
  }

  // Here we define some asynchronous logic
  io.c := io.a & io.b
}

object AND_Gate {
  // Let's go
  def main(args: Array[String]) {
    SpinalVhdl(new AND_Gate)
  }
}



class CustomClockExample extends Component {
  val io = new Bundle {
    val clk = in Bool
    val resetn = in Bool
    val result = out UInt (4 bits)
  }

  val myClockDomainConfig = ClockDomainConfig(
    clockEdge = RISING,
    resetKind = ASYNC,
    resetActiveLevel = LOW
  )
  val myClockDomain = ClockDomain(io.clk,io.resetn,config = myClockDomainConfig)

  val myArea = new ClockingArea(myClockDomain){
    val myReg = Reg(UInt(4 bits)) init(7)
    myReg := myReg + 1

    io.result := myReg
  }
}


object CustomClockExample{
  def main(args: Array[String]) {
    SpinalVhdl(new CustomClockExample)
  }
}



class ExternalClockExample extends Component {
  val io = new Bundle {
    val result = out UInt (4 bits)
  }
  val myClockDomain = ClockDomain.external("myClockName")
  val myArea = new ClockingArea(myClockDomain){
    val myReg = Reg(UInt(4 bits)) init(7)
    myReg := myReg + 1

    io.result := myReg
  }
}


object ExternalClockExample{
  def main(args: Array[String]) {
    SpinalVhdl(new ExternalClockExample)
  }
}



object RgbToGray{
  class RgbToGray extends Component{
    val io = new Bundle{
      val clear = in Bool
      val r,g,b = in UInt(8 bits)

      val wr = out Bool
      val address = out UInt(16 bits)
      val data = out UInt(8 bits)
    }

    def coef(value : UInt,by : Float) : UInt = (value * U((255*by).toInt,8 bits) >> 8)
    val gray = RegNext(
      coef(io.r,0.3f) +
      coef(io.g,0.4f) +
      coef(io.b,0.3f)
    )

    val address = CounterFreeRun(stateCount = 1 << 16)
    io.address := address
    io.wr := True
    io.data := gray

    when(io.clear){
      gray := 0
      address.clear()
      io.wr := False
    }
  }


  def main(args: Array[String]) {
    SpinalVhdl(new RgbToGray)
  }
}


object CombinatorialLogic {

  class TopLevel extends Component {
    val io = new Bundle {
      val cond   = in Bool
      val value      = in UInt (4 bit)
      val withoutProcess = out UInt(4 bits)
      val withProcess = out UInt(4 bits)
    }
    io.withProcess := io.value
    io.withoutProcess := 0
    when(io.cond){
      switch(io.value){
        is(U"0000"){
          io.withoutProcess := 8
        }
        is(U"0001"){
          io.withoutProcess := 9
        }
        default{
          io.withoutProcess := io.value+1
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    //SpinalVhdl(new TopLevel)
    SpinalVhdl(new TopLevel)
  }
}


object FlipFlop {
  class TopLevel extends Component {
    val io = new Bundle {
      val cond   = in Bool
      val value  = in UInt (4 bit)
      val resultA = out UInt(4 bit)
      val resultB = out UInt(4 bit)
    }

    val regWithReset = Reg(UInt(4 bits)) init(0)
    val regWithoutReset = Reg(UInt(4 bits))

    regWithReset := io.value

    regWithoutReset := 0
    when(io.cond){
      regWithoutReset := io.value
    }

    io.resultA := regWithReset
    io.resultB := regWithoutReset
  }

  def main(args: Array[String]): Unit = {
    //SpinalVhdl(new TopLevel)
    SpinalVhdl(new TopLevel)
  }
}


object SinFir {
  class TopLevel(resolutionWidth : Int,sampleCount : Int,firLength : Int) extends Component {
    val io = new Bundle {
      val sin = out SInt(resolutionWidth bit)
      val sinFir = out SInt(resolutionWidth bit)

      val square = out SInt(resolutionWidth bit)
      val squareFir = out SInt(resolutionWidth bit)
    }

    def sinTable = (0 until sampleCount).map(sampleIndex => {
      val sinValue = Math.sin(2 * Math.PI * sampleIndex / sampleCount)
      S((sinValue * ((1<<resolutionWidth)/2-1)).toInt,resolutionWidth bits)
    })

    val rom =  Mem(SInt(resolutionWidth bit),initialContent = sinTable)
    val phase = CounterFreeRun(sampleCount)
    val sin = rom.readSync(phase)
    val square =  S(resolutionWidth-1 -> ! phase.msb, default -> phase.msb)

    val firCoefs = (0 until firLength).map(i => {
      val coefValue = 0.54 - 0.46 * Math.cos(2 * Math.PI * i / firLength)
      S((coefValue * ((1<<resolutionWidth)/2-1)).toInt,resolutionWidth bits)
    })

    def applyFirTo(that : SInt) : SInt = {
      val firMul = for((coef,value) <- (firCoefs, History(that, firLength-1)).zipped) yield {
        (coef * value) >> resolutionWidth
      }
      firMul.foldLeft(S(0,resolutionWidth + log2Up(firLength) bits))(_ + _)
    }

    io.sin := sin
    io.sinFir := applyFirTo(sin) >> log2Up(firLength)

    io.square := square
    io.squareFir := applyFirTo(square) >> log2Up(firLength)
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel(
      resolutionWidth=16,
      sampleCount=64,
      firLength=16
    ))
  }
}



