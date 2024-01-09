package spinal.tester.code


import spinal.core._
import spinal.demo.mandelbrot._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3SlaveFactory, Apb3, Apb3Config}
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}
import spinal.lib.bus.amba4.axilite.{AxiLite4SlaveFactory, AxiLite4Config, AxiLite4}
import spinal.lib.bus.avalon.{AvalonMM, AvalonMMSlaveFactory}
import spinal.lib.com.uart._
import spinal.lib.cpu.riscv.impl.extension._
import spinal.lib.cpu.riscv.impl.{InstructionCacheConfig, dynamic, sync, RiscvCoreConfig}
import spinal.lib.graphic.Rgb
import spinal.lib.misc.{Prescaler, Timer}


import scala.util.Random

object C0 {

  class MyComponent extends Component {
    val io = new Bundle {
      val a = in Bool()
      val output = out Bool()
    }

    io.output := io.a
  }

}

object C1 {

  class MyComponent extends Component {
    val io = new Bundle {
      val a = in Bool()
      val b = in Bool()
      val c = in Bool()
      val output = out Bool()
    }

    io.output := (io.a & io.b) | (!io.c)
  }

}

object C2 {

  class MyComponent extends Component {
    val io = new Bundle {
      val a = in Bool()
      val b = in Bool()
      val c = in Bool()
      val output = out Bool()
    }

    io.output := (io.a & io.b) | (!io.c)
  }

}

object C3 {

  class MyComponent extends Component {
    val io = new Bundle {
      val a = in Bool()
      val b = in Bool()
      val c = in Bool()
      val output = out Bool()
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
      val a = in Bool()
    }

    val reg1 = Reg(Bool())
    val reg2 = Reg(Bool()) init (False)
    val reg3 = Reg(Bool())
    reg3 := io.a
  }

  class MyTopLevel extends Component {
    val io = new Bundle {
      val coreClk = in Bool()
      val coreReset = in Bool()
      val peripheralClk = in Bool()
      val peripheralReset = in Bool()
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
      val subIn = in Bool()
      val subOut = out Bool()
    }
    //...
  }

  class MyComponent extends Component {
    val io = new Bundle {
      val a = in Bool()
      val b = in Bool()
      val output = out Bool()
    }

    val compInstance = new MySubComponent

    compInstance.io.subIn := io.a
    io.output := compInstance.io.subOut | io.b
  }

}


object C6 {

  class MyComponent extends Component {
    val io = new Bundle {
      val conds = in Vec(Bool(), 2)
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
    io.result := B(selectedSource)
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

      val result = Color(channelWidth)
      result.r := channelAdd(this.r, that.r)
      result.g := channelAdd(this.g, that.g)
      result.b := channelAdd(this.b, that.b)

      def channelAdd(left: UInt, right: UInt): UInt = {
        val (value, carry) = AddWithCarry(right, left)
        return Mux(carry, left.maxValue, value)
      }

      return result
    }
  }

  class MyColorSummer(sourceCount: Int, channelWidth: Int) extends Component {
    val io = new Bundle {
      val sources = in Vec(Color(channelWidth), sourceCount)
      val result  = out(Color(channelWidth))
    }

    var sum = io.sources(0)
    for (i <- 1 until sourceCount) {
      sum \= sum + io.sources(i)
    }
    io.result := sum

    // But you can do all this stuff by this way:
    //io.result := io.sources.reduce(_ + _)
  }


  def main(args: Array[String]) {
    println("START")
    SpinalVhdl(new MyColorSummer(4, 8))
    println("DONE")
  }
}

object C10_1 {

  case class Flow[T <: Data](dataType: T) extends Bundle {
    val valid = Bool()
    val data: T = cloneOf(dataType)
    //..
  }

  case class Stream[T <: Data](dataType: T) extends Bundle {
    val valid = Bool()
    val ready = Bool()
    val data: T = cloneOf(dataType)
    //..
  }

  case class Fragment[T <: Data](dataType: T) extends Bundle {
    val last = Bool()
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
}

object C10_removed {

  case class Flow[T <: Data](dataType: T) extends Bundle with IMasterSlave {
    val valid = Bool()
    val data: T = cloneOf(dataType)

    override def asMaster(): Unit = out(this)

    override def asSlave(): Unit = in(this)
  }

  abstract case class Stream[T <: Data](dataType: T) extends Bundle with IMasterSlave {
    val valid = Bool()
    val ready = Bool()
    val data: T = cloneOf(dataType)

    override def asMaster(): Unit = {
      out(valid, data)
      in(ready)
    }


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
      val flag = Bool()
      val logic = Bool()
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

object C13b {

  object MyEnum extends SpinalEnum {
    val state0, state1, anotherState = newElement()
  }

  class MyComponent extends Component {
    val state = Reg(MyEnum) init(MyEnum.state0)
    switch(state) {
      is(MyEnum.state0) {

      }
      is(MyEnum.state1) {

      }
      is(MyEnum.anotherState) {

      }
      default{

      }
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new MyComponent)
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
    val valid = Bool()
    val ready = Bool()
    val data: T = cloneOf(dataType)

    //Equivalent to SystemVerilog modport
    override def asMaster(): Unit = {
      out(valid)
      in(ready)
      out(data)
    }

  }

  //Define a RGB color data type with parameterizable channel width
  case class RGB(rWidth: Int, gWidth: Int, bWidth: Int) extends Bundle {
    val r = UInt(rWidth bit)
    val g = UInt(gWidth bit)
    val b = UInt(bWidth bit)

    def isBlack : Bool = r === 0 && g === 0 && b === 0

    //Define the + operator to allow the summation of 2 RGB
    def +(that: RGB): RGB = {
      val result = cloneOf(this)
      result.r := this.r + that.r
      result.g := this.g + that.g
      result.b := this.b + that.b
      result
    }
  }

  val source = spinal.lib.Stream(RGB(5,6,5))
  val sink = source.throwWhen(source.payload.isBlack).stage()

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


  object Funcitonal{
    val addresses = Vec(UInt(8 bits),4)
    val key  = UInt(8 bits)
    val hits = addresses.map(address => address === key)
    val hit  = hits.reduce((a,b) => a || b)
  }

  def main(args: Array[String]) {
    SpinalVhdl(StreamRgbAdder(RGB(5, 6, 5), 4)) //Generate the VHDL for a 4 srcPort and a RGB config of 5,6,5 bits
  }

}


object T1 {
  val cond                =  in Bool()
  val mySignal            = Bool()
  val myRegister          = Reg(UInt(4 bit))
  val myRegisterWithReset = Reg(UInt(4 bit)) init (3)

  mySignal := False
  when(cond) {
    mySignal            := True
    myRegister          := myRegister + 1
    myRegisterWithReset := myRegisterWithReset + 1
  }
}


object T2 {

  val valid = Bool()
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
    val valid = Bool()
    val ready = Bool()
    val data: T = cloneOf(dataType)
  }

  val myStreamOfColor = Stream(Color(8))
}

object T3b {

  case class Stream[T <: Data](dataType: T) extends Bundle {
    val valid = Bool()
    val ready = Bool()
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
    val valid = Bool()
    val ready = Bool()
    val data: T = cloneOf(dataType)

    override def asMaster(): Unit = {
      out(valid)
      in(ready)
      out(data)
    }
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
    val condA, condB = in Bool()
    val result = out Bool()
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

  //  class ApbUartCtrl(apbConfig: Apb3Config) extends Component {
  //    val io = new Bundle {
  //      val bus = slave(new Apb3(apbConfig))
  //      val uart = master(Uart())
  //    }
  //    val busCtrl = new Apb3SlaveController(io.bus) //This is a APB3 slave controller builder tool
  //
  //    val config = busCtrl.writeOnlyRegOf(UartCtrlConfig(), 0x10) //Create a write only configuration register at address 0x10
  //    val writeStream = busCtrl.writeStreamOf(Bits(8 bit), 0x20)
  //    val readStream = busCtrl.readStreamOf(Bits(8 bit), 0x30)
  //
  //    val uartCtrl = new UartCtrl()
  //    uartCtrl.io.config := config
  //    uartCtrl.io.write <-< writeStream //Pipelined connection
  //    uartCtrl.io.read.toStream.queue(16) >> readStream  //Queued connection
  //    uartCtrl.io.uart <> io.uart
  //  }
}


object t8_B2 {
  //
  //  class ApbUartCtrl(apbConfig: Apb3Config) extends Component {
  //    val io = new Bundle {
  //      val apb = slave(new Apb3(apbConfig))
  //      val uart = master(Uart())
  //    }
  //    val uartCtrl = new UartCtrl()
  //    uartCtrl.io.uart <> io.uart
  //
  //    val busCtrl = new Apb3SlaveController(io.apb)
  //    busCtrl.writeOnlyRegOf(uartCtrl.io.config, 0x10)
  //    busCtrl.writeStream(uartCtrl.io.write, 0x20)
  //    busCtrl.readStream(uartCtrl.io.read.toStream.queue(16), 0x30)
  //  }

}


object t9 {

  class FrameTaskSolver(p: MandelbrotCoreParameters) extends Component {
    val io = new Bundle {
      val frameTask = slave Stream FrameTask(p)
      val pixelResult = master Stream Fragment(PixelResult(p))
    }

    val pixelTaskGenerator = new PixelTaskGenerator(p)
    val pixelTaskDispatcher = new StreamDispatcherSequencial(Fragment(PixelTask(p)), p.pixelTaskSolverCount)
    val pixelTaskSolver = for (i <- 0 until p.pixelTaskSolverCount) yield new PixelTaskSolver(p)
    val pixelResultArbiter = StreamArbiterFactory.sequentialOrder.build(Fragment(PixelResult(p)), p.pixelTaskSolverCount)

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

  val arrayOfBool = Vec(Bool(), 5)
  val orOfBool = arrayOfBool.reduce(_ || _)


  val arrayOfUInt = Vec(UInt(5 bit), 16)
  val sumOfUInt = arrayOfUInt.fold(U(0, 9 bit))(_ + _)

  val arrayOfStreamA = Vec(Stream(Bool), 10)
  val arrayOfStreamB = Vec(Stream(Bool), 10)
  (arrayOfStreamA, arrayOfStreamB).zipped.foreach(_ >> _)

  for (i <- 0 to 10) {
    arrayOfStreamA(i) >> arrayOfStreamB(i)
  }


  //  val arrayOfArrayOfBool = Vec(Vec(Bool(), 5), 10)
  //  val orReduce2D = arrayOfArrayOfBool.map(_.reduce(_ || _)).reduce(_ || _)


}


object t11 {
  val waveform = for (i <- 0 to 1023) yield S((Math.pow(Math.sin(i * 2 * Math.PI / 1024.0), 1) * 32767).toInt)
  val rom = Mem(SInt(16 bit), waveform)
  val animatedWaveform = rom.readSync(CounterFreeRun(1024))
}


object t12 {

  val coreClk = in Bool()
  val coreReset = in Bool()
  val coreClockDomain = ClockDomain(coreClk, coreReset)

  val coreClockedArea = new ClockingArea(coreClockDomain) {
    val counter = Reg(UInt(8 bit))

  }
}


object t13 {
  val normalVec = Vec(UInt(4 bit), 10)
  //  val variableVec = Vec(UInt(5 bit), UInt(9 bit), UInt(16 bit))

  val containZero = normalVec.sContains(0)
  val existOne = normalVec.sExist(_ === 1)
  val (firstTwoValid, firstTwoIndex) = normalVec.sFindFirst(_ === 2)
  val toto = normalVec.map(_ === 3)


  val targetAddress = UInt(32 bit)

  val lines = new Area {
    class tag extends Bundle {
      val valid = Bool()
      val address = UInt(32 bit)
      val dirty = Bool()

      def hit(targetAddress: UInt): Bool = valid && address === targetAddress
    }

    val tags = Vec(new tag, 8)
    val hits = tags.map(tag => tag.hit(targetAddress))
    val hitValid = hits.reduce((a, b) => a || b)
    val hitIndex = OHToUInt(hits)
  }
}


























































import spinal.core._

class AND_Gate extends Component {

  /**
    * This is the component definition that corresponds to
    * the VHDL entity of the component
    */
  val io = new Bundle {
    val a = in Bool()
    val b = in Bool()
    val c = out Bool()
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
    val clk = in Bool()
    val resetn = in Bool()
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
      val clear = in Bool()
      val r,g,b = in UInt(8 bits)

      val wr = out Bool()
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


object RgbToGray2{
  // Input RGB color
  val r,g,b = UInt(8 bits)

  // Define a function to multiply a UInt by a scala Float value.
  def coefMul(value : UInt,by : Float) : UInt = (value * U((255*by).toInt,8 bits) >> 8)

  //Calculate the gray level
  val gray = coefMul(r,0.3f) +
    coefMul(g,0.4f) +
    coefMul(b,0.3f)


  class RgbToGray extends Component{
    val io = new Bundle{
      val clear = in Bool()
      val r,g,b = in UInt(8 bits)

      val wr      = out Bool()
      val address = out UInt(16 bits)
      val data    = out UInt(8 bits)
    }

    def coef(value : UInt,by : Float) : UInt =
      (value * U((255*by).toInt,8 bits) >> 8)

    val gray = RegNext(
      coef(io.r,0.3f) +
        coef(io.g,0.4f) +
        coef(io.b,0.3f)
    )

    val address = Reg(UInt(8 bits)) init(0)
    address := address + 1

    io.address := address
    io.wr      := True
    io.data    := gray

    when(io.clear){
      gray    := 0
      address := 0
      io.wr   := False
    }
  }
}

object CombinatorialLogic {

  class TopLevel extends Component {
    val io = new Bundle {
      val cond            = in Bool()
      val value           = in UInt (4 bit)
      val withoutProcess  = out UInt(4 bits)
      val withProcess     = out UInt(4 bits)
    }
    io.withoutProcess := io.value
    io.withProcess := 0
    when(io.cond){
      switch(io.value){
        is(U"0000"){
          io.withProcess := 8
        }
        is(U"0001"){
          io.withProcess := 9
        }
        default{
          io.withProcess := io.value+1
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
      val cond   = in Bool()
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
      val firMul = for((coef,value) <- (firCoefs, History(that, firLength)).zipped) yield {
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

    SpinalVerilog(new TopLevel(
      resolutionWidth=16,
      sampleCount=64,
      firLength=16
    ))
  }
}



object SinFir2{
  class TopLevel(resolutionWidth : Int,sampleCount : Int,firLength : Int) extends Component {
    val io = new Bundle {
      val sin = out SInt(resolutionWidth bit)
      val sinFiltred = out SInt(resolutionWidth bit)
    }

    def sinTable = for(sampleIndex <- 0 until sampleCount) yield {
      val sinValue = Math.sin(2 * Math.PI * sampleIndex / sampleCount)
      S((sinValue * ((1<<resolutionWidth)/2-1)).toInt,resolutionWidth bits)
    }

    val rom =  Mem(SInt(resolutionWidth bit),initialContent = sinTable)
    val phase = Reg(UInt(log2Up(sampleCount) bits)) init(0)
    phase := phase + 1

    io.sin := rom.readSync(phase)
    io.sinFiltred := RegNext(io.sinFiltred  - (io.sinFiltred  >> 5) + (io.sin >> 5)) init(0)
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel(
      resolutionWidth=16,
      sampleCount=64,
      firLength=16
    ))

    SpinalVerilog(new TopLevel(
      resolutionWidth=16,
      sampleCount=64,
      firLength=16
    ))
  }

  //  class TopLevel(resolutionWidth : Int,sampleCount : Int,firLength : Int) extends Component {
  //    val io = new Bundle {
  //      val square = out Bool()
  //      val sin = out SInt(resolutionWidth bit)
  //      val mixed = out SInt(resolutionWidth bit)
  //      val filtred = out SInt(resolutionWidth bit)
  //    }
  //
  //    def sinTable = for(sampleIndex <- 0 until sampleCount) yield {
  //      val sinValue = Math.sin(2 * Math.PI * sampleIndex / sampleCount)
  //      S((sinValue * ((1<<resolutionWidth)/2-1)).toInt,resolutionWidth bits)
  //    }
  //
  //    val rom =  Mem(SInt(resolutionWidth bit),initialContent = sinTable)
  //    val phase = Reg(UInt(log2Up(sampleCount) bits)) init(0)
  //    phase := phase + 1
  //
  //    io.sin := rom.readSync(phase)
  //    io.square := phase.msb
  //    io.mixed := (io.sin >> 1) + S((resolutionWidth-1 downto resolutionWidth-2) -> False,(resolutionWidth-3 downto 0) -> io.square)
  //    io.filtred := RegNext(io.filtred  - (io.filtred  >> 5) + (io.mixed >> 5)) init(0)
  //  }


}


object c99{


  class MyTopLevel extends Component {
    val io = new Bundle {
      val coreClk = in Bool()
      val coreReset = in Bool()
    }
    val coreClockDomain = ClockDomain(
      clock  = io.coreClk,
      reset  = io.coreReset,
      config = ClockDomainConfig(
        clockEdge        = RISING,
        resetKind        = ASYNC,
        resetActiveLevel = HIGH
      )
    )

    val coreArea = new ClockingArea(coreClockDomain) {
      val myCoreClockedRegister = Reg(UInt(4 bit))
      //...
    }
  }
}



object c666{
  class AvalonUartCtrl(uartCtrlConfig : UartCtrlGenerics, rxFifoDepth : Int) extends Component{
    val io = new Bundle{
      val bus =  slave(AvalonMM(AvalonMMUartCtrl.getAvalonMMConfig))
      val uart = master(Uart())
    }

    // Instanciate an simple uart controller
    val uartCtrl = new UartCtrl(uartCtrlConfig)
    io.uart <> uartCtrl.io.uart

    // Create an instance of the AvalonMMSlaveFactory that will then be used as a slave factory drived by io.bus
    val busCtrl = AvalonMMSlaveFactory(io.bus)

    // Ask the busCtrl to create a readable/writable register at the address 0
    // and drive uartCtrl.io.config.clockDivider with this register
    busCtrl.driveAndRead(uartCtrl.io.config.clockDivider,address = 0)

    // Do the same thing than above but for uartCtrl.io.config.frame at the address 4
    busCtrl.driveAndRead(uartCtrl.io.config.frame,address = 4)

    // Ask the busCtrl to create a writable Flow[Bits] (valid/payload) at the address 8.
    // Then convert it into a stream and connect it to the uartCtrl.io.write by using an register stage (>->)
    busCtrl.createAndDriveFlow(Bits(uartCtrlConfig.dataWidthMax bits),address = 8).toStream >-> uartCtrl.io.write

    // To avoid losing writes commands between the Flow to Stream transformation just above,
    // make the occupancy of the uartCtrl.io.write readable at address 8
    busCtrl.read(uartCtrl.io.write.valid,address = 8)

    // Take uartCtrl.io.read, convert it into a Stream, then connect it to the input of a FIFO of 64 elements
    // Then make the output of the FIFO readable at the address 12 by using a non blocking protocol
    // (bit 0 => data valid, bits 8 downto 1 => data)
    busCtrl.readStreamNonBlocking(uartCtrl.io.read.queue(rxFifoDepth),address = 12,validBitOffset = 31,payloadBitOffset = 0)
  }


  class Apb3UartCtrl_1(rxFifoDepth : Int) extends Component {
    val io = new Bundle {
      val bus = slave(Apb3(addressWidth = 4, dataWidth = 32))
      val uart = master(Uart())
    }

    // Instanciate an simple uart controller
    val uartCtrl = new UartCtrl()

    //Connect its uart bus
    io.uart <> uartCtrl.io.uart

    //Here we need some glue between the io.bus and the uartCtrl !
  }


  class Apb3UartCtrl_2(rxFifoDepth : Int) extends Component {
    val io = new Bundle {
      val bus = slave(Apb3(addressWidth = 4, dataWidth = 32))
      val uart = master(Uart())
    }

    // Instanciate an simple uart controller
    val uartCtrl = new UartCtrl()

    //Connect its uart bus
    io.uart <> uartCtrl.io.uart




    // Create an instance of the Apb3SlaveFactory that will then be used as a slave factory drived by io.bus
    val busCtrl = Apb3SlaveFactory(io.bus)
  }

  class Apb3UartCtrl(rxFifoDepth : Int) extends Component{
    val io = new Bundle{
      val bus =  slave(Apb3(addressWidth = 4, dataWidth = 32))
      val uart = master(Uart())
    }

    // Instanciate an simple uart controller
    val uartCtrl = new UartCtrl()
    io.uart <> uartCtrl.io.uart

    // Create an instance of the AvalonMMSlaveFactory that will then be used as a slave factory drived by io.bus
    val busCtrl = Apb3SlaveFactory(io.bus)

    // Ask the busCtrl to create a readable/writable register at the address 0
    // and drive uartCtrl.io.config.clockDivider with this register
    busCtrl.driveAndRead(uartCtrl.io.config.clockDivider,address = 0)

    // Do the same thing than above but for uartCtrl.io.config.frame at the address 4
    busCtrl.driveAndRead(uartCtrl.io.config.frame,address = 4)

    // Ask the busCtrl to create a writable Flow[Bits] (valid/payload) at the address 8.
    // Then convert it into a stream and connect it to the uartCtrl.io.write by using an register stage (>->)
    val writeFlow = busCtrl.createAndDriveFlow(Bits(3 bits),address = 8)
    writeFlow.toStream.stage >> uartCtrl.io.write

    // To avoid losing writes commands between the Flow to Stream transformation just above,
    // make the occupancy of the uartCtrl.io.write readable at address 8
    busCtrl.read(uartCtrl.io.write.valid,address = 8)

    // Take uartCtrl.io.read, convert it into a Stream, then connect it to the input of a FIFO of 'rxFifoDepth' elements
    // Then make the output of the FIFO readable at the address 12 by using a non blocking protocol
    // (bit 31 => data valid, bits 7 downto 0 => data)
    val readStream = uartCtrl.io.read.queue(rxFifoDepth)
    busCtrl.readStreamNonBlocking(readStream,address = 12,validBitOffset = 31,payloadBitOffset = 0)
  }
}

object c6669{
  class AvalonUartCtrl(uartCtrlConfig : UartCtrlGenerics, rxFifoDepth : Int) extends Component{
    val io = new Bundle{
      val bus =  slave(AvalonMM(AvalonMMUartCtrl.getAvalonMMConfig))
      val uart = master(Uart())
    }

    val uartCtrl = new UartCtrl(uartCtrlConfig)
    io.uart <> uartCtrl.io.uart

    val busCtrl = AvalonMMSlaveFactory(io.bus)
    //Make clockDivider register
    busCtrl.driveAndRead(uartCtrl.io.config.clockDivider,address = 0)
    //Make frame register
    busCtrl.driveAndRead(uartCtrl.io.config.frame,address = 4)
    //Make writeCmd register
    val writeFlow = busCtrl.createAndDriveFlow(Bits(uartCtrlConfig.dataWidthMax bits),address = 8)
    writeFlow.toStream.stage() >> uartCtrl.io.write
    //Make writeBusy register
    busCtrl.read(uartCtrl.io.write.valid,address = 8)
    //Make read register
    busCtrl.readStreamNonBlocking(uartCtrl.io.read.queue(rxFifoDepth),address = 12,validBitOffset = 31,payloadBitOffset = 0)
  }
}


object c4828{
  class SinusGenerator(resolutionWidth : Int,sampleCount : Int) extends Component {
    val io = new Bundle {
      val sin = out SInt (resolutionWidth bits)
    }

    def sinTable() = (0 until sampleCount).map(sampleIndex => {
      val sinValue = Math.sin(2 * Math.PI * sampleIndex / sampleCount)
      S((sinValue * ((1 << resolutionWidth) / 2 - 1)).toInt, resolutionWidth bits)
    })

    val rom   = Mem(SInt(resolutionWidth bit), initialContent = sinTable())
    val phase = CounterFreeRun(sampleCount)
    val sin   = rom.readSync(phase)
  }

  class MyComponent extends Component {
    val io = new Bundle{
      val a      = in Bool()
      val result = out Bool()
    }
    when(io.a){
      io.result := True
    }
  }

}



object c9482 {

  class TopLevel extends Component {
    def complicatedLogic(input : UInt) = RegNext(RegNext(input))

    val a = UInt(8 bits)
    val b = UInt(8 bits)

    val aCalcResult = complicatedLogic(a)

    val aCalcResultLatency = LatencyAnalysis(a,aCalcResult)
    val bDelayed = Delay(b,cycleCount = aCalcResultLatency)

    val result = aCalcResult + bDelayed


    in(a,b)
    out(result)
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }

}



object CheatSheet{
  //  object myEnum extends SpinalEnum([encoding]){
  //    val IDLE, STATE1 = newElement()
  //  }
  object State extends SpinalEnum{

    val IDLE, S0,S1,S2 = newElement()
  }

  //  val myBool = Bool(4 < 2 )
  val myBool = True
  val myUInt = U(13, 32 bits)
  //  val myBits = B"8'hA3" // h,d,b,x,o
  val myBits = B"0110"
  val itMatch = myBits === M"00--10--"






}

object StreamDemo{

  case class RGB(redWidth : Int,greenWidth : Int,blueWidth : Int) extends Bundle{
    val r = UInt(redWidth bits)
    val g = UInt(greenWidth bits)
    val b = UInt(blueWidth bits)

    def isBlack() : Bool = (r === 0 && g === 0 && b === 0)
  }

  val source,sink = Stream(RGB(5,6,5))
  sink <-< source.throwWhen(source.payload.isBlack())

}

object TimerDemo{
  val apb = Apb3(addressWidth = 8, dataWidth = 32)
  val external = new Bundle{
    val clear,tick = Bool()
  }

  val prescaler = Prescaler(16)
  val timerA = Timer(32)
  val timerB,timerC,timerD = Timer(16)

  val busCtrl = Apb3SlaveFactory(apb)
  val prescalerBridge = prescaler.driveFrom(busCtrl,0x00)

  val timerABridge = timerA.driveFrom(busCtrl,0x40)(
    ticks  = List(True, prescaler.io.overflow),
    clears = List(timerA.io.full)
  )

  val timerBBridge = timerB.driveFrom(busCtrl,0x50)(
    ticks  = List(True, prescaler.io.overflow, external.tick),
    clears = List(timerB.io.full, external.clear)
  )

  val timerCBridge = timerC.driveFrom(busCtrl,0x60)(
    ticks  = List(True, prescaler.io.overflow, external.tick),
    clears = List(timerC.io.full, external.clear)
  )

  val timerDBridge = timerD.driveFrom(busCtrl,0x70)(
    ticks  = List(True, prescaler.io.overflow, external.tick),
    clears = List(timerD.io.full, external.clear)
  )
}


object DemoMusc564{
  case class Apb3Config(addressWidth  : Int,
                        dataWidth     : Int,
                        selWidth      : Int     = 1,
                        useSlaveError : Boolean = true)

  case class Apb3(config: Apb3Config) extends Bundle with IMasterSlave {
    val PADDR      = UInt(config.addressWidth bit)
    val PSEL       = Bits(config.selWidth bits)
    val PENABLE    = Bool()
    val PREADY     = Bool()
    val PWRITE     = Bool()
    val PWDATA     = Bits(config.dataWidth bit)
    val PRDATA     = Bits(config.dataWidth bit)
    val PSLVERROR  = if(config.useSlaveError) Bool() else null

    override def asMaster(): Unit = {
      out(PADDR,PSEL,PENABLE,PWRITE,PWDATA)
      in(PREADY,PRDATA)
      if(config.useSlaveError) in(PSLVERROR)
    }
  }


  val apbConfig = Apb3Config(
    addressWidth  = 16,
    dataWidth     = 32,
    selWidth      = 1,
    useSlaveError = false
  )

//  val io = new Bundle{
//    val apb = slave(Apb3(apbConfig))
//  }
//
//  io.apb.PREADY := True
//  when(io.apb.PSEL(0) && io.apb.PENABLE){
//    //...
//  }
}


object P636{
  class TopLevel extends Component{
    val source, sink = Stream(Rgb(5,6,5))

    source.queue(16) >> sink

    slave(source)
    master(sink)
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}

object P6364{
  class TopLevel extends Component{
    val source, sink = Stream(Rgb(5,6,5))
    val fifo = StreamFifo(
      dataType = Rgb(5,6,5),
      depth = 16
    )
    fifo.io.push << source
    fifo.io.pop >> sink

    slave(source)
    master(sink)
  }
  {
    case class Stream[T <: Data](payloadType : HardType[T]) extends Bundle with IMasterSlave{
      val valid   = Bool()
      val ready   = Bool()
      val payload = payloadType()

      override def asMaster(): Unit = {
        out(valid,payload)
        in(ready)
      }

      def >>(sink: Stream[T]): Unit ={
        sink.valid   := this.valid
        this.ready   := sink.ready
        sink.payload := this.payload
      }
      def <<(source: Stream[T]): Unit = source >> this
      def queue(depth : Int) : Stream[T] = ???
    }

    case class RGB( rWidth : Int,
                    gWidth : Int,
                    bWidth : Int){
      val r = UInt(rWidth bits)
      val g = UInt(gWidth bits)
      val b = UInt(bWidth bits)
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}


object PlayAxiConnect{
  case class Axi4Config(addressWidth : Int,
                        dataWidth    : Int,
                        idWidth      : Int)

  case class Axi4(config: Axi4Config) extends Bundle {
    val ar_valid = Bool()
    val ar_ready = Bool()
    val ar_addr  = UInt(config.addressWidth bits)
    // ...
  }
}

object PlayAxiConnect2{
  case class Axi4Config(addressWidth : Int,
                        dataWidth    : Int,
                        idWidth      : Int)

  case class Axi4(config: Axi4Config) extends Bundle {
    // ...

    def >> (slave : Axi4): Unit = {
      // connect this to slave
      // ...
    }
  }
}



object PlayRgbToGray34{
  // Input RGB color
  val r,g,b = UInt(8 bits)

  // Define a function to multiply a UInt by a scala Float value.
  def coefMul(value : UInt,by : Float) : UInt = {
    val resultReg = Reg(UInt(8 bits))
    resultReg := value * U((255*by).toInt,8 bits) >> 8
    return resultReg
  }

  //Calculate the gray level
  val gray = coefMul(r, 0.3f) +
    coefMul(g, 0.4f) +
    coefMul(b, 0.3f)
}



object TrololOOP{
  abstract class BusSlaveFactory {
    def read(that: Data,
             address: BigInt,
             bitOffset: Int): Unit

    def write[T <: Data](that: T,
                         address: BigInt,
                         bitOffset: Int): T

    def onWrite(address: BigInt)(doThat: => Unit): Unit
    def onRead(address: BigInt)(doThat: => Unit): Unit

    def createReadWrite[T <: Data](dataType: T,
                                   address: BigInt,
                                   bitOffset : Int = 0): T = {
      val reg = Reg(dataType)
      write(reg,address,bitOffset)
      read(reg,address,bitOffset)
      reg
    }
  }

  class Apb3SlaveFactory(bus: Apb3) extends BusSlaveFactory{
    override def read(that: Data, address: BigInt, bitOffset: Int): Unit = {???}
    override def write[T <: Data](that: T, address: BigInt, bitOffset: Int): T = {???}
    override def onWrite(address: BigInt)(doThat: => Unit): Unit = {???}
    override def onRead(address: BigInt)(doThat: => Unit): Unit = {???}
  }




  val cpuConfig = RiscvCoreConfig(
    pcWidth = 32,
    addrWidth = 32,
    startAddress = 0x00000000,
    branchPrediction = dynamic,
    bypassExecute0 = true,
    bypassExecute1 = true,
    bypassWriteBack = true
    // ...
  )

  cpuConfig.add(new MulExtension)
  cpuConfig.add(new DivExtension)
  cpuConfig.add(new BarrelShifterFullExtension)
}

object SementicAA{
  val inc, clear = Bool()
  val counter = Reg(UInt(8 bits))

  when(inc){
    counter := counter + 1
  }
  when(clear){
    counter := 0
  }
}

object SementicAB{
  val inc, clear = Bool()
  val counter = Reg(UInt(8 bits))

  def setCounter(value : UInt): Unit = {
    counter := value
  }

  when(inc){
    setCounter(counter + 1)
  }
  when(clear){
    counter := 0
  }
}


object SementicAC{
  val inc, clear = Bool()
  val counter = Reg(UInt(8 bits))

  def setCounterWhen(cond : Bool,value : UInt): Unit = {
    when(cond) {
      counter := value
    }
  }

  setCounterWhen(cond = inc,   value = counter + 1)
  setCounterWhen(cond = clear, value = 0)
}

object SementicAD{
  val inc, clear = Bool()
  val counter = Reg(UInt(8 bits))

  def setSomethingWhen(something : UInt,cond : Bool,value : UInt): Unit = {
    when(cond) {
      something := value
    }
  }

  setSomethingWhen(something = counter, cond = inc,   value = counter + 1)
  setSomethingWhen(something = counter, cond = clear, value = 0)
}


object PlayAxiLiteFactory{
  //Create a new AxiLite4 bus
  val axiLiteConfig = AxiLite4Config(
    addressWidth =  12,
    dataWidth = 32
  )
  val bus = AxiLite4(axiLiteConfig)

  //Create the factory which is able to create some bridging logic between the bus and some hardware
  val factory = new AxiLite4SlaveFactory(bus)

  //Create 'a' and 'b' as write only register
  val a = factory.createWriteOnly(UInt(32 bits), address = 0)
  val b = factory.createWriteOnly(UInt(32 bits), address = 4)

  //Do some calculation
  val result = a * b

  //Make 'result' readable by the bus
  factory.read(result(31 downto 0), address = 8)
}

object PlayAdder2{
  import spinal.core._

  class Adder(width : Int) extends Component{
    val io = new Bundle {
      val a,b = in UInt(width bits)
      val result = out UInt(width bits)
    }
    val a_add_b = UInt(width bits)
    a_add_b := io.a + io.b
    io.result := a_add_b
  }

  def dummy2: Unit ={
    val adder8 = new Adder(8)
    adder8.io.a := 21
    adder8.io.b := 21
    when(adder8.io.result > 32){
      //...
    }


    val landaLogic = new Area{
      val a,b = UInt(8 bits)
      val result = a + b
      val flag = False
    }

    val somethereElse = new Area{
      val flag = False || landaLogic.result.lsb
    }

    when(landaLogic.result === 88){
      somethereElse.flag := False
    }
  }

  class Yolo(width : Int) extends Component{
    val myBool = False
    when(in.Bool().setName("something")){
      myBool := True
    }

    myBool.asOutput()

    val myUInt = out UInt(8 bits)
    myUInt := ((3 downto 0) -> myBool,default -> true)


  }
  def sinus(size:Int, res:BitCount) = {
    (0 to size).map (i =>
      U(((Math.sin(i)+1) * Math.pow(2,res.value)/2).toInt)
    )
  }
  // memory initialised with a sinus
  val mySinus = Mem(UInt(16 bits), sinus(1024, 16 bits))
  def dummy() = {
    val uint8 = UInt(8 bits)
    val uint12 = UInt(8 bits)
    uint8 := uint12 //Error
    uint12 := uint8 //Error
    uint8 := uint12.resize(8)
    uint8 := uint12.resized
    uint8 := uint12(7 downto 0)
    uint8 := uint12(uint8.range)

    val bits4 = Bits(4 bits)
    uint8 := bits4.asUInt.resized

    case class RGB(channelWidth : Int) extends Bundle{
      val r,g,b = UInt(channelWidth bits)
    }

    val bitsIn, bitsOut = Bits(12 bits)
    val color = RGB(4)
    color.assignFromBits(bitsIn)
    bitsOut := color.asBits

    val something,somethingElse = Bool()

    val result = UInt(8 bits)
    result := 0
    when(something){
      result := 42
      when(somethingElse){
        result := 0xEE
      }
    }

    val clearFlag = False

    val counter = Reg(UInt(8 bits))
    when(something){
      counter := counter + 1
    }
    when(clearFlag){
      counter := 0
    }

    val softReset = False

    when(softReset){
      counter := 0
      result := 0x88
    }



    val isZero = Bool()
    val shifted = UInt(9 bits)
    val square  = UInt(16 bits)
    def load(value : UInt) : Unit = {
      require(widthOf(value) == 8)
      isZero := value === 0
      shifted := value << 1
      square := value * value
    }

    when(something){
      load(U"1110_0000")
    } otherwise {
      load(U"0010_0010")
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new Adder(8))
    SpinalVerilog(new Adder(8))
    SpinalVhdl(new Yolo(8))
  }

//  {
//    val myBool = Bool()
//    myBool := False
//    myBool := True
//    myBool := Bool(4 > 7)
//
//    val myUInt = UInt(8 bits)
//    myUInt := "0001_1100"
//    myUInt := "xEE"
//    myUInt := 42
//    myUInt := U(54, 8 bits)
//    myUInt :=((3 downto 0) -> myBool, default -> true)
//    when(myUInt === U(myUInt.range -> true)) {
//      myUInt(3) := False
//    }
//
//    val myConstant = U"0001_1100"
//  }
//
//  {
//    val myBool = False
//    when(???){
//      myBool := True
//    }
//  }
}



object Summit extends App{
  import spinal.core._
  import spinal.core.sim._
  import spinal.lib._
  import spinal.lib.pipeline._

  SpinalVerilog(new Component{

    val pcIn = in(UInt(32 bits))
    val pcOut = out(UInt(32 bits))

    val pip = new Pipeline{
      val A, B, C = new Stage

      connect(A, B)(Connection.M2S())
      connect(B, C)(Connection.M2S())

      val PC = A.insert(pcIn)
      val PC_NEXT = B.insert(B(PC) + 4)
      pcOut := C(PC_NEXT)

      build()
    }
  })

  SpinalVerilog(new Component{

    val pcIn = in(UInt(32 bits))
    val pcOut = out(UInt(32 bits))

    val pip = new Pipeline{
      val A = new Stage{
        val PC = insert(pcIn)
      }

      val B = new Stage(Connection.M2S()){
        when(A.PC > 4){
          throwIt()
        }
      }

      val C = new Stage(Connection.M2S()){
        pcOut := A.PC
      }

      build()
    }
  })



  SimConfig.withFstWave.compile(new Component {
    val pcIn = slave Stream(UInt(32 bits))
    val pcOut = out(UInt(32 bits))

    val pip = new Pipeline {
      val A = new Stage {
        valid := pcIn.valid
        val PC = insert(pcIn.payload)
        pcIn.ready := isReady
      }

      val B = new Stage(Connection.M2S()) {
        when(A.PC > 4) {
//          haltIt()
        }
      }

      val C = new Stage(Connection.M2S()) {
        pcOut := A.PC
      }

      def compose() = new Area{
        val X = Stageable(Bool())
        A(X) := False
        C(X)
      }

      val C1 = compose()
      val C2 = compose()

      val miaou = Bool().setLambdaName(C1.X.isNamed)(s"wuff_${C1.X.getName()}_rawrrr")
      println(C1.X.getName())

      build()
    }
  }).doSim{dut =>
    dut.clockDomain.forkStimulus(10)
    for(i <- 0 until 10){
      dut.pcIn.valid #= true
      dut.pcIn.payload #= i
      dut.clockDomain.waitSamplingWhere(dut.pcIn.ready.toBoolean)
    }
  }


}


object Ccc37A {

  class Timer extends Component {
    val increment = in Bool()
    val counter = Reg(UInt(8 bits)) init(0)
    val full = counter === 255

    when(increment){
      counter := counter + 1
    }
  }

}