package spinal.tester.code

import spinal.core._
import spinal.debugger.LogicAnalyserBuilder
import spinal.lib._

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
      val conds = in Vec(2, Bool)
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
      val sources = in Vec(sourceCount, Color(channelWidth))
      val result = out Bits (3 * channelWidth bit)
    }
    val selectedSource = io.sources(io.sel)
    io.result := toBits(selectedSource)
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
      val sources = in Vec(sourceCount, Color(channelWidth))
      val result = out(Color(channelWidth))
    }

    var sum = io.sources(0)
    for (i <- 1 until sourceCount) {
      sum = sum + io.sources(i)
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
      val inputs = Vec(portCount, slave Stream (dataType))
      val output = master Stream (dataType)
      val chosen = out UInt (log2Up(portCount) bit)
    } //...
  }

  class StreamFork[T <: Data](dataType: T, portCount: Int) extends Component {
    val io = new Bundle {
      val input = slave Stream (dataType)
      val output = Vec(portCount, master Stream (dataType))
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
    io.memoryReadAddress.data := toBits(addressCounter)

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

    override def asMaster: this.type = out(this)
    override def asSlave: this.type = in(this)
  }

  abstract case class Stream[T <: Data](dataType: T) extends Bundle with IMasterSlave {
    val valid = Bool
    val ready = Bool
    val data: T = cloneOf(dataType)

    override def asMaster: this.type = {
      out(valid, data)
      in(ready)
      this
    }
    override def asSlave: this.type = asMaster.flip


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

  //  val uartCtrl = new UartCtrl()
  //  uartCtrl.read >> logicAnalyser.io.slavePort
  //  uartCtrl.write << logicAnalyser.io.masterPort
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

    assert(3 == latencyAnalysis(io.slavePort.data, io.masterPort.data))
    assert(2 == latencyAnalysis(io.masterPort.ready, io.slavePort.ready))
  }
}


object C13 {

  object MyEnum extends SpinalEnum {
    val state0, state1, anotherState = Value
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
  when(timeout){ //implicit conversion to Bool
    timeout.clear //Clear the flag and the internal counter
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
    override def asMaster: this.type = {
      out(valid)
      in(ready)
      out(data)
      this
    }

    override def asSlave: this.type = asMaster.flip //.flip reverse all signal direction
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
      val srcPort = Vec(srcCount, slave(Stream(rgbType)))
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