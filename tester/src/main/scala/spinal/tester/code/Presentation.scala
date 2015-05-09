package spinal.tester.code

import spinal.core._
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

  case class Handshake[T <: Data](dataType: T) extends Bundle {
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
  class HandshakeFifo[T <: Data](dataType: T, depth: Int) extends Component {
    val io = new Bundle {
      val pushPort = slave Handshake (dataType)
      val popPort = master Handshake (dataType)
      val occupancy = out UInt (log2Up(depth + 1) bit)
    } //...
  }

  class HandshakeArbiter[T <: Data](dataType: T, val portCount: Int) extends Component {
    val io = new Bundle {
      val inputs = Vec(portCount, slave Handshake (dataType))
      val output = master Handshake (dataType)
      val chosen = out UInt (log2Up(portCount) bit)
    } //...
  }

  class HandshakeFork[T <: Data](dataType: T, portCount: Int) extends Component {
    val io = new Bundle {
      val input = slave Handshake (dataType)
      val output = Vec(portCount, master Handshake (dataType))
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

      val memoryReadAddress = master Handshake(Bits(32 bit))
      val memoryReadData = slave Handshake(Bits(32 bit))

      val colorStream = master Handshake (Color(8))
    }

    io.run.ready := False
    val addressCounter = RegInit(U"x0000")
    when(io.memoryReadAddress.fire){
      addressCounter := addressCounter + 1
      when(addressCounter === U"xFFFF"){
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

  abstract case class Handshake[T <: Data](dataType: T) extends Bundle with IMasterSlave {
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

    def <<(that: Handshake[T]): Unit = {
      this.valid := that.valid
      that.ready := this.ready
      this.data := that.data
    }
    def <-<(that: Handshake[T])
    def </<(that: Handshake[T])
    def &(cond: Bool): Handshake[T]
    def queue(size: Int): Handshake[T]
    def fire: Bool
    def throwWhen(cond: Bool)
  }
}

object C11 {

}