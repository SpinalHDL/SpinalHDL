About SpinalHDL
============



Getting started
===============
## SBT

```scala
scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  "com.github.spinalhdl" % "spinalhdl-core_2.11" % "latest.release",
  "com.github.spinalhdl" % "spinalhdl-lib_2.11" % "latest.release"
)
```

## JAR

    https://oss.sonatype.org/content/groups/public/com/github/spinalhdl/spinalhdl-core_2.11/
    https://oss.sonatype.org/content/groups/public/com/github/spinalhdl/spinalhdl-lib_2.11/

Examples
===============
## Simple component

```scala
import spinal.core._

class MyTopLevel extends Component {
  val io = new Bundle {
    val a = in Bool()
    val b = in Bool()
    val c = out Bool()
  }
  io.c := io.a & io.b
}

object MyTopLevel {
  def main(args: Array[String]) {
    SpinalVhdl(new MyTopLevel)
  }
}
```

## Carry adder

```scala
import spinal.core._

class CarryAdder(size : Int) extends Component{
  val io = new Bundle{
    val a = in UInt(size bit)
    val b = in UInt(size bit)
    val result = out UInt(size bit)      //result = a + b
  }

  var c = Bool(false)                   //Carry, like a VHDL variable
  for (i <- 0 until size) {
    val a = io.a(i)
    val b = io.b(i)
    io.result(i) := a ^ b ^ c
    c = (a & b) | (a & c) | (b & c);    //variable assignment
  }
}


object CarryAdderProject {
  def main(args: Array[String]) {
    SpinalVhdl(new CarryAdder(4))
  }
}
```

## Counter with parity

```scala
class CounterWithParity(size : Int) extends Component{
  val io = new Bundle{
    val increment = in Bool()
    val value = out UInt(size bit)
    val evenParity = out Bool()
  }

  val counter = RegInit(UInt(0,size bit))
  when(io.increment){
    counter := counter + UInt(1)
  }
  io.evenParity := counter.toBools.reduceLeft(_ ^ _)    //Get all bit of counter and then xor them together

  io.value := counter
}
```

## Interface / Custom data types / Multi clock system

```scala

import spinal.core._
import spinal.lib._

//Define custom data types
class MyDataType extends Bundle{
  val a = UInt(8 bit)
  val b = Bool()
}

class MultiClockTopLevel extends Component {
  val io = new Bundle {
    val clkA = in Bool()
    val resetA = in Bool()
    val clkB = in Bool()
    val resetB = in Bool()

    //Create handshake interface (valid, ready, data) to transport MyDataType data
    val slaveInteface = slave Handshake(new MyDataType)
    val masterInterface = master Handshake(new MyDataType)
  }

  //Create clock domains from inputs clocks and resets
  val clockDomainA = ClockDomain(io.clkA,io.resetA)
  val clockDomainB = ClockDomain(io.clkB,io.resetB)

  //Create a fifo able to cross clock domain a handshake of MyDataType
  val fifo = new HandshakeFifoCC(new MyDataType,16,clockDomainA,clockDomainB)
  fifo.io.push << io.slaveInteface    //Easy connection provided by Handshake library
  fifo.io.pop >> io.masterInterface
}


object MultiClockTopLevel {
  def main(args: Array[String]) {
    SpinalVhdl(new MultiClockTopLevel)
  }
}
```

## Dual clock FIFO

```scala
import spinal.core._
import spinal.lib._

class HandshakeFifoCCIo[T <: Data](dataType: T, depth: Int) extends Bundle {
  val push = slave Handshake (dataType)
  val pop = master Handshake (dataType)
  val pushOccupancy = out UInt (log2Up(depth) + 1 bit)
  val popOccupancy = out UInt (log2Up(depth) + 1 bit)
}

class HandshakeFifoCC[T <: Data](dataType: T, depth: Int, pushClockDomain: ClockDomain, popClockDomain: ClockDomain) extends Component {
  assert(isPow2(depth))
  assert(depth >= 2)

  val io = new HandshakeFifoCCIo(dataType, depth)

  val ptrWidth = log2Up(depth) + 1
  def isFull(a: Bits, b: Bits) = a(ptrWidth - 1, ptrWidth - 2) === ~b(ptrWidth - 1, ptrWidth - 2) && a(ptrWidth - 3, 0) === b(ptrWidth - 3, 0)
  def isEmpty(a: Bits, b: Bits) = a === b

  val ram = Mem(dataType, depth)

  val popToPushGray = Bits(ptrWidth bit)
  val pushToPopGray = Bits(ptrWidth bit)

  val pushCC = new ClockingArea(pushClockDomain) {
    val pushPtr = Counter(depth << 1)
    val pushPtrGray = RegNext(toGray(pushPtr.valueNext))
    val popPtrGray = BufferCC(popToPushGray, Bits(0))
    val full = isFull(pushPtrGray, popPtrGray)

    io.push.ready := !full
    when(io.push.fire) {
      ram(pushPtr) := io.push.data
      pushPtr ++
    }

    io.pushOccupancy := pushPtr - fromGray(popPtrGray)
  }

  val popCC = new ClockingArea(popClockDomain) {
    val popPtr = Counter(depth << 1)
    val popPtrGray = RegNext(toGray(popPtr.valueNext))
    val pushPtrGray = BufferCC(pushToPopGray, Bits(0))
    val empty = isEmpty(popPtrGray, pushPtrGray)

    io.pop.valid := !empty
    io.pop.data := ram.readSyncCC(popPtr.valueNext)
    when(io.pop.fire) {
      popPtr ++
    }

    io.popOccupancy := fromGray(pushPtrGray) - popPtr
  }

  pushToPopGray := pushCC.pushPtrGray
  popToPushGray := popCC.popPtrGray
}
```

Other consideration
===============
Intellij scala plugin has some syntax highlight bug. Please use scala plugin >= 1.4
