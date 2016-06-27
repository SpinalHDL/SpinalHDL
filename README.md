About SpinalHDL
============
SpinalHDL is a scala library that allows the user to describe his digital hardware and then generate the corresponding VHDL/Verilog file.
## Advantages over VHDL/Verilog
- No restriction to the genericity of your hardware description by using Scala constructs
- No more endless wiring. Create and connect complex buses like AXI in one line.
- Evolving capabilities. Create your own buses definition and abstraction layer.
- Reduce code size by a high factor, especially for wiring. Allowing you to have a better visibility, more productivity and fewer headaches.
- Free and user friendly IDE. Thanks to scala world for auto-completion, error highlight, navigation shortcut and many others
- Extract information from your digital design and then generate files that contain information about some latency and addresses
- Bidirectional translation between any data type and bits. Useful to load a complex data structure from a CPU interface.
- Check for you that there is no combinational loop / latch
- Check that there is no unintentional cross clock domain

Getting started
===============
## Links
- Documentation                  <br> http://spinalhdl.github.io/SpinalDoc/
- Presentation of the language   <br> http://spinalhdl.github.io/SpinalDoc/presentation/
- SBT base project               <br> https://github.com/SpinalHDL/SpinalBaseProject

[![Join the chat at https://gitter.im/SpinalHDL/SpinalHDL](https://badges.gitter.im/SpinalHDL/SpinalHDL.svg)](https://gitter.im/SpinalHDL/SpinalHDL?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![Build Status](https://travis-ci.org/SpinalHDL/SpinalHDL.svg?branch=master)](https://travis-ci.org/SpinalHDL/SpinalHDL)

## SBT

```scala
scalaVersion := "2.11.6"

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

## Basics
- Base Types : Bool, Bits, UInt, SInt, Enumeration
- Bundle : That allow you to describe a data structure with the possibility for each element to specify the direction (in,out). That is useful to describe bus
- Reg : Create a register
- Vec : That allow you to create an array of data
- Mem : Give the possibility to instantiate memory
- BlackBox : Allow you to instantiate a third party HDL component

## Simple component

```scala
// spinal.core contain all basics (Bool, UInt, Bundle, Reg, Component, ..)
import spinal.core._  

//A simple component definition
class MyTopLevel extends Component {
  //Define some input/output. Bundle like a VHDL record or a verilog struct.
  val io = new Bundle {
    val a = in Bool
    val b = in Bool
    val c = out Bool
  }
  
  //Define some asynchronous logic
  io.c := io.a & io.b
}

//This is the main of the project. It create a instance of MyTopLevel and 
//call the SpinalHDL library to flush it into a VHDL file.
object MyMain {
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

  var c = False                   //Carry, like a VHDL variable
  for (i <- 0 until size) {
	//Create some intermediate value in the loop scope.
    val a = io.a(i)	 
    val b = io.b(i)  
	
	//The carry adder's asynchronous logic
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
    val increment = in Bool
    val value = out UInt(size bit)
    val evenParity = out Bool
  }

  //Create a register of UInt(size-1 downto 0) 
  //init(0) force it to zero when a reset occur
  //In Spinal, you don't have to play with clock and reset signals each time
  //  you create register. You define a clock domain area and this is done.
  val counter = Reg(UInt(size bit)) init(0)
  when(io.increment){
    counter := counter + 1
  }
  
  //Get all bit of counter and then xor them together
  //toBools create a vector of Bool from each bit of counter
  //reduceLeft is a scala function that mix all bit together 
  //  from the left to the right
  io.evenParity := counter.toBools.reduceLeft(_ ^ _)   

  io.value := counter
}
```


## Why not Chisel ?
For many reason : http://spinalhdl.github.io/SpinalDoc/#what-are-the-differences-between-chisel-vs-spinal-

Other consideration
===============
Intellij scala plugin has some syntax highlight bug. As far i know, Intellij 14.1 is the best version. 

