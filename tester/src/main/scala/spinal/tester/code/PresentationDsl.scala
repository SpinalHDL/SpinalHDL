package spinal.tester.code

import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.bus.amba4.axi.Axi4CrossbarFactory
import spinal.lib.memory.sdram.sdr.MT48LC16M16A2

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/**
 * Created by PIC32F_USER on 13/11/2016.
 */
object IntroHdl{
  import spinal.core._

  class MyComponent extends Component {
    val io = new Bundle {
      val a           = in    Bool
      val b           = in    Bool
      val c            = in    Bool
      val result   = out Bool
    }
    val a_and_b = io.a & io.b
    val not_c = !io.c
    io.result := a_and_b | not_c
  }

  def main(args: Array[String]) {
    SpinalVhdl(new MyComponent)
  }
}



object PresentationDSL{



  object AST{
    import spinal.core._

    val a,b,c  = Bool
    val result = Bool
    result := (a || b) && c
  }

  object AST2{
    class Node{
      val inputs = ArrayBuffer[Node]()
    }
    class Bool extends Node{
      def &&(right : Bool) : Bool = ???
      def ||(right : Bool) : Bool = ???
      def unary_! : Bool = ???
    }
    class And extends Node
    class Or extends Node
    class Not extends Node

  }

  object AST3{
    import spinal.core._
    val bools = List.fill(3)(in Bool)

    var boolsAnd = bools.head
    for(bool <- bools.tail){
      boolsAnd = boolsAnd && bool
    }

    val output = out Bool()
    output := boolsAnd

  }


  object BuilderPattern{

  }

  object CurryingAndBlocks{
    import spinal.core._

    object when {
      def apply(cond: Bool)(block: => Unit): Unit = {
        //...
      }
    }
    val something, data, result = Bool
    result := False
    when(something){
      result := data
    }
  }

  object Otherwise{
    import spinal.core._

    class When(context : Bool){
      def otherwise(block : => Unit) : Unit =  {
        //...
      }
    }

    object when {
      def apply(cond: Bool)(block: => Unit): When = {
        //...
        new When(cond)
      }
    }

    when(???){
      //...
    } otherwise {
      //...
    }
  }

  object Context{
    object when {
      var context = List[Bool]()
      def apply(cond: Bool)(block: => Unit): Unit = {
        context = cond :: context
        block
        context = context.tail
      }
    }

    class Node
    class Bool extends Node{
      def := (that : Bool): Unit ={
        // use when.context
      }
    }
  }

  object StringInterpolation{
    implicit class LiteralBuilder(sc: StringContext) {
      def x(args: Any*): BigInt = BigInt(args(0).asInstanceOf[String].replace("_",""),16)
    }

    val literal = x"CAFE_F00D"
  }

  object PimpMyLibraryPattern{
    implicit class StringPimper(pimped : String){
      def countUpperCases() : Int = pimped.count(_.isUpper)
    }

    "ChewBacca".countUpperCases()   //2


  }

  object Prefix{
    import spinal.core.BitCount
    import spinal.core._


    class BaseType{
      def asInput() : this.type = ???
    }

    class Bool extends BaseType
    class UInt(width : Int) extends BaseType

    object in{
      def Bool() = new Bool().asInput()
      def UInt(width : BitCount) = new UInt(width.value).asInput()
    }

    val a = in Bool()
    val b = in UInt(4 bits)


    val c = Bool().asInput()
  }


  object Postfix{
    case class BitCount(value : Int)
    implicit class IntPimper(pimped : Int){
      def bits = BitCount(pimped)
    }

    class UInt(width : Int)
    object UInt{
      def apply(width : BitCount) = new UInt(width.value)
    }
    
    val a,b = UInt(4 bits)
  }

  object VarOperator{
    case class MegaInt(value : Int){
      def +(that : MegaInt) = MegaInt(this.value + that.value)
    }

    var a = MegaInt(666)
    a     += MegaInt(22) //a = MegaInt(666 + 22)
  }

  object DataTypesWrong {
    class Bundle {
      def :=(that: this.type) = ??? //Can't do that
    }
  }

  object DataTypes{
    import spinal.core._

    implicit class BundlePimper[T <: Bundle](pimped : T){
      def :=(that : T) = ???
    }

    class RGB extends Bundle{
      val r,g,b = UInt(8 bits)
    }

    val a,b = new RGB
    a := b  //It is strongly typed and only accept RGB := RGB
  }

  object DataTypes2 {
    class Data
    class Bundle[T] extends Data{
      def := (that : T) : Unit = ???
    }
    class RGB extends Bundle[RGB]
  }

  object DataTypes3 {
    class Data
    class Bundle extends Data{
      type T <: Bundle
      def := (that : T) : Unit = ???
    }
    class RGB extends Bundle{
      override type T = RGB
    }
  }
  object Color{
    import spinal.core._
    case class Color(channelWidth: Int) extends Bundle {
      val r,g,b = UInt(channelWidth bits)

      def +(that: Color): Color = {
        val result = Color(channelWidth)

        result.r := this.r + that.r
        result.g := this.g + that.g
        result.b := this.b + that.b

        return result
      }
    }

    val a,b = Color(8)
    val c = a + b
    val d = Color(8)
    d := c

    def foo(i : Int) = 3
    foo{3}
  }




  object Pinsec{
    
  }
}


object PresentationSymbolic{
  import spinal.core._
  import spinal.lib._
  import spinal.lib.bus.amba3.apb._
//  val x = Reg(Bool)
//  val x = Reg(Bool) init(False)
  val a,b,c,d,e = Bool
//  val x = Reg(Bool)
//  x := a
//  when(d){
//    when(e){
//      x := b
//    } otherwise {
//      x := c
//    }
//  }
//val x = ~a
//  val x = a && b
//  val x = a && (b || c)
//  val tmp = b || c
//  val x = a && tmp
//  val x = Mux(c, b, c)
//  val x = c ? b |c
//  val x = Bool()
//  when(c){
//    x := b
//  } otherwise {
//    x := a
//  }
//
//  val x = ~a
//  val y = Bool()
//  y := x
//  val sel = UInt(2 bits)
//  val x = sel.mux(
//    0 -> a,
//    1 -> b,
//    2 -> c,
//    3 -> d
//  )
//  val vec = Vec(a,b,c,d)
//  val x = vec(sel)
//  val x = Bool()
//  switch(sel){
//    is(0) {x := a}
//    is(1) {x := b}
//    is(2) {x := c}
//    is(3) {x := d}
//  }
  class Pwm(width : Int) extends Component{
    val io = new Bundle{
      val enable    = in Bool
      val dutyCycle = in UInt(width bits)
      val pwm       = out Bool
    }
    // ...
  }
//  class ApbFifo(packetWidth : Int,
//                fifoDepth : Int) extends Component{
//    val io = new Bundle{
//      val apb = slave(Apb3(
//        addressWidth = 32,
//        dataWidth = 32
//      ))
//      val pop = master(Stream(Bits(packetWidth bits)))
//      val pwm       = out Bool
//    }
//    // ...
//  }
  case class RGB(channelWidth : Int) extends Bundle{
    val r,g,b = UInt(channelWidth bits)
  }

  case class MemoryPort( addressWidth : Int,
                         dataWidth : Int) extends Bundle with IMasterSlave {
    val enable    = Bool
    val rwn       = Bool
    val address   = Bits(addressWidth bits)
    val writeData = Bits(dataWidth bits)
    val readData  = Bits(dataWidth bits)

    override def asMaster(): Unit = {
      out(enable,rwn,address,writeData)
      in(readData)
    }
  }

  val src = RGB(8)
  val dst = Reg(RGB(channelWidth = 8))
  dst := src


  class MappedFifo(packetWidth : Int,
                   fifoDepth : Int) extends Component{
    val io = new Bundle{
      val apb = slave(MemoryPort(
        addressWidth = 32,
        dataWidth = 32
      ))
      val pop = master(Stream(Bits(packetWidth bits)))
    }
    // ...
  }


  class SubComponent extends Component{
    val io = new Bundle {
      val dutyCycle = out UInt(16 bits)
    }
    io.dutyCycle := 42
  }

  class Toplevel extends Component{
    val io = new Bundle{
      val pin = out Bool
    }

    val subComponent = new SubComponent

    val ctrl = new Pwm(width = 10)
    ctrl.io.enable := True
    ctrl.io.dutyCycle := subComponent.io.dutyCycle << 6
    ctrl.io.pwm <> io.pin
  }
//case class Apb3(config: Apb3Config) extends Bundle with IMasterSlave {
//
//  val PADDR      = UInt(config.addressWidth bits)
//  val PSEL       = Bits(config.selWidth bits)
//  val PENABLE    = Bool
//  val PREADY     = Bool
//  val PWRITE     = Bool
//  val PWDATA     = Bits(config.dataWidth bits)
//  val PRDATA     = Bits(config.dataWidth bits)
//  val PSLVERROR  = if(config.useSlaveError) Bool else null
//
//  override def asMaster(): Unit = {
//    out(PADDR, PSEL, PENABLE, PWRITE, PWDATA)
//    in(PREADY, PRDATA)
//    if(config.useSlaveError) in(PSLVERROR)
//  }

}


object Wosh{
  import spinal.core._


//  {
//
//    import spinal.core._
//
//    //Component is like Module in Verilog
//    class MyToplevel extends Component{
//      //IO definition
//      val a,b    = in  UInt(8 bits)
//      val result = out UInt(8 bits)
//
//      //Behaviour
//      result := a + b
//    }
//
//    //Scala main used to ask SpinalHDL to generate the Verilog of MyToplevel
////    object Main extends App{
////      SpinalVerilog(new MyToplevel)
////    }
//  }

//  {
//    import spinal.core._
//
//    class MyToplevel extends Component {
//      //...
//      val counter = Reg(UInt(8 bits))
//      val full = counter === 255
//      when(!full) {
//        counter := counter + 1
//      }
//      //...
//    }
//
//
//
//  }
//
//  {
//    val cond = Bool()
//    val a, b, c = Reg(UInt(8 bits))
//    when(cond) {
//      a := a + 1
//      b := b + 1
//      c := c + 1
//    }
//  }

  {
    val cond = Bool()
    val a, b, c = Reg(UInt(8 bits))
    val regs = ArrayBuffer[UInt]()
    regs += a
    regs += b
    regs += c
    when(cond) {
      for(reg <- regs) {
        reg := reg + 1
      }
    }
  }

  {
    val bus = Apb3(addressWidth = 16, dataWidth = 32)
    val regA, regB = Reg(Bits(32 bits))

    val doWrite = bus.PSEL(0) && bus.PENABLE && bus.PWRITE
    switch(bus.PADDR){
      is(0x00){
        when(doWrite){ regA := bus.PWDATA }
        bus.PRDATA := regA
      }
      is(0x04){
        when(doWrite){ regB := bus.PWDATA }
        bus.PRDATA := regB
      }
    }
  }

  {
    val bus = Apb3(addressWidth = 16, dataWidth = 32)
    val regA, regB = Reg(Bits(32 bits))

    case class Mapping(address : Int, data : Bits)
    val spec = ArrayBuffer[Mapping]()

    spec += Mapping(0x00, regA)
    spec += Mapping(0x04, regB)

    val doWrite = bus.PSEL(0) && bus.PENABLE &&  bus.PWRITE
    switch(bus.PADDR){
      for(mapping <- spec){
        is(mapping.address){
          when(doWrite){ mapping.data := bus.PWDATA }
          bus.PRDATA := mapping.data
        }
      }
    }
  }


  {

    case class Mapping(address: Int, data: Bits)

    case class Apb3Mapper(bus : Apb3) {
      val spec = ArrayBuffer[Mapping]()

      def flush() {
        val doWrite = bus.PSEL(0) && bus.PENABLE && bus.PWRITE
        switch(bus.PADDR) {
          for (mapping <- spec) {
            is(mapping.address) {
              when(doWrite) {
                mapping.data := bus.PWDATA
              }
              bus.PRDATA := mapping.data
            }
          }
        }
      }
    }

    val bus = Apb3(addressWidth = 16, dataWidth = 32)
    val regA, regB = Reg(Bits(32 bits))

    val mapper = Apb3Mapper(bus)
    mapper.spec += Mapping(0x00, regA)
    mapper.spec += Mapping(0x04, regB)

    mapper.flush()
  }

//  {
//
//    case class Apb3Mapper(bus: Apb3) {
//      // ...
//      def readWrite(address: Int, data: Bits) = spec += Mapping(address, data)
//      // ...
//    }
//
//    val bus = Apb3(addressWidth = 16, dataWidth = 32)
//    val regA, regB = Reg(Bits(32 bits))
//
//    val mapper = Apb3Mapper(bus)
//
//    mapper.readWrite(0x00, regA)
//    mapper.readWrite(0x04, regB)
//
//    mapper.flush()
//  }

//  {
//
//    case class Apb3Mapper(bus : Apb3) {
//      // ...
//      def createReadWrite(address : Int) : Bits = {
//        val reg = Reg(Bits(32 bits))
//        spec += Mapping(address, reg)
//        return reg
//      }
//      // ...
//    }
//
//    val bus = Apb3(addressWidth = 16, dataWidth = 32)
//    val mapper = Apb3Mapper(bus)
//
//    val regA = mapper.createReadWrite(0x00)
//    val regB = mapper.createReadWrite(0x04)
//
//    mapper.flush()
//  }

//  {
//    import spinal.lib.fsm._
//
//    class Ctrl extends StateMachine{
//      val stateA = new State with EntryPoint
//      val stateB = new State
//
//      val counter = Reg(UInt(8 bits)) init (0)
//
//      stateA.whenIsActive (goto(stateB))
//
//      stateB.onEntry(counter := 0)
//      stateB.whenIsActive {
//        counter := counter + 1
//        when(counter === 4){
//          goto(stateA)
//        }
//      }
//    }
//  }

//  {
//
//    val sdramCtrl = Axi4SharedSdramCtrl(
//      axiDataWidth = 32,
//      axiIdWidth   = 4,
//      layout       = MT48LC16M16A2.layout,
//      timing       = MT48LC16M16A2.timings,
//      CAS          = 3
//    )
//
//
//    val axiCrossbar = Axi4CrossbarFactory()
//
//    axiCrossbar.addSlaves(
//      ram.io.axi       -> (0x80000000L,   32 KiB),
//      sdramCtrl.io.axi -> (0x40000000L,   16 MiB),
//      apbBridge.io.axi -> (0xF0000000L,    1 MiB)
//    )
//
//    axiCrossbar.addConnections(
//      core.iBus       -> List(ram.io.axi,  sdramCtrl.io.axi),
//      core.dBus       -> List(ram.io.axi,  sdramCtrl.io.axi, apbBridge.io.axi),
//      vgaCtrl.io.axi  -> List(             sdramCtrl.io.axi)
//    )
//
//    axiCrossbar.build()
//  }
}