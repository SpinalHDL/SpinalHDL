package spinal.tester.code

import spinal.core.{SpinalTagReady, SpinalVerilog}
import spinal.core.fiber.{Fiber, Handle}
import spinal.lib.IMasterSlave
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4CrossbarFactory, Axi4SpecRenamer}
import spinal.lib.bus.misc.DefaultMapping
import spinal.lib.bus.tilelink.{M2sParameters, M2sSupport, M2sTransfers, S2mSupport, SizeRange}
import spinal.lib.bus.tilelink.fabric.WidthAdapter
import spinal.lib.bus.wishbone.{Wishbone, WishboneConfig, WishboneSlaveFactory}
import spinal.lib.memory.sdram.sdr.MT48LC16M16A2

import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Future, Promise}


/**
 * Created by PIC32F_USER on 13/11/2016.
 */
object IntroHdl{
  import spinal.core._

  class MyComponent extends Component {
    val io = new Bundle {
      val a           = in    Bool()
      val b           = in    Bool()
      val c            = in    Bool()
      val result   = out Bool()
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

    val a,b,c  = Bool()
    val result = Bool()
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
    val bools = List.fill(3)(in Bool())

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
    val something, data, result = Bool()
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




//  object Postfix{
//    case class BitCount(value : Int)
//    implicit class IntPimper(pimped : Int){
//      def bits = BitCount(pimped)
//    }
//
//    class UInt(width : Int)
//    object UInt{
//      def apply(width : BitCount) = new UInt(width.value)
//    }
//
//    val a,b = UInt(4 bits)
//  }

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

//  object DataTypes{
//    import spinal.core._
//
//    implicit class BundlePimper[T <: Bundle](pimped : T){
//      def :=(that : T) = ???
//    }
//
//    class RGB extends Bundle{
//      val r,g,b = UInt(8 bits)
//    }
//
//    val a,b = new RGB
//    a := b  //It is strongly typed and only accept RGB := RGB
//  }

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
//  val x = Reg(Bool())
//  val x = Reg(Bool()) init(False)
  val a,b,c,d,e = Bool()
//  val x = Reg(Bool())
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
      val enable    = in Bool()
      val dutyCycle = in UInt(width bits)
      val pwm       = out Bool()
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
//      val pwm       = out Bool()
//    }
//    // ...
//  }
  case class RGB(channelWidth : Int) extends Bundle{
    val r,g,b = UInt(channelWidth bits)
  }

  case class MemoryPort( addressWidth : Int,
                         dataWidth : Int) extends Bundle with IMasterSlave {
    val enable    = Bool()
    val rwn       = Bool()
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
      val pin = out Bool()
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
//  val PENABLE    = Bool()
//  val PREADY     = Bool()
//  val PWRITE     = Bool()
//  val PWDATA     = Bits(config.dataWidth bits)
//  val PRDATA     = Bits(config.dataWidth bits)
//  val PSLVERROR  = if(config.useSlaveError) Bool() else null
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

object Fsic{
  object Main extends App{
    println("hello world")
  }

  object Main2 extends App{
    val writer = new PrintWriter(new File("toplevel.v" ))
    writer.write("module miaou{\n")
    writer.write("  input  clk,\n")
    //...
    writer.close()
  }

  import spinal.core._
  object Main3 extends App{
    SpinalVerilog(
      new Module{
        val a,b = in UInt(8 bits)
        val result = out(a + b)
      }
    )
  }


  import spinal.lib._

  object Main4 extends App{
    SpinalVerilog(
      new Module{
        val bus = slave(Wishbone(
          addressWidth = 8,
          dataWidth = 32
        ))

        val mapper = WishboneSlaveFactory(bus)
        val regA = mapper.createWriteOnly(
          dataType = UInt(8 bits),
          address = 0x0
        )
        mapper.read(
          that    = regA,
          address = 0x4
        )
      }
    )
  }
}


object Main3 extends App{

  import spinal.core._
  SpinalVerilog(
    new Module{
      val a,b = in UInt(8 bits)
      val result = out(a + b)
    }
  )
}

object Main5 extends App{
  import spinal.core._
  import spinal.lib._
  SpinalVerilog(
    new Module {
      val ram = Mem.fill(256)(UInt(16 bits))
      val writeA = slave(ram.writePort())
      val writeB = slave(ram.writePort())
      val readA  = slave(ram.readAsyncPort())
      val readB  = slave(ram.readAsyncPort())

      this.dslBody.walkDeclarations{
        case mem : Mem[_] => {
          println(s"found $mem")
          mem.dlcForeach[Any]{
            case write : MemWrite     => println(s"  found one write port")
            case read  : MemReadAsync => println(s"  found one read port")
          }
          mem.removeStatement()
        }
        case _ =>
      }
    }
  )
}

object Main6 extends App{
  import spinal.core._
  import spinal.lib._
  SpinalVerilog(
    new Module {
      val a,b,c = out UInt(8 bits)
      val array = ArrayBuffer[UInt]()
      array += a
      array += b
      array += c
      for(element <- array){
        element := 0
      }
    }
  )
}



////////////////////////////////////////////
/*
- with wires
- with buses
- with software API
- with automation
- with decentralization
- with negotiation
- with introspection
 */
object Main100 extends App{
  import spinal.core._
  import spinal.lib._

  SpinalVerilog(new Module{

    val axiConfig = Axi4Config(20, 32, 4, useQos = false, useProt = false, useCache = false, useLock = false, useRegion = false, useBurst = false)
    val axi = Axi4(axiConfig)
    Axi4SpecRenamer(axi)

    val apb = Apb3(
      addressWidth = 20,
      dataWidth = 32
    )

    val apbConfig = Apb3Config(
      addressWidth = 20,
      dataWidth    = 32
    )
    val apb2 = Apb3(apbConfig)

    val commonBus = Apb3(
      addressWidth = 20,
      dataWidth = 32
    )

    val gpioBus, uartBus = Apb3(
      addressWidth = 12,
      dataWidth = 32
    )

    val apbDecoder = Apb3Decoder(
      master = commonBus,
      slaves = List(
        gpioBus -> (0x2000, 4 kB),
        uartBus -> (0x5000, 4 kB)
      )
    )
  })
}


object Main101 extends App{
  import spinal.core._
  import spinal.lib._

  SpinalVerilog(new Module{
    val cpu0Bus, cpu1Bus = Axi4(Axi4Config(32, 32, idWidth = 2))
    val mainBus          = Axi4(Axi4Config(32, 32, idWidth = 4))
    val ramBus           = Axi4(Axi4Config(16, 32, idWidth = 6))
    val peripheralBus    = Axi4(Axi4Config(20, 32, idWidth = 6))
    val gpioBus, uartBus = Axi4(Axi4Config(12, 32, idWidth = 8))

    val axiCrossbar = Axi4CrossbarFactory()
    axiCrossbar.addSlaves(
      mainBus       -> (0x00000000,  4 GB),
      ramBus        -> (0x40000000, 64 kB),
      peripheralBus -> (0x10000000,  1 MB),
      gpioBus       -> (    0x2000,  4 kB),
      uartBus       -> (    0x5000,  4 kB)
    )

    axiCrossbar.addConnections(
      cpu0Bus       -> List(mainBus),
      cpu1Bus       -> List(mainBus),
      mainBus       -> List(ramBus, peripheralBus),
      peripheralBus -> List(gpioBus, uartBus)
    )
    axiCrossbar.build()

  })
}

object Main102 extends App{
  import spinal.core._
  import spinal.lib._

  SpinalVerilog(new Module{
    import spinal.lib.bus.tilelink
//    val m0, m1 = tilelink.fabric.Node()
//    val shared = tilelink.fabric.Node()
//    val s0, s1 = tilelink.fabric.Node()
//
//    shared << m0
//    shared << m0
//    s0 at 0x1000 of shared
//    s1 at 0x2000 of shared

    import spinal.lib.bus.tilelink.fabric.Node

    val cpu0Bus, cpu1Bus = Node()
    val mainBus          = Node()
    val ramBus           = Node()
    val peripheralBus    = Node()
    val gpioBus, uartBus = Node()

    mainBus << List(cpu0Bus, cpu1Bus)
    ramBus        at 0x40000000  of ramBus
    peripheralBus at 0x10000000  of ramBus
    gpioBus       at     0x2000  of peripheralBus
    uartBus       at     0x5000  of peripheralBus

  })

  import spinal.lib.bus.tilelink.fabric._
  case class Cpu() extends Area{
    val node = Node.master()
    val logic = Fiber build new Area{
      node.m2s.proposed load M2sSupport(
        addressWidth = 32,
        dataWidth = 64,
        transfers = M2sTransfers(
          get = SizeRange.upTo(64),
          putFull = SizeRange.upTo(64)
        )
      )
      node.m2s.parameters load M2sParameters(
        support = node.m2s.proposed,
        sourceCount = 4
      )

      node.s2m.supported load S2mSupport.none
      slave(node.bus)
    }
  }

  case class Peripheral() extends Area{
    val node = Node.master()
    val logic = Fiber build new Area{
      node.m2s.supported load M2sSupport(
        addressWidth = 12,
        dataWidth = 32,
        transfers = M2sTransfers(
          get = SizeRange.upTo(64),
          putFull = SizeRange.upTo(64)
        )
      )

      node.s2m.none()
      master(node.bus)
    }
  }

  import spinal.lib.bus.tilelink
  {

    class Node extends Area{
      // Node data model
      val cpuBus   = Handle[tilelink.Bus]()
      val adapter  = ArrayBuffer[Connection]()
      val downs = ArrayBuffer[Connection]()

      val proposed = Handle[tilelink.M2sSupport]()
      val supported = Handle[tilelink.M2sSupport]()
      val parameters = Handle[tilelink.M2sParameters]()

      //Fork an elaboration thread
      val thread = Fiber build new Area{
        // Do the Negotiation
        // Generate the required arbiter / decoder logic.
      }
    }

    class Connection(val m : Node, val s : Node) extends Area{
      val thread = Fiber build new Area{
        // Connect the m.decoder to the s.arbiter
      }
    }

  }
}



object Main103 extends App{
  import spinal.core._
  import spinal.lib._
  import spinal.lib.bus.tilelink
  import spinal.core.fiber._

  SpinalVerilog(new Module{
    val bus = Handle[tilelink.Bus]

    val thread1 = Fiber build {
      println("Thread 1 start")
      //Will wait on bus.load (from thread 2)
      bus.a.valid   := False
      bus.a.address := 42
      println("Thread 1 done")
    }

    val thread2 = Fiber build {
      println("Thread 2 start")
      val config = tilelink.BusParameter.simple(
        addressWidth = 32,
        dataWidth    = 64,
        sizeBytes    = 16,
        sourceWidth  =  4
      )
      //Will allow thread 1 to continue
      bus.load(tilelink.Bus(config))
      println("Thread 2 done")
    }
  })
}


object Main104 extends App {

  import spinal.core._
  import spinal.lib._


  import spinal.lib.bus.tilelink.fabric._

  case class Cpu() extends Area {
    val node = Node.master()

    val thread = Fiber build new Area {
      node.m2s.proposed load M2sSupport(
        addressWidth = 32,
        dataWidth = 64,
        transfers = M2sTransfers(
          get = SizeRange.upTo(64),
          putFull = SizeRange.upTo(64)
        )
      )
      node.m2s.parameters load M2sParameters(
        support = node.m2s.supported,
        sourceCount = 4
      )

      node.s2m.supported load S2mSupport.none
      // Implement the actual CPU hardware
      node.bus.a.valid := False
      node.bus.a.address := 0x42
      slave(node.bus)
    }
  }

  case class Peripheral() extends Area {
    val node = Node.slave()
    val logic = Fiber build new Area {
      node.m2s.supported load M2sSupport(
        addressWidth = 12,
        dataWidth = 32,
        transfers = M2sTransfers(
          get = SizeRange(4),
          putFull = SizeRange(4)
        )
      )

      node.s2m.none()
      master(node.bus)
    }
  }

  SpinalVerilog(new Module{
    import spinal.lib.bus.tilelink
    import spinal.lib.bus.tilelink.fabric.Node
    import tilelink.fabric

    val cpu        = Cpu()
    val adapter    = WidthAdapter()
    val peripheral = Peripheral()

    adapter.up << cpu.node
    peripheral.node at 0x2000 of adapter.down
  })

}


object Main105 extends App {
  import spinal.core._
  case class Node() extends Area with SpinalTagReady{
    def <<(that : Any) = {}
    def at(v : Int) = new {
      def of (a : Any) = {}
    }
  }

  import spinal.lib.bus.misc.SizeMapping
  trait MemoryConnection extends SpinalTag {
    def m : Nameable with SpinalTagReady
    def s : Nameable with SpinalTagReady
    def mapping : SizeMapping
//    def sToM(downs : MemoryTransfers, args : MappedNode) : MemoryTransfers

    override def toString = s"${m.getName()} ${s.getName()} $mapping"
  }

  SpinalVerilog(new Module{
    class CustomTag(val str : String) extends SpinalTag

    val counter = Reg(UInt(8 bits))
    counter := counter + 1

    counter.addTag(new CustomTag("hello"))
    counter.addTag(new CustomTag("miaou"))

    counter.foreachTag{
      case ct : CustomTag => println(ct.str)
      case _ =>
    }


    val cpu0Bus, cpu1Bus = Node()
    val mainBus          = Node()
    val ramBus           = Node()
    val peripheralBus    = Node()
    val gpioBus, uartBus = Node()

    mainBus << List(cpu0Bus, cpu1Bus)
    ramBus        at 0x40000000  of ramBus
    peripheralBus at 0x10000000  of ramBus
    gpioBus       at     0x2000  of peripheralBus
    uartBus       at     0x5000  of peripheralBus

    new MemoryConnection{
      def m = cpu0Bus
      def s = mainBus
      def mapping = SizeMapping(0, 0x100000000l)
      m.addTag(this)
      s.addTag(this)
    }
    new MemoryConnection{
      def m = mainBus
      def s = ramBus
      def mapping = SizeMapping(0x40000000, 0x10000)
      m.addTag(this)
      s.addTag(this)
    }
    new MemoryConnection{
      def m = mainBus
      def s = peripheralBus
      def mapping = SizeMapping(0x10000000, 0x100000)
      m.addTag(this)
      s.addTag(this)
    }
    val tag = new MemoryConnection{
      def m = peripheralBus
      def s = gpioBus
      def mapping = SizeMapping(0x2000, 0x1000)
    }
    peripheralBus.addTag(tag)
    gpioBus.addTag(tag)
    new MemoryConnection{
      def m = peripheralBus
      def s = uartBus
      def mapping = SizeMapping(0x5000, 0x1000)
      m.addTag(this)
      s.addTag(this)
    }

    uartBus.foreachTag{
      case mc : MemoryConnection => println(mc)
      case _ =>
    }

    def rec(node : Nameable with SpinalTagReady, level : Int){
      node.foreachTag{
        case mc : MemoryConnection if mc.m == node => {
          println("  "*level + mc)
          rec(mc.s, level + 1)
        }
        case _ =>
      }
    }

    rec(cpu0Bus, 0)

    {
      val peripheralBus = Node()
      val apbBus = Apb3(12, 32)
      val tag = new MemoryConnection {
        def m = peripheralBus
        def s = apbBus
        def mapping = SizeMapping(0x5000, 0x1000)
      }


      import spinal.lib.bus.tilelink

//      class TilelinkToAxiBridge() extends Area{
//        val up = tilelink.fabric.Node.slave()
//        val down = axi4.fabric.Node.master()
//
//        val tag = new MemoryConnection {
//          def m = peripheralBus
//          def s = apbBus
//          def mapping = SizeMapping(0, 0x1000)
//          up.add(this)
//          down.add(this)
//        }
//
//        val logic = Fiber build new Area{
//          // Handle the negotiation from Tilelink to AXI
//          // ...
//          // Generate the hardware
//          // ...
//        }
//      }

    }

  })


  case class Apb3(addressWidth: Int,
                  dataWidth : Int) extends Bundle with IMasterSlave {

    val PADDR      = UInt(addressWidth bits)
    val PSEL       = Bool()
    val PENABLE    = Bool()
    val PREADY     = Bool()
    val PWRITE     = Bool()
    val PWDATA     = Bits(dataWidth bits)
    val PRDATA     = Bits(dataWidth bits)
    val PSLVERROR  = Bool()

    override def asMaster(): Unit = {
      out(PADDR, PSEL, PENABLE, PWRITE, PWDATA)
      in(PREADY, PRDATA, PSLVERROR)
    }
  }
}

//ffmpeg -i video.mp4 -ac 1 video_mono.mp4


object Shubacktchan{
  import spinal.core._
  class Mux extends Component {
    // define some IO
    val a,b = in UInt(8 bits)
    val sel = in Bool()
    val result = out UInt(8 bits)

    //Define some behaviour
    when(sel){
      result := a
    } otherwise {
      result := b
    }
  }




  import spinal.lib.bus.tilelink
  class Node extends Area {
    val bus = Handle[tilelink.Bus]()

  }

////EXECUTION UNITES
//plugins += new ExecutionUnitBase("EU0")
//plugins += new SrcPlugin("EU0", earlySrc = true)
//plugins += new IntAluPlugin("EU0", aluStage = 0)
//plugins += new ShiftPlugin("EU0" , aluStage = 0)
//plugins += new BranchPlugin("EU0")
//
//plugins += new ExecutionUnitBase("EU1")
//plugins += new SrcPlugin("EU1")
//plugins += new IntAluPlugin("EU1")
//plugins += new ShiftPlugin("EU1")
//plugins += new BranchPlugin("EU1")
//
//plugins += new ExecutionUnitBase("EU2", writebackCountMax = 1)
//plugins += new SrcPlugin("EU2", earlySrc = true)
//plugins += new MulPlugin("EU2", writebackAt = 2, staticLatency = false)
//plugins += new DivPlugin("EU2", writebackAt = 2)
//plugins += new LoadPlugin("EU2")
//plugins += new StorePlugin("EU2")
//plugins += new EnvCallPlugin("EU2")(rescheduleAt = 2)
//plugins += new CsrAccessPlugin("EU2")(
//  decodeAt = 0,
//  readAt = 1,
//  writeAt = 2,
//  writebackAt = 2,
//  staticLatency = false
//)
}

object Shubacktchan2 extends App{
  import spinal.core._
  import spinal.core.fiber._
  SpinalVerilog(new Component{



    val _ = new Area {
      val dataWidth = Handle[Int]

      val peripheral = Fiber build new Area {
        val data = UInt(dataWidth.get bits)
      }

      val cpu = Fiber build new Area {
        dataWidth.load(32)
      }
    }

    import spinal.lib.pipeline._
    import spinal.lib.pipeline.Connection._
   /* val _ = new Area {

      val pipeline = new Pipeline {
        val fetch = new Stage()
        val decode = new Stage(M2S())
        val execute = new Stage(M2S())
      }

      val lock = Lock()
      Fiber build {
        lock.await()
        pipeline.build()
      }

      val PC = Stageable(UInt(32 bits))

      lock.retain()
      val decoder = Fiber build new Area {
        val isNop = pipeline.decode(PC) === 0x00000013
        lock.release()
      }
    }*/

    //    val proposal = Handle[Int]
//    val supported = Handle[Int]
//
//    val cpu = Fiber build new Area{
//      proposal.load(32)
//
//    }
//
//    val ram = Fiber build new Area {
//      supported.load(Math.min(proposal, 32))
//    }

//    val ram = new Area{
//      val dataWidth = Handle[Int]
//      val hardware = Fiber build new Area {
//        val bus = UInt(dataWidth.get bits)
//      }
//    }
//
//    val interconnect = Fiber build new Area{
//      // ...
//      ram.dataWidth.load(32)
//    }


    class Node extends Nameable{
      val ups = ArrayBuffer[Node]()
      val downs = ArrayBuffer[Node]()

      def <<(up: Node): Unit = {
        ups += up
        up.downs += this
      }

      val proposal = Handle[Int]
      val supported = Handle[Int]

      Fiber build {
        if(ups.nonEmpty) proposal.load(ups.map(_.proposal.get).max)
        if(downs.nonEmpty) supported.load(downs.map(_.supported.get).max)
        println(s"$this got ${proposal.get} ${supported.get}")
      }
    }

    class Cpu(propose : Int) extends Node{
      Fiber build{
        proposal.load(propose)
      }
    }

    class Peripheral(upTo: Int) extends Node {
      Fiber build {
        supported.load(Math.min(proposal, upTo))
      }
    }

    val a = new Cpu(16)
    val b = new Cpu(32)
    val shared = new Node()
    val x = new Peripheral(16)
    val y = new Peripheral(32)

    shared << a
    shared << b
    x << shared
    y << shared

  })
}


//object ShoubakaklalaChackalak{
//  val featureEnabled = true
//  if(featureEnabled){
//    Reg(UInt(8 bits))
//  }
//
//
//  for(i <- 0 to 2){
//    Reg(UInt(8 bits))
//  }
//
//  val flushes = ArrayBuffer[FlushCmd]()
//  flushes += new FlushCmd()
//  flushes += new FlushCmd()
//  flushes += new FlushCmd()
//
//  val PC = Hardtype(UInt(32 bits))
//  val Instruction = Hardtype(UInt(32 bits))
//  val fetch, decode, execute = HashMap[Hardtype, Hardware]()
////  ...
////  fetch(PC) := ...
////  ...
//  val target = execute(PC) + 0x666
//}