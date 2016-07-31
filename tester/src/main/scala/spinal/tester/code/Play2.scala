package spinal.tester.code


import java.io.{PrintWriter, ByteArrayOutputStream}
import java.util

import spinal.core.Operator.UInt.Add
import spinal.core._
import spinal.lib._
import spinal.lib.bus.avalon._
import spinal.lib.bus.amba4.axilite.{AxiLite4SpecRenamer, AxiLite4Config, AxiLite4}
import spinal.lib.bus.neutral.NeutralStreamDma
import spinal.lib.com.uart.UartCtrl
import spinal.lib.eda.mentor.MentorDo
import spinal.lib.fsm._
import spinal.lib.graphic.{Rgb, RgbConfig}
import spinal.lib.graphic.vga.{AvalonMMVgaCtrl, VgaCtrl}
import spinal.lib.com.i2c._
import spinal.lib.io.ReadableOpenDrain


import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.sys.process.{ProcessLogger, Process, ProcessIO}


/**
 * Created by PIC32F_USER on 21/05/2016.
 */


object PlayB7 {

  class TopLevel extends Component {

    val io = new Bundle {
      val x = in  UInt( 4 bits )
      val z = out UInt( 4 bits )
    }
    val a = Stream(Fragment(UInt(4 bits)))
    val b = StreamArbiterFactory.sequentialOrder.noLock.build(a,10)
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}


object Play74 {

  class TopLevel extends Component {

    when(in(Bool)) {
      assert(
        assertion = True,
        message = "Address read doesn't match the address of the device ",
        severity = NOTE
      )
    }
    report(
      message = "Address read doesn't match the address of the device ",
      severity = NOTE
    )
  }


  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}


object PlayB6 {

  class TopLevel extends Component {
    val notUsed = False


    val uartCtrl = new UartCtrl()
    val writeCmd = Stream(Bits(8 bits))
    writeCmd.queue(16).haltWhen(True) >> uartCtrl.io.write

    val result = out(True)
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}


object PlayB5 {

  class TopLevel extends Component {
    val myClock = in Bool

    val myClockDomain = ClockDomain(myClock, True)

    val area = new ClockingArea(myClockDomain) {
      val result = out(RegNext(in(Bool)) init (True))
    }

  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PlayFixedPoint {

  class TopLevel extends Component {
    val i16_m2 = in.SFix(16 exp,-2 exp)
    val i16_0 = in SFix(16 exp,0 exp)
    val i8_m2 = in SFix(8 exp,-2 exp)
    val o16_m2 = out SFix(16 exp,-2 exp)
    val o16_m0 = out SFix(16 exp,0 exp)
    val o14_m2 = out SFix(14 exp,-2 exp)


    o16_m2 := i16_m2
    o16_m0 := i16_m2.truncated
    o14_m2 := i16_m2.truncated
    o16_m0 := i16_m2.truncated
    o14_m2 := i16_m2.truncated

    val o4_m2 = out(SFix(4 exp,-2 exp))
    o4_m2 := 1.25    //Will load 5 in i4_m2.raw
    val oo4_m2 = out(SFix(4 exp,-2 exp))
    oo4_m2 := 4       //Will load 16 in i4_m2.raw

  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}


object PlayBug75 {
  implicit class StreamPimped(pimped : Stream[UInt]){
    def asStreamSInt() : Stream[SInt] = pimped.translateWith(pimped.payload.asSInt)
  }

  class TopLevel extends Component {
    val src = slave Stream(UInt(4 bits))
    val sink = master Stream(SInt(4 bits))

    sink << src.asStreamSInt()
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PlayBlackBox3 {
  class Ram_1w_1r(_wordWidth: Int, _wordCount: Int) extends BlackBox {
    val generic = new Generic {
      val wordCount = _wordCount
      val wordWidth = _wordWidth
    }

    val io = new Bundle {
      val clk = in Bool

      val wr = new Bundle {
        val en = in Bool
        val addr = in UInt (log2Up(_wordCount) bit)
        val data = in Bits (_wordWidth bit)
      }
      val rd = new Bundle {
        val en = in Bool
        val addr = in UInt (log2Up(_wordCount) bit)
        val data = out Bits (_wordWidth bit)
      }
    }.setName("")

    mapClockDomain(clock=io.clk)
  }

  class TopLevel extends Component {
    val io = new Bundle {
      val wr = new Bundle {
        val en = in Bool
        val addr = in UInt (log2Up(16) bit)
        val data = in Bits (8 bit)
      }
      val rd = new Bundle {
        val en = in Bool
        val addr = in UInt (log2Up(16) bit)
        val data = out Bits (8 bit)
      }
    }
    val ram = new Ram_1w_1r(8,16)

    io.wr.en <> ram.io.wr.en
    io.wr.addr <> ram.io.wr.addr
    io.wr.data <> ram.io.wr.data
    io.rd.en   <> ram.io.rd.en
    io.rd.addr <> ram.io.rd.addr
    io.rd.data <> ram.io.rd.data

  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}


object PlayB1 {

  class TopLevel extends Component {
    val io = new Bundle() {
      val cond = in Bool
      val input = in UInt (4 bit)
      val output = out UInt(4 bits)
    }

    var carry = Bool(false)
    for (bit <- io.input.asBools) {
      when(io.cond) {
        carry \= carry & bit
      }
    }
    io.output := carry.asUInt

  }

  def main(args: Array[String]): Unit = {
    //SpinalVhdl(new TopLevel)
    SpinalVhdl(new TopLevel)
  }
}

object PlayB2 {

  class TopLevel extends Component {

    val input = in UInt (3 bit)
    val output = out UInt(4 bits)
    output := input + input
    //    switch(io.input){
    //      is(0){
    //        io.output := 0
    //      }
    //      is(1){
    //        io.output := 1
    //      }
    //      is(2){
    //        io.output := 2
    //      }
    //      default{
    //        io.output := 3
    //      }
    //    }
  }

  def main(args: Array[String]): Unit = {
    //SpinalVhdl(new TopLevel)

    SpinalConfig(mode = VHDL).generate(new TopLevel)
  }
}
object PlayB3 {

  class TopLevel extends Component {

    val input = in UInt (4 bit)
    val output = out UInt(4 bits)
    output := 1

  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PlayB4 {

  class TopLevel extends Component {
    val address = in UInt(8 bits)
    val writeData = in Bits(8 bits)
    val chipSelect = in Bool
    val writeEnable = in Bool
    val readData = out Bits(8 bits)

    val mem = Mem(Bits(8 bits),16)
    readData := mem.writeReadSync(address,writeData,chipSelect,writeEnable)




  }

  def main(args: Array[String]): Unit = {
    //SpinalVhdl(new TopLevel)
    SpinalVhdl(new TopLevel)
  }
}


object PlayAssert {

  class TopLevel extends Component {
    val a,b = in UInt(4 bits)
    val tmp = a + 2
    when(a.lsb) {
      when(a.msb) {
        assert(tmp === 8, "Yolo", NOTE)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    //SpinalVhdl(new TopLevel)
    SpinalVhdl(new TopLevel)
  }
}



object PlayAssert2 {

  class TopLevel extends Component {
    val valid = RegInit(False)
    val ready = in Bool

    when(ready){
      valid := False
    }
    // some logic

    assert(
      assertion = !(valid.fall && !ready),
      message   = "Valid drop when ready was low",
      severity  = ERROR
    )
  }

  def main(args: Array[String]): Unit = {
    //SpinalVhdl(new TopLevel)
    SpinalVhdl(new TopLevel)
  }
}


object PlayPerf {

  def timeOf(that : => Unit,message : String) : Unit = {
    for(i <- 0 until 10){
      val start = System.nanoTime()
      that
      val end = System.nanoTime()
      println(message + " " + (end-start)*1e-9)
    }

  }

  def main(args: Array[String]): Unit = {
    var total = 0
    val arrayBuffer = ArrayBuffer.tabulate(1000*1000*10)(i => i)
    val array = Array.tabulate(1000*1000*10)(i => i)
    timeOf({
      var sum = 0
      for(e <- arrayBuffer){
        sum += e
      }
      total += sum
    },"For loop ArrayBuffer")

    timeOf({
      var sum = 0
      var idx = arrayBuffer.length
      while(idx != 0){
        idx -= 1
        sum += arrayBuffer(idx)
      }
      total += sum
    },"While ArrayBuffer")


    timeOf({
      var sum = 0
      for(e <- array){
        sum += e
      }
      total += sum
    },"For loop Array")

    timeOf({
      var sum = 0
      var idx = array.length
      while(idx != 0){
        idx -= 1
        sum += array(idx)
      }
      total += sum
    },"While Array")


    timeOf({
      var sum = 0
      def doIt(i : Int) = sum += i
      var idx = array.length
      while(idx != 0){
        idx -= 1
        sum += idx
      }
      total += sum
    },"while inc")

    timeOf({
      var sum = 0
      def doIt(i : Int) = sum += i
      var idx = array.length
      while(idx != 0){
        idx -= 1
        doIt(idx)
      }
      total += sum
    },"while doit")


    abstract class AbstractWorker{
      def doIt(dx : Int) : Unit
    }

    var sum = 0
    class WorkerA extends AbstractWorker{
      override def doIt(idx : Int) : Unit = sum += idx
    }
    class WorkerB extends AbstractWorker{
      override def doIt(idx : Int) : Unit = print(idx)
    }

    val arrayWorkerA = Array.tabulate(1000*1000*10)(i => new WorkerA)
    timeOf({
      sum = 0
      var idx = array.length
      while(idx != 0){
        idx -= 1
        arrayWorkerA(idx).doIt(idx)
      }
      total += sum
    },"while WorkerA")

    class NodeArrayBuffer(val inputs : ArrayBuffer[Int]){

    }

    abstract class NodeAbstract{
      def onEachInput(doThat : (Int,Int) => Unit) : Unit
    }

    class FuncCtion(a : Int,b : Int) extends NodeAbstract{
      override def onEachInput(doThat: (Int, Int) => Unit): Unit = {
        doThat(a,0)
        doThat(b,1)
      }
    }
    class OperRator(a : Int,b : Int) extends NodeAbstract{
      override def onEachInput(doThat: (Int, Int) => Unit): Unit = {
        doThat(a,0)
        doThat(b,1)
      }
    }

    val arrayNodeArrayBuffer = Array.tabulate(1000*1000)(i => new NodeArrayBuffer(ArrayBuffer(i*2,i*3)))
    val arrayNodeAbstract = Array.tabulate(1000*1000)(i => if(i % 2 == 0) new FuncCtion(i*2,i*3) else new OperRator(i*2,i*3))

    timeOf({
      sum = 0
      var idx = arrayNodeArrayBuffer.length
      while(idx != 0){
        idx -= 1
        arrayNodeArrayBuffer(idx).inputs.foreach(sum += _)
      }
      total += sum
    },"while NodeArrayBuffer foreach")

    timeOf({
      sum = 0
      var idx = arrayNodeArrayBuffer.length
      while(idx != 0){
        idx -= 1
        for(e <- arrayNodeArrayBuffer(idx).inputs)
          sum += e
      }
      total += sum
    },"while NodeArrayBuffer for")

    timeOf({
      sum = 0
      var idx = arrayNodeArrayBuffer.length
      while(idx != 0){
        idx -= 1
        val inputs = arrayNodeArrayBuffer(idx).inputs
        var idx2 = inputs.length
        while(idx2 != 0){
          idx2 -= 1
          sum += inputs(idx2)
        }
      }
      total += sum
    },"while NodeArrayBuffer while")


    timeOf({
      sum = 0
      var idx = arrayNodeAbstract.length
      while(idx != 0){
        idx -= 1
        arrayNodeAbstract(idx).onEachInput((e,i) => sum += e)
      }
      total += sum
    },"while NodeAbstract")

    class Toto{
      var i = 5
    }
    val toto = new Toto
    timeOf({
      sum = 0
      var idx = arrayNodeAbstract.length
      while(idx != 0){
        idx -= 1
        new Toto
      }
      total += sum
    },"while new")

    timeOf({
      sum = 0
      var idx = arrayNodeAbstract.length
      while(idx != 0){
        idx -= 1
        toto.getClass().newInstance()
      }
      total += sum
    },"while new")


    println(total)
  }
}




object PlayBug43{

  /* Bus configuration  */
  case class SimpleBusConfig(val dataWidth:Int=32, val addrWidth:Int=32)

  /* Bus definition */
  case class SimpleBus(val config:SimpleBusConfig) extends Bundle with IMasterSlave {
    val cs   = Bool
    val rwn  = Bool
    val dIn  = Bits(config.dataWidth bits)
    val addr = Bits(config.addrWidth bits)
    val dOut = Bits(config.dataWidth bits)

    override def asMaster(): this.type = {
      out(cs)
      out(rwn)
      out(dIn)
      out(addr)
      in(dOut)
      this
    }
  }


  /* Factory */
  trait SimpleBusSlaveFactoryElement

  case class SimpleBusSlaveFactoryRead(that : Data, address : BigInt) extends SimpleBusSlaveFactoryElement
  case class SimpleBusSlaveFactoryWrite(that : Data, address : BigInt) extends SimpleBusSlaveFactoryElement

  class SimpleBusSlaveFactory(bus : SimpleBus) extends Area {

    val elements = ArrayBuffer[SimpleBusSlaveFactoryElement]()

    def read(that : Data, address : BigInt ): Unit = elements += SimpleBusSlaveFactoryRead(that, address)
    def write(that: Data, address : BigInt ): Unit = elements += SimpleBusSlaveFactoryWrite(that, address)

    component.addPrePopTask(() =>{

      bus.dOut := 0
      when(bus.cs){
        when(bus.rwn){

          for (e <- elements ; if e.isInstanceOf[SimpleBusSlaveFactoryWrite]){
            val w = e.asInstanceOf[SimpleBusSlaveFactoryWrite]
            when(w.address === bus.addr){
              w.that := bus.dIn
            }
          }

        } otherwise {

          for (e <- elements ; if e.isInstanceOf[SimpleBusSlaveFactoryRead]){
            val w = e.asInstanceOf[SimpleBusSlaveFactoryRead]
            when(w.address === bus.addr){
              bus.dOut := w.that.asBits
            }
          }

        }
      }
    })

  }

  /* Top Level */
  class PlaySimpleBus(dataWidth : Int, addrWidth:Int ) extends Component{

    val io = new Bundle{
      val bus    = slave(SimpleBus(SimpleBusConfig(dataWidth,addrWidth)))
      val reg1   = out Bits(dataWidth bits)
    }

    val factory = new SimpleBusSlaveFactory(io.bus)
    val reg1 = Reg(Bits( dataWidth bits )) init(0)

    factory.read(reg1, 0x00112233l)
    factory.write(reg1, 0xdeadbeefl)
    io.reg1 := reg1

  }
  def main(args: Array[String]) {
    SpinalVhdl(new PlaySimpleBus(32,32))
  }


}


object PlayUnconstrained {
  class Counter extends Component{
    val io = new Bundle{
      val load = slave  Flow(UInt)
      val value = out UInt
    }
    val register = Reg(UInt()) init(0)
    register := register + 1
    when(io.load.valid){
      register := io.load.payload
    }
    io.value := register
  }

  class TopLevel extends Component {
    val io = new Bundle{
      val load = slave Flow(UInt(8 bits))
      val value = out UInt
    }

    val counter = new Counter
    counter.io.load <> io.load
    counter.io.value <> io.value
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PlayBootReset {

  class TopLevel extends Component {
    val coreClk = in Bool
    val coreClockDomain = ClockDomain(coreClk,config = ClockDomainConfig(resetKind = BOOT))
    val core = new ClockingArea(coreClockDomain){
      val input = in UInt (4 bit)
      val output = out UInt(4 bits)
      output := RegNext(output) init(0)

    }

  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}



object PlayWidthChanger {
  case class RGB() extends Bundle{
    val r,g,b = UInt(8 bits)
  }
  
  class TopLevel extends Component {
    val cmd = slave (Stream Fragment Bits(8 bits))
    val rsp = master(cmd.toStreamOf(RGB()))
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}


object PlayB8 {

  def main(args: Array[String]): Unit = {
//    SpinalConfig(mode = VHDL,targetDirectory="temp/myDesign").generate(new UartCtrl)
    SpinalConfig.shell(Seq("-aa"))(new UartCtrl)
  }
}

object PlayVerilog1 {
  class Sub extends Component{
    val cmd = in UInt(4 bits)
    val rsp = out UInt(4 bits)
    rsp := cmd + cmd
  }

  class TopLevel extends Component {
    val a,b = in UInt(4 bits)
    val x,y,z = out UInt(4 bits)

    x := a - b

    y := a + b
    y(0) := False

    z(z.range) := U"0110"

    val l,m = UInt(4 bits).keep()
    l := a & b
    m := a
    when(a === b){
      m := b
    }

    val n,o = Reg(UInt(4 bits)).keep()
    n := a & b
    o := a
    when(a === b){
      o := b
    }

    val p,q = Reg(UInt(4 bits)).keep() init(U"0010")
    p := a & b
    q := a
    when(a === b){
      q := b
    }

    val sub = new Sub
    sub.cmd := 0
    val subOut = out(UInt(4 bits))
    subOut := sub.rsp

    val r = UInt(5 bits).keep()
    r := ((a-b) >> 2).resized


    object MyEnum extends SpinalEnum{
      val a,b,c = newElement
    }

    val e1 = MyEnum().keep
    e1 := MyEnum.a

    r.addAttribute("flag")
    r.addAttribute("value","yolo")


    val s = out(UInt(4 bits))
    s := 15
    s(0) := False
  }
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = Verilog,defaultConfigForClockDomains=ClockDomainConfig(clockEdge = RISING,resetKind = SYNC,resetActiveLevel = LOW))
      .generate(new TopLevel)
  }
}



object PlaySwitch4 {

  object MyEnum extends SpinalEnum{
    val a,b,c = newElement
  }

  class TopLevel extends Component {
    val sel = in (MyEnum())
    val result = out UInt(8 bits)

    result := 5
    switch(sel){
      is(MyEnum.a){
        result := 0
      }
      is(MyEnum.b){
        result := 1
      }
//      default{
//        result := 2
//      }
    }

    val sel2 = in UInt(8 bits)
    val result2 = out UInt(8 bits)
    result2 := 3
    switch(sel2){
      is(0){
        result2 := 0
      }
      is(1){
        result2 := 1
      }
      is(2){
        result2 := 2
      }
    }
  }



  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
    SpinalVerilog(new TopLevel)
  }
}



object Play65{


  class TopLevel extends Component {
    val sel = in (Vec(Bool,8))
    val result = out UInt(8 bits)
    val result2 = out UInt(8 bits)

    result := 6
    result2 := 0

    when(sel(3)){
      result := 5
    }.elsewhen(sel(4)){
      result := 6
    }

    when(sel(0)){
      result := 0
    }otherwise{
      result := 1
      result2 := 1
      when(sel(1)){
        result := 2
      }.elsewhen(sel(2)){
        result := 4
        result2 := 3
      }
    }
  }



  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}



object PlayResize{


  class TopLevel extends Component {
    val cmd = in Vec(Bool,3)
    val rsp = out Vec(Bool,2)
    rsp := cmd.asBits.asSInt.resize(2).asBools

    for((e,i) <- cmd.zipWithIndex) e.setName(('a' + i).toChar.toString)
    for((e,i) <- rsp.zipWithIndex) e.setName(('x' + i).toChar.toString)
  }



  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
    SpinalVerilog(new TopLevel().setDefinitionName("TopLevelV"))
  }
}




object PlayGenerics{
  class TopLevel extends Component {

    val io = new Bundle {
      val in1  = in Bool
      val out1 = out Bool
    }

    val inbuf = alt_inbuf()
    inbuf.i := io.in1
    io.out1 := inbuf.o
  }


  trait BOOLEAN { def value : String }
  case object ON   extends BOOLEAN { def value = "ON"   }
  case object OFF  extends BOOLEAN { def value = "OFF"  }
  case object NONE extends BOOLEAN { def value = "None" }


  trait IO_STRANDARD { def value : String }
  case object STD_1_2V      extends IO_STRANDARD { def value = "1.2V" }
  case object STD_1_2V_HSTL extends IO_STRANDARD { def value = "1.2- V HSTL" }
  case object STD_1_2V_HSUL extends IO_STRANDARD { def value = "1.2- V HSUL" }
  case object STD_NONE      extends IO_STRANDARD { def value = "None" }


  /**
   * ALT_INBUF
    *
    * @TODO add library altera.altera_primitives_components
   */
  case class alt_inbuf(_io_standard           : IO_STRANDARD = STD_NONE,
                       _location              : String       = "None",
                       _enable_bus_hold       : BOOLEAN      = NONE,
                       _weak_pull_up_resistor : BOOLEAN      = NONE,
                       _termination           : String       = "None") extends BlackBox{

    val generic = new Generic {
      val io_standard           = _io_standard.value
      val location              = _location
      val enable_bus_hold       = _enable_bus_hold.value
      val weak_pull_up_resistor = _weak_pull_up_resistor.value
      val termination           = _termination
    }

    val io = new Bundle{
      val i = in Bool
      val o = out Bool
    }.setName("")

    def i : Bool = io.i
    def o : Bool = io.o
  }



  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}



object PlayCocotb{


  class TopLevel extends Component {
    val clear = in Bool
    val incrementBy = in UInt(16 bits)
    val result = out (Reg(UInt(16 bits))) init(0)
    result := result + incrementBy
    when(clear){
      result := 0
    }
  }



  def main(args: Array[String]): Unit = {
    //SpinalVhdl(new TopLevel)
    SpinalVerilog(new TopLevel)

    import scala.sys.process._


    def doCmd(cmd : String): Unit ={
      println(cmd)
      Process("sh -c \"" + cmd + "\"") !
    }

    doCmd("export COCOTB=/d/pro/hdl/cocotbRepo && cd tester/src/test/python/TopLevel && make")

  }
}

object PlayShell{
  def main(args: Array[String]) {
    var res = ""
    val io = new ProcessIO(
      stdin  => { stdin.write(("Yolo\n").getBytes)
        stdin.flush()
        stdin.close() },
      stdout => { scala.io.Source.fromInputStream(stdout).getLines.foreach(println)
        stdout.close() },
      stderr => { scala.io.Source.fromInputStream(stderr).getLines.foreach(println)
        stderr.close()})
    val proc = Process("cmd").run(io)
  }
}

object PlayRefl{
  def main(args: Array[String]) {
    class Toto{
      @dontName var caca = 42.0
      @dontName val cash = 2.0
      @dontName val name = "salut"
    }
    val toto = new Toto
    println(toto)
  }
}

object PlayEnumTypes{
  class MyEnum extends SpinalEnum{
    val s0,s1,s2 = newElement()
  }
  class MyEnum2 extends SpinalEnum{
    val s0,s1,s2 = newElement()
  }

  class TopLevel extends Component {
    val enumDef = new MyEnum
    val enumDef2 = new MyEnum2

    //    implicit def EnumEtoEnumE2[T <: SpinalEnum,T2 <: T](element : SpinalEnumElement[T])  = element.asInstanceOf[SpinalEnumElement[T2]]

    val e0 = enumDef()
    val e1 : SpinalEnumElement[MyEnum] = enumDef.s0.asInstanceOf[ SpinalEnumElement[MyEnum] ]
    val e2 : SpinalEnumElement[MyEnum] = enumDef.s0
    e2 := enumDef.s0
    val e3 : enumDef.E = e1.asInstanceOf[enumDef.E]
    val e4 : enumDef.E = e1

    //    val f0 = enumDef2()
    //    val f1 : SpinalEnumElement[MyEnum2] = enumDef.s0.asInstanceOf[ SpinalEnumElement[MyEnum] ]
    //    val f2 : SpinalEnumElement[MyEnum2] = enumDef.s0
    //    f2 := enumDef.s0
    //    val f3 : enumDef2.E = f1.asInstanceOf[enumDef.E]
    //    val f4 : enumDef2.E = f1
  }



  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}


object PlayOthersLike {

  import spinal.lib.fsm._

  class TopLevel extends Component {
    val t0 = out(B(5 -> true,default -> false))
    val t1 = out Bits(10 bits)
    t1 := B(5 -> true,default -> false)


    val t2 = out(B(5 -> true,default -> False))
    val t3 = out Bits(10 bits)
    t3 := B(5 -> true,default -> False)
  }


  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}


object PlayNodeWithoutWidth{
  import spinal.lib.fsm._
  class TopLevel extends Component {
    val a,b,c = in UInt(8 bits)

    val result = out Bits


      result := 0
    out(B((3 downto 0) -> a(0)))
//    result := a + a
//    when(b === 0){
//      result := c + a
//    }
//    result.getWidth
  }



  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PlayI2CMasterHAL {

  class I2CMasterHALTester extends Component {

    val generic = I2CMasterHALGenerics()

    val io = new Bundle {
      val i2c    = master( I2C() )
      val config = in( I2CMasterHALConfig(generic) )
      val cmd    = slave Stream(I2CMasteHALCmd(generic))
      val rsp    = master Flow(I2CMasterHALRsp (generic))
    }


    val myMasterI2C = new I2CMasterHAL(generic)
    io <> myMasterI2C.io
  }

  def main(args: Array[String]) {
    SpinalConfig(
      mode = Verilog,
      dumpWave = DumpWaveConfig(depth = 0),
      defaultConfigForClockDomains = ClockDomainConfig(clockEdge = RISING, resetKind = ASYNC, resetActiveLevel = LOW),
      defaultClockDomainFrequency = FixedFrequency(50e6)
    ).generate(new I2CMasterHALTester).printPruned
  }
}


object PlayI2CSlaveHAL {

  class I2CSlaveHALTester extends Component {

    val generic = I2CSlaveHALGenerics()

    val io = new Bundle {
      val i2c    = slave( I2C() )
      val config = in(I2CSlaveHALConfig(generic))
      val cmd    = master  Flow ( I2CSlaveHALCmd(generic) )
      val rsp    = slave Stream ( I2CSlaveHALRsp(generic) )
    }

    val mySlave = new I2CSlaveHAL(generic)
    io <> mySlave.io
  }

  def main(args: Array[String]) {
    SpinalConfig(
      mode = Verilog,
      dumpWave = DumpWaveConfig(depth = 0),
      defaultConfigForClockDomains = ClockDomainConfig(clockEdge = RISING, resetKind = ASYNC, resetActiveLevel = LOW),
      defaultClockDomainFrequency  = FixedFrequency(50e6)
    ).generate(new I2CSlaveHALTester).printPruned
  }
}
object PlayI2CHAL{

  class I2CHALTester extends Component{

    val slaveGeneric  = I2CSlaveHALGenerics()
    val masterGeneric = I2CMasterHALGenerics()

    val io = new Bundle{
      val ioSlave = new Bundle {
        val cmd  = master  Flow ( I2CSlaveHALCmd(slaveGeneric) )
        val rsp  = slave Stream ( I2CSlaveHALRsp(slaveGeneric) )
      }
      val ioMaster = new Bundle {
        val cmd    = slave Stream(I2CMasteHALCmd(masterGeneric))
        val rsp    = master Flow (I2CMasterHALRsp (masterGeneric))
      }

      val sda = out Bool
      val scl = out Bool
    }

    val i2cSlave  = new I2CSlaveHAL(slaveGeneric)
    val i2cMaster = new I2CMasterHAL(masterGeneric)


    i2cSlave.io.cmd  <> io.ioSlave.cmd
    i2cSlave.io.rsp  <> io.ioSlave.rsp
    i2cMaster.io.cmd <> io.ioMaster.cmd
    i2cMaster.io.rsp <> io.ioMaster.rsp
    i2cMaster.io.config.setSCLFrequency(2e6)
    i2cMaster.io.config.enCollision := True
    i2cMaster.io.config.setClockDividerSampling(5)

    io.sda := i2cMaster.io.i2c.sda.read
    io.scl := i2cMaster.io.i2c.scl.read
    i2cSlave.io.config.setClockDividerSampling(5)


    interconnect(Seq(i2cMaster.io.i2c.scl, i2cSlave.io.i2c.scl))
    interconnect(Seq(i2cMaster.io.i2c.sda, i2cSlave.io.i2c.sda))


    //def interconnect[T <: Data](elements : Seq[ReadableOpenDrain[T]]) : Unit = {
    def interconnect(elements : Seq[ReadableOpenDrain[Bool]]) : Unit = {
      val readValue = elements.map(_.write).reduce(_ & _)
      elements.foreach(_.read := readValue)
    }
  }

  def main(args : Array[String]): Unit ={
    SpinalConfig(
        mode = Verilog,
        dumpWave = DumpWaveConfig(depth = 0),
        defaultConfigForClockDomains = ClockDomainConfig(clockEdge = RISING, resetKind = ASYNC, resetActiveLevel = LOW),
        defaultClockDomainFrequency  = FixedFrequency(50e6)
    ).generate(new I2CHALTester()).printPruned()
  }
}

object PlayI2CHALTest{

  class I2CHAL_App extends Component {

    val io = new Bundle{
      val bus       = slave(AvalonMM(AvalonMMSlaveFactory.getAvalonConfig(4,32)))
      val i2cSlave  = master( I2C() )
      val i2cMaster = slave( I2C() )
    }

    /**
      * Create the master and the slave
      */
    val masterI2CGeneric = I2CMasterHALGenerics()
    val masterI2C     = new I2CMasterHAL(masterI2CGeneric)
    val slaveI2C      = new I2CSlaveHAL(I2CSlaveHALGenerics())


    /**
      * Define all register needed to manage the master and the slave
      */
    val masterStatusCMD = Reg(Bits(32 bits)) randBoot()
    val masterStatusRSP = Reg(Bits(32 bits)) randBoot()
    val masterStreamRSP = Stream(I2CMasterHALRsp(masterI2CGeneric))

    val slaveRSP       = Reg(Bits(32 bits)) randBoot()
    val slaveStatusCMD = Reg(Bits(32 bits)) randBoot()
    val slaveStatusRSP = Reg(Bits(32 bits)) randBoot()
    val salveRSP_valid = Reg(Bool) init(False)

    masterI2C.io.rsp.toStream >-> masterStreamRSP

    /**
      * Create the Avalon bus and the rigister bank
      */
    val busCtrl = AvalonMMSlaveFactory(io.bus)

    busCtrl.driveAndRead(masterI2C.io.config,    0x00)
    busCtrl.createAndDriveFlow(I2CMasteHALCmd(masterI2CGeneric), 0x04).toStream >-> masterI2C.io.cmd
    busCtrl.readStreamNonBlocking(masterStreamRSP, address = 0x0C, validBitOffset=0, payloadBitOffset=1)
    busCtrl.readAndWrite (masterStatusCMD, 0x08)
    //  busCtrl.readAndWrite (masterStatusRSP, 0x0C)

    busCtrl.driveAndRead(slaveI2C.io.config.clockDividerSampling,    0x10)
    busCtrl.write(slaveRSP,       0x14)
    busCtrl.readAndWrite (slaveStatusCMD, 0x18)
    busCtrl.readAndWrite (slaveStatusRSP, 0x1C)
    busCtrl.onWrite(0x14){ salveRSP_valid.set() }



    // Master connection

    masterI2C.io.i2c <> io.i2cMaster


    when(masterI2C.io.cmd.ready){
      masterStatusCMD(0) := True
    }

    // Slave connection

    slaveI2C.io.i2c  <> io.i2cSlave

    slaveI2C.io.rsp.mode.assignFromBits( slaveRSP(1 downto 0) )
    slaveI2C.io.rsp.data  := slaveRSP(9 downto 2)
    slaveI2C.io.rsp.valid  := salveRSP_valid


    when(slaveI2C.io.rsp.ready){
      slaveStatusRSP(0) := True
      salveRSP_valid    := False
    }
    when(slaveI2C.io.cmd.valid){
      slaveStatusCMD := 0
      slaveStatusCMD(0) := True
      slaveStatusCMD(2 downto 0)  := slaveI2C.io.cmd.mode.asBits
      slaveStatusCMD(10 downto 3) := slaveI2C.io.cmd.data
    }
  }

  def main(args : Array[String]): Unit ={
    SpinalConfig(
      mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(clockEdge = RISING, resetKind = ASYNC, resetActiveLevel = LOW),
      defaultClockDomainFrequency  = FixedFrequency(50e6)
    ).generate(new I2CHAL_App()).printPruned()
  }

}



object PlayEnumName {

  class TopLevel extends Component {
    val a = new SpinalEnum{
      val x,b,c = newElement
    }
    val myEnum = new SpinalEnum{
      val a,b,c = newElement
    }
    val state = out(myEnum.a())
    val state2 = out(a.b())

    switch(state){
      is(myEnum.a, myEnum.b){
        state2 := a.c
      }
    }

    def toto: Unit ={
      val enumDef = new SpinalEnum{
        val a,b,c = newElement
        setName("myEnum")
      }
      val xx = out(enumDef.b())
    }
    toto
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}



object PlayWidthInferation {

  class TopLevel extends Component {
    val sel = in Bool
    val result = out(Mux(sel,S(1),S(0)) + 1 + 3)

  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}

object PlaySoftReset {

  class TopLevel extends Component {
    val io = new Bundle{
      val clk = in Bool
      val resetn = in Bool
      val softReset = in Bool

      val result = out UInt(4 bits)
    }

    val asyncClockDomain = ClockDomain(
      clock     = io.clk,
      reset     = io.resetn,
      softReset = io.softReset,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetActiveLevel = LOW,
        resetKind = ASYNC,
        softResetActiveLevel = HIGH
      )
    )

    val syncClockDomain = ClockDomain(
      clock     = io.clk,
      reset     = io.resetn,
      softReset = io.softReset,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetActiveLevel = LOW,
        resetKind = SYNC,
        softResetActiveLevel = HIGH
      )
    )

    val bootClockDomain = ClockDomain(
      clock     = io.clk,
      softReset = io.softReset,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetActiveLevel = LOW,
        resetKind = BOOT,
        softResetActiveLevel = HIGH
      )
    )

    val noResetClockDomain = ClockDomain(
      clock     = io.clk,
      softReset = io.softReset,
      config = ClockDomainConfig(
        clockEdge = RISING,
        softResetActiveLevel = HIGH
      )
    )

    val noResetNoSoftResetClockDomain = ClockDomain(
      clock     = io.clk,
      config = ClockDomainConfig(
        clockEdge = RISING
      )
    )

    val async = new ClockingArea(asyncClockDomain) {
      val counter = Reg(UInt(4 bits)) init (0)
      counter := counter + 1
    }

    val sync = new ClockingArea(syncClockDomain) {
      val counter = Reg(UInt(4 bits)) init (0)
      counter := counter + 1
    }

    val boot = new ClockingArea(bootClockDomain) {
      val counter = Reg(UInt(4 bits)) init (0)
      counter := counter + 1
    }

    val noReset = new ClockingArea(noResetClockDomain) {
      val counter = Reg(UInt(4 bits)) init (0)
      counter := counter + 1
    }

    val noResetNoSoftReset = new ClockingArea(noResetNoSoftResetClockDomain) {
      val counter = Reg(UInt(4 bits))
      counter := counter + 1
    }
    io.result := async.counter + sync.counter  + boot.counter + noReset.counter + noResetNoSoftReset.counter
  }

  def main(args: Array[String]) {
    val config = SpinalConfig()
    config.generateVerilog(new TopLevel)
    config.generateVhdl(new TopLevel)
  }
}


object PlayEnumInferation {
  object myEnum extends SpinalEnum{
    val a,b,c = newElement
  }
  class TopLevel extends Component {

    val sel = in UInt(4 bits)
//    val stateAuto             = out(myEnum())
//    val stateNative           = out(myEnum(native))
//    val stateBinarySequancial = out(myEnum(binarySequancial))
//    val stateBinaryOneHot     = out(myEnum(binaryOneHot))
//
//    stateAuto := myEnum.a
//    stateNative := myEnum.a
//    stateBinarySequancial := myEnum.a
//    stateBinaryOneHot := myEnum.a
//
//    when(sel === 0){
//      stateBinaryOneHot := stateBinarySequancial
//    }

    val inferredMux1 = Mux(sel(0),myEnum.a(binaryOneHot),myEnum.a)
    val inferredMux2 = Mux(sel(0),myEnum.b,myEnum.a)
    val egualsOut     = out(inferredMux1 === inferredMux2)
    val stateIn = in (myEnum(binaryOneHot))
    val stateBinarySequancialIsA    = out(stateIn === myEnum.a)
    val outState = out (myEnum())
    outState := stateIn
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    SpinalVerilog(new TopLevel)
  }
}

object PlayHex {

  class TopLevel extends Component {
    val io = new Bundle {
      val HEX0 = out Bits (7 bit)
      val HEX1 = out Bits (7 bit)
    }

    val segROM = Vec(Bits(7 bit),10)
    segROM(0) := "1000000"
    segROM(1) := "1111001"
    segROM(2) := "0100100"
    segROM(3) := "0110000"
    segROM(4) := "0011001"
    segROM(5) := "0010010"
    segROM(6) := "0000010"
    segROM(7) := "1011000"
    segROM(8) := "0000000"
    segROM(9) := "0010000"

    val hexFSM = new StateMachine{

      val delayState = new StateDelay(cyclesCount = 27000000) with EntryPoint
      val countState = new State
      val translationState = new State

      val sighex0 = Reg(UInt(4 bit)) init(0)
      val sighex1 = Reg(UInt(4 bit)) init(0)

      val HEX0 = Reg(Bits (7 bit))
      val HEX1 = Reg(Bits (7 bit))

      delayState
        .whenCompleted {
          goto(countState)
        }

      countState
        .whenIsActive {
          sighex0 := sighex0 + 1
          when(sighex0 === 0x9) {
            sighex0 := 0
            sighex1 := sighex1 + 1
            when(sighex1 === 0x9) {
              sighex1 := 0
            }
          }
          goto(translationState)
        }

      translationState
        .whenIsActive {
          HEX0 := segROM(sighex0)
          HEX1 := segROM(sighex1)
          goto(delayState)
        }
    }

    io.HEX0 := hexFSM.HEX0
    io.HEX1 := hexFSM.HEX1
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}



object PlayHistory {
  class TopLevel extends Component {
    @dontName val input = in(UInt(4 bits))
    val history = History(input,2 to 4,init=U(0))
    val result = out(history.clone)
    result := history
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    SpinalVerilog(new TopLevel)
  }
}


object PlayRandBoot{
  class TopLevel extends Component {
    val toto = Reg(UInt(4 bits)) randBoot() dontSimplifyIt()
    toto := 3

    val titi = out(Reg(UInt(4 bits)))
    titi.randBoot()
    titi := toto + 1
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    SpinalVerilog(new TopLevel)
  }
}

object PlayCCBuffer{
  class TopLevel extends Component {
    val result = out(Reg(UInt(3 bits))).addTag(crossClockBuffer).addAttribute("Yolo").randBoot()
    result := result + 2

  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    SpinalVerilog(new TopLevel)
  }
}



object PlayRename{
  class TopLevel extends Component {
    val io = new Bundle{
      val source = slave(AxiLite4(AxiLite4Config(32,32)))
      val sink = master(AxiLite4(AxiLite4Config(32,32)))
    }

    io.sink << io.source

    val toto = Reg(Bool).init(False).keep //Force to keep clock and reset in this small example
  }

  def main(args: Array[String]) {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(
      clockEdge = RISING,
      resetKind = ASYNC,
      resetActiveLevel = LOW
    )).generateVhdl({
      val myIp = new TopLevel
      AxiLite4SpecRenamer(myIp.io.source)
      AxiLite4SpecRenamer(myIp.io.sink)
      ClockDomain.current.clock.setName("fancyClockName")
      ClockDomain.current.reset.setName("fancyResetName")
      myIp
    })
  }
}


object PlayMentorDo{
  case class Packet() extends Bundle{
    val e1,e2 = Bool
  }
  class TopLevel extends Component {
    val io = new Bundle{
      val a,b    = in UInt(8 bits)
      val result = out UInt(8 bits)

      val cmd = slave Stream(Packet())
      val rsp = master Stream(Packet())
    }

    io.result := io.a + io.b

    val fifo = new StreamFifo(Packet(),64)
    fifo.io.push << io.cmd
    fifo.io.pop  >> io.rsp

    fifo.noIoPrefix()
  }

  def main(args: Array[String]) {
    val report = SpinalVhdl(new TopLevel)
    val toplevel = report.toplevel
    MentorDo()
      .add(toplevel,Seq("yolo"),depth=4)
      .build("/toplevel_tb/uut/","mentor.do")
  }
}

object PlayAuto{
  class I2CHAL extends Component{

    val slaveGeneric  = I2CSlaveHALGenerics()
    val masterGeneric = I2CMasterHALGenerics()

    val io = new Bundle{
      val ioSlave = new Bundle {
        val cmd  = master  Flow ( I2CSlaveHALCmd(slaveGeneric) )
        val rsp  = slave Stream ( I2CSlaveHALRsp(slaveGeneric) )
      }
      val ioMaster = new Bundle {
        val cmd    = slave Stream(I2CMasteHALCmd(masterGeneric))
        val rsp    = master Flow (I2CMasterHALRsp (masterGeneric))
      }
    }

    val i2cSlave  = new I2CSlaveHAL(slaveGeneric)
    val i2cMaster = new I2CMasterHAL(masterGeneric)
    val simSDA    = new SimOpenDrain()
    val simSCL    = new SimOpenDrain()

    i2cSlave.io.cmd  <> io.ioSlave.cmd
    i2cSlave.io.rsp  <> io.ioSlave.rsp
    i2cMaster.io.cmd <> io.ioMaster.cmd
    i2cMaster.io.rsp <> io.ioMaster.rsp
    i2cMaster.io.config.setSCLFrequency(2e6)

    simSDA.io.input     <> i2cMaster.io.i2c.sda

    simSDA.io.output.read     := i2cSlave.io.i2c.sda.write
    i2cSlave.io.i2c.sda.read  := simSDA.io.output.write

    simSCL.io.input.read      := i2cMaster.io.i2c.scl.write
    i2cMaster.io.i2c.scl.read := simSCL.io.input.write

    simSCL.io.output.read     := i2cSlave.io.i2c.scl.write
    i2cSlave.io.i2c.scl.read  := simSCL.io.output.write


  }

  class SimOpenDrain extends Component{
    val io = new Bundle{
      val input  = slave ( ReadableOpenDrain(Bool)  )
      val output = master ( ReadableOpenDrain(Bool) )
    }

    val sim = new Area{
      when(io.input.read === False || io.output.read === False){
        io.output.write := False
        io.input.write  := False
      }otherwise{
        io.output.write  := True
        io.input.write   := True
      }
    }
  }

  def main(args : Array[String]): Unit ={
    SpinalConfig(
      mode = Verilog,
      dumpWave = DumpWaveConfig(depth = 0),
      defaultConfigForClockDomains = ClockDomainConfig(clockEdge = RISING, resetKind = ASYNC, resetActiveLevel = LOW),
      defaultClockDomainFrequency  = FixedFrequency(50e6)
    ).generate(new I2CHAL()).printPruned()
  }
}

object PlayImplicitArg{
  class A(implicit x : Int)
//  class B extends A
  def main(args: Array[String]) {
    implicit val answer = 42
    val a = new A

  }
}




object PlayNameRefl{
  class TopLevel extends Component {
    val io = new Bundle{
      val result = out Bool
    }

    val toto = io.result

    toto := False
    val i = 2
    var o = 3
    var titi = False

  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}
object PlayMinMax{
  class TopLevel extends Component {
    val io = new Bundle{
      val result = out Bool
    }

    val toto = io.result

    toto := False
    val i = 2
    var o = 3
    var titi = False
    def apply[T <: Data with Num[T]](nums : T*) : T = list(nums)
    def list[T <: Data with Num[T]](nums: Seq[T] ) : T = { nums.reduceLeft(_ min _) }

    val tata = out(apply(U"00",U"00",U"01"))

  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}


object PlayMask{
  case class MyBundle() extends Bundle{
    val a,b,c = SInt(3 bits)
  }
  class TopLevel extends Component {
    def doIt[T <: Data](that : T) : T = {
      val uint = that.asBits.asUInt
      val masked = uint & ~(uint - 1)
      val ret = that.clone
      ret.assignFromBits(masked.asBits)
      ret
    }
    val result = out(doIt(in(MyBundle())))
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}



object PlayWhenSyntax{


  def main(args: Array[String]) {
    class When{
      def otherwise = new Otherwise
    }
    class Otherwise{
      def  when(cond : Boolean)(block : => Unit) = new When
    }

    def when(cond : Boolean)(block : => Unit) = new When

    when(true){

    }.otherwise.when(false){

    }
  }
}


object PlayPwm{

  object PWMMode extends SpinalEnum {
    val nSingleEdge, nDualEdge = newElement()
  }


  class PWMControl(width: Int) extends Bundle{
    val enable = in Bool
    val period = in UInt(width bits)
    val mode = in(PWMMode)
  }

  class PWMCore(width: Int) extends Component {
    val io = new Bundle {
      val control = new PWMControl(width)
      val duty = in UInt(width+1 bits)
      val output = out Bool
      val outSync = out Bool
    }

    import PWMMode._

    val counter = Reg(UInt(width bits)) init (0)
    val outSyncReg = Reg(Bool)
    val decrementCounter = Reg(Bool) init(False)

    val PwmCounter = new Area {
      val upDownCount = SInt(width bits)
      when(decrementCounter){
        upDownCount := -1
      } otherwise {
        upDownCount := 1
      }

      counter := counter + upDownCount.asUInt
      outSyncReg := False
      // Uper or equal comparator used for safety purposes
      when(counter >= io.control.period) {
        when(io.control.mode === nSingleEdge) {
          counter := 0
        }

        when(io.control.mode === nDualEdge) {
          decrementCounter := True
        }
      }

      when(counter === 0) {
        decrementCounter := False
        outSyncReg := True
      }

      when(!io.control.enable) {
        decrementCounter := False
        counter := 0
      }
    }

    io.output := RegNext(counter < io.duty)
    io.outSync := outSyncReg

    val cond = True
    cond.?[SInt](1)  | 1
  }
}




object PlayResized54{
  class TopLevel extends Component {
    val readAddr = in UInt(4 bits)

    val readData = out Bits(4 bits)
    val mem = Mem(Bits(4 bits), 16)
    readData := mem(readAddr)

    val pc = U(32,32 bits)
    val pcPlus4 = pc + 4
    pcPlus4.addAttribute("keep")

    pcPlus4.keep()
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    SpinalVerilog(new TopLevel)
  }
}



object PlaySwitchError{
  class TopLevel extends Component {
    val value = in Bits(8 bits)
    val temp = UInt(8 bits)
    val result = out UInt(8 bits)
    val toto = B"00001111"
    switch(value){
      is(0){
        temp := 0
      }
      is(1){
        temp := 1
      }
      default{
        temp := 2
      }
    }

    result := RegNext(temp) init(9)
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    SpinalVerilog(new TopLevel)
    assert(doCmd(s"ghdl -a --ieee=synopsys TopLevel.vhd TopLevel_tb.vhd") == 0,"GHDL analysis fail")
    assert(doCmd(s"ghdl -e --ieee=synopsys TopLevel_tb"                   ) == 0,"GHDL elaboration fail")
    assert(doCmd(s"ghdl -r --ieee=synopsys TopLevel_tb --vcd=wave.vcd"    ) == 0,"GHDL simulation fail")
    println("SUCCESS")
  }

  def doCmd(cmd : String) : Int = {
    import scala.sys.process._
    println(cmd)
    cmd !
  }
}




object PlayCall{
  class TopLevel extends Component {
    val a,b,c = in UInt(8 bits)
    val result = out(Reg(UInt(8 bits)))
    result := result + a + b + c

    val clear = Callable(result := 0)

    when(result > 42){
      clear.call()
    }
    when(result === 11){
      clear.call()
    }

  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    SpinalVerilog(new TopLevel)
  }
}


object PlaySyntaxCheck{
  val inputStream = Stream(Bits(8 bits))
  val dispatchedStreams = StreamDispatcherSequencial(
    input = inputStream,
    outputCount = 3
  )
}


object PlayNameableIssue{
  class TopLevel extends Component {
    val arbiterRoundRobinInputs =  Vec(slave Stream(Bits(8 bits)),3)
    val arbiterRoundRobinOutput =  master Stream(Bits(8 bits))
    arbiterRoundRobinOutput << StreamArbiterFactory.roundRobin.on(arbiterRoundRobinInputs)
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}


object PlayNameableIssue2{
  class TopLevel extends Component {
    val arbiterRoundRobinInputs =  Vec(slave Stream(Bits(8 bits)),3)
    val arbiterRoundRobinOutput =  master Stream(Bits(8 bits))
    arbiterRoundRobinOutput << StreamArbiterFactory.roundRobin.on(arbiterRoundRobinInputs)
  }

  def main(args: Array[String]) {
    SpinalVhdl(new StreamArbiter(Bits(8 bits),3)(StreamArbiter.Arbitration.lowerFirst,StreamArbiter.Lock.transactionLock))
  }
}
object PlayNameableIssue3{
  class TopLevel extends Component {
    val cmd =  slave Stream(Bits(8 bits))
    val rsp =  master Stream(Bits(8 bits))
    cmd.queue(16) >> rsp
  }

  def main(args: Array[String]) {
    SpinalConfig(globalPrefix = "yolo_").generateVhdl(new TopLevel().setDefinitionName("miaou"))
  }
}


object PlayBug54{
  class TopLevel extends Component {
    val a = in Bool
    val fixedWidth = new Area {
      val resultBits = out Bits (8 bits)
      resultBits := B(7 -> false, (6 downto 0) -> a)
      val resultUInt = out UInt (8 bits)
      resultUInt := U(7 -> false, (6 downto 0) -> a)
      val resultSInt = out SInt (8 bits)
      resultSInt := S(7 -> false, (6 downto 0) -> a)
    }
    val unfixedWidth = new Area {
      val resultBits = out Bits (8 bits)
      resultBits := (4 -> False,default -> a)
      val resultUInt = out UInt (8 bits)
      resultUInt := (4 -> false,default -> a)
      val resultSInt = out SInt (8 bits)
      resultSInt := ((4 downto 0) -> false,default -> a)
    }

    val unfixeConstdWidth = new Area {
      val resultBits = out Bits (8 bits)
      resultBits := (4 -> False,default -> True)
      val resultUInt = out UInt (8 bits)
      resultUInt := (4 -> false,default -> True)
      val resultSInt = out SInt (8 bits)
      resultSInt := ((4 downto 0) -> false,default -> True)
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel())
    SpinalVerilog(new TopLevel())
  }
}

object PlayBug5441{
  object State extends SpinalEnum{
    val s0,s1,s2,s3 = newElement()
  }
  class TopLevel extends Component {
    val tmp = State()
    val result = out(State)
    result := tmp
    tmp := result
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel())
  }
}

object PlayBug544441{
  class TopLevel extends Component {
//    val condA = in Bool
//    val condB = in Bool
//    val result = out Bits(8 bits)
//    result(3 downto 2) := 0
//    when(condA){
////      when(condB){
//        result := 4
////      }
//
//    }

    val a,b,c = out Bits(8 bits)
    a := "0000_0001"
    b := "x55"
    c.:=("xAA")
    
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel())
  }
}



object Play928267{
  class SPIConfig extends Bundle {
    val CPOL = in Bool
    val CPHA = in Bool
    val Prescale = in UInt(8 bits)
  }

  class SPIMaster(nEnables: Int) extends Component {
    val io = new Bundle{
      val configIf = new SPIConfig
      val inputData = slave Stream(Bits(8 bits))
      val sck = out Bool
      val miso = in Bool
      val mosi = out Bool
      val slvsel = out Bits(nEnables bits)
    }

    val sckReg = Reg(Bool) init(False)
    sckReg := io.configIf.CPOL

    val clockGen = new Area {
      val sckCounter = Reg(UInt(8 bits)) init(0)
      sckCounter := sckCounter + 1
      when (sckCounter === io.configIf.Prescale) {
        sckReg := !sckReg
        sckCounter := 0
      }
    }


    val dataOutReg = Reg(Bits(8 bits))
    val masterFsm = new StateMachine {
      io.inputData.ready := False

      val Idle : State = new State with EntryPoint {
        io.inputData.ready := True
        whenIsActive {
          when (io.inputData.valid) {
            dataOutReg := io.inputData.payload
            goto(ShiftData)
          }
        }
      }

      val ShiftData : State = new State {
        whenIsActive {
          goto(Idle)
        }
      }

    }
    io.sck := sckReg
    io.mosi := False
    io.slvsel := B(default -> true)
  }
  def main(args: Array[String]) {
    SpinalConfig().dumpWave().generateVhdl(new SPIMaster(4)).printPruned()
    //ou
    SpinalConfig(dumpWave = DumpWaveConfig()).generateVhdl(new SPIMaster(4)).printPruned()
  }
}


object PlayLogicLock{
  class TopLevel extends Component {
    val sel = in UInt(4 bits)
    val result = out Bits(8 bits)

    switch(sel){
      is(0){
        result := 3
      }
      default{
        result := 4
      }
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}


object PlaySimple{
  class TopLevel extends Component {
    val a,b = in UInt(8 bits)
    val result = out UInt(8 bits)
    result := a + b
  }

  def main(args: Array[String]) {
    SpinalConfig().addTransformationPhase(new PhaseDummy(println("MIAOU"))).generateVhdl(new TopLevel)
  }
}



object PlayRamBB{
  class TopLevel extends Component {
    val clockB = in Bool

    val rgbConfig = RgbConfig(5,6,5)
    val mem = Mem(Rgb(rgbConfig),1 << 16).setAsBlackBox()

    val writePort = in(mem.writePort)
    val readSyncPort = slave(mem.readSyncPort)
    val readAsyncAddr = in UInt(16 bits)
    val readAsyncData = out(mem.readAsync(readAsyncAddr))


    val clockBArea = new ClockingArea(ClockDomain(clockB)){
      val readSyncAddr = in UInt(16 bits)
      val readSyncEn = in Bool
      val readSyncPort = out(mem.readSyncCC(readSyncAddr,readSyncEn))
    }
//    MemBlackBoxer.applyOn(mem)
  }

  def main(args: Array[String]) {
    SpinalConfig().
      addStandardMemBlackboxer(blackboxAll).
      generateVhdl(new TopLevel)
  }
}

object PlayProcess{
  class TopLevel extends Component {
    val a,b,c = in Bool
    val result = out UInt(8 bits)

    val combinatorial   =     UInt(8 bits)
    val regWithoutReset = Reg(UInt(8 bits))
    val regWithReset    = Reg(UInt(8 bits)) init(0)

    when(a){
      combinatorial := 0
      when(b){
        regWithoutReset := regWithoutReset + 1
        when(c){
          combinatorial := 2
        }
      }
    }otherwise{
      regWithReset := regWithReset + 1
      combinatorial := 1
    }

    result := combinatorial + regWithoutReset + regWithReset
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}



object PlayHandshake{
  case class Handshake[T <: Data](dataType : T) extends Bundle{
    val valid   = Bool
    val ready   = Bool
    val payload = cloneOf(dataType)  //Make another instance of 'dataType'

    def asMaster() : Handshake[T] = {
      out(valid,payload)
      in(ready)
      this
    }

    def asSlave() : Handshake[T] = {
      in(valid,payload)
      out(ready)
      this
    }
  }

  class ArbiterLowPortFirst[T <: Data](dataType : T,inputsCount : Int) extends Component {
    val io = new Bundle{
      val inputs = Vec(Handshake(dataType).asSlave(),size = inputsCount)
      val output = Handshake(dataType).asMaster()
    }
    
    var free = True
    io.output.valid := False
    io.output.payload := io.inputs(0).payload //default
    for(input <- io.inputs){
      input.ready := io.output.ready && free
      when(input.valid && free){
        free \= False
        io.output.valid := True
        io.output.payload := input.payload
      }
    }
  }

  case class RGB(redWidth : Int,greenWidth : Int,blueWidth : Int) extends Bundle{
    val r = UInt(redWidth bits)
    val g = UInt(greenWidth bits)
    val b = UInt(blueWidth bits)
  }

  class TopLevel extends Component{
    val io = new Bundle{
      val a      = Handshake(RGB(5,6,5)).asSlave
      val b      = Handshake(RGB(5,6,5)).asSlave
      val c      = Handshake(RGB(5,6,5)).asSlave
      val output = Handshake(RGB(5,6,5)).asMaster
    }

    val arbiter = new ArbiterLowPortFirst(
      dataType = RGB(5,6,5),
      inputsCount = 3
    )

    arbiter.io.inputs(0) <> io.a
    arbiter.io.inputs(1) <> io.b
    arbiter.io.inputs(2) <> io.c
    arbiter.io.output    <> io.output
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}

object PlayMasterSlave{
  class APB( addressWidth: Int,
              dataWidth: Int,
              selWidth : Int,
              useSlaveError : Boolean) extends Bundle with IMasterSlave {

    val PADDR      = UInt(addressWidth bit)
    val PSEL       = Bits(selWidth bits)
    val PENABLE    = Bool
    val PREADY     = Bool
    val PWRITE     = Bool
    val PWDATA     = Bits(dataWidth bit)
    val PRDATA     = Bits(dataWidth bit)
    val PSLVERROR  = if(useSlaveError) Bool else null   //This wire is added to the bundle only when useSlaveError is true

    override def asMaster(): this.type = {
      out(PADDR,PSEL,PENABLE,PWRITE,PWDATA)
      in(PREADY,PRDATA)
      if(useSlaveError) in(PSLVERROR)
      this
    }
  }

  case class APBConfig(addressWidth: Int,
                       dataWidth: Int,
                       selWidth : Int,
                       useSlaveError : Boolean)

  class APB3(val config: APBConfig) extends Bundle with IMasterSlave {
    val PADDR      = UInt(config.addressWidth bit)
    val PSEL       = Bits(config.selWidth bits)
    val PENABLE    = Bool
    val PREADY     = Bool
    val PWRITE     = Bool
    val PWDATA     = Bits(config.dataWidth bit)
    val PRDATA     = Bits(config.dataWidth bit)
    val PSLVERROR  = if(config.useSlaveError) Bool else null

    override def asMaster(): this.type = {
      out(PADDR,PSEL,PENABLE,PWRITE,PWDATA)
      in(PREADY,PRDATA)
      if(config.useSlaveError) in(PSLVERROR)
      this
    }
  }


  class MyBundle extends Bundle{
    val publicElement = Bool
    private val privateElement = Bool

    def getPrivateElement() : Bool = {
      return privateElement
    }

    def setPrivateElement(value : Bool) : Unit = {
      privateElement := value
    }

    def setPrivateElementByAnAbstractWay(trigger : UInt) : Unit = {
      when(trigger > 10) {
        privateElement := True
      }
    }
  }


//  class RGB(channelWidth : Int) extends Bundle{
//    val r,g,b = UInt(channelWidth bits)
//
//    def isClear() : Bool ={
//      return r === 0 && g === 0 && b === 0
//    }
//  }
//
//  class RGBA(channelWidth : Int) extends RGB(channelWidth){
//    val a = UInt(channelWidth bits)
//
//    override def isClear() : Bool ={
//      return super.isClear() && a === 0
//    }
//  }

  case class Handshake[T <: Data](dataType : T) extends Bundle{
    val valid   = Bool
    val ready   = Bool
    val payload = cloneOf(dataType)  //All data type that i give to the Handshake class should implement the clone method
  }

  case class RGB() extends Bundle{
    val r,g,b = UInt(8 bits)
  }

  class RGBA extends Bundle{
    val r,g,b,a = UInt(8 bits)

    override def clone: this.type = new RGBA().asInstanceOf[this.type]  //RGBA is not a "case class", To to be abble to clone it in the Handhsake, we need to implement this.
  }


  implicit class Imp(toto : Toto){
    def aaa = 2
    override def clone() : Toto = new Toto
  }
  class Toto{

  }

  val toto = new Toto
  toto.aaa
//  toto.clo

//
//  object Handshake{
//    def apply() = new Handshake
//  }
//  class Handshake extends Bundle with MasterSlave{
//    @masterx val valid = Bool
//    @masterx val payload = Bits(8 bits)
//  }
////  implicit def masterToHandshake()
//  class TopLevel extends Component {
//    val rsp = master(new Handshake)
//  }
//
//  def main(args: Array[String]) {
//    SpinalVhdl(new TopLevel)
//  }
}



object PlayRegTriplify{
  def triplifyReg(regOutput : BaseType) : Unit = {

    def cloneAssignementTree(finalOutput : Node,node : Node,into : Node,intoId : Int) : Unit = {
      node match {
        case node : MultipleAssignmentNode => {
          val cpy = node.cloneMultipleAssignmentNode
          for(i <- 0 until node.inputs.length) cpy.inputs += null.asInstanceOf[cpy.T]
          node.onEachInput((input,inputId) => cloneAssignementTree(finalOutput,input,cpy,inputId))
          into.setInput(intoId,cpy)
        }
        case node : WhenNode => {
          val cpy = node.cloneWhenNode
          node.onEachInput((input, inputId) => cloneAssignementTree(finalOutput,input, cpy, inputId))
          into.setInput(intoId,cpy)
        }
        case node : AssignementNode => {
          val cpy = node.clone(finalOutput)
          node.onEachInput((input, inputId) => cloneAssignementTree(finalOutput,input, cpy, inputId))
          into.setInput(intoId,cpy)
        }
        case node => into.setInput(intoId,node)
      }
    }

    def cloneReg(outBaseType : BaseType,that : Reg) : Reg = {
      val clone = that.cloneReg()
      cloneAssignementTree(outBaseType,that.dataInput,clone,RegS.getDataInputId)
      cloneAssignementTree(outBaseType,that.initialValue,clone,RegS.getInitialValueId)
      clone.dataInput match {
        case node : MultipleAssignmentNode =>{
          if(node.inputs.head.isInstanceOf[Reg]) node.setInput(0,clone)
        }
        case _ =>
      }
      clone
    }

    val originalReg = regOutput.input.asInstanceOf[Reg]
    val regs = for(i <- 0 to 2) yield {
      val baseType = regOutput.clone()
      baseType.input = cloneReg(baseType,originalReg)
      baseType.setPartialName(regOutput,i.toString)
      baseType
    }

    regOutput.input = null
    regOutput.compositeAssign = null

    regOutput match {
      case regOutput : Bool => {
        val r0 = regs(0).asInstanceOf[Bool]
        val r1 = regs(1).asInstanceOf[Bool]
        val r2 = regs(2).asInstanceOf[Bool]
        regOutput.assignFrom((r0 & r1) | (r0 & r2) | (r1 & r2) ,false)
      }
      case regOutput : Bits => {
        val r0 = regs(0).asInstanceOf[Bits]
        val r1 = regs(1).asInstanceOf[Bits]
        val r2 = regs(2).asInstanceOf[Bits]
        regOutput.assignFrom((r0 & r1) | (r0 & r2) | (r1 & r2) ,false)
      }
      case regOutput : UInt => {
        val r0 = regs(0).asInstanceOf[UInt]
        val r1 = regs(1).asInstanceOf[UInt]
        val r2 = regs(2).asInstanceOf[UInt]
        regOutput.assignFrom((r0 & r1) | (r0 & r2) | (r1 & r2) ,false)
      }
      case regOutput : SInt => {
        val r0 = regs(0).asInstanceOf[SInt]
        val r1 = regs(1).asInstanceOf[SInt]
        val r2 = regs(2).asInstanceOf[SInt]
        regOutput.assignFrom((r0 & r1) | (r0 & r2) | (r1 & r2) ,false)
      }
    }

    regOutput.compositeAssign = new Assignable {
      override def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = {
        regs.foreach(_.input.asInstanceOf[Reg].assignFrom(that,conservative))
      }
    }
  }

  class TopLevel extends Component {
    val cond = in Bool
    val a,b = in UInt(8 bits)
    val result = out UInt(8 bits)
    val counter = Reg(UInt(8 bits))


    when(cond){
      counter := a + b
    }
    when(counter > 54){
      counter(4) := False
    }

    triplifyReg(counter)

    when(counter === 34){
      counter := 3
    }

    result := counter
  }

  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new TopLevel)
  }
}

