package spinal.tester.code


import java.io.{PrintWriter, ByteArrayOutputStream}
import java.util

import spinal.core._
import spinal.lib._
import spinal.lib.bus.neutral.NeutralStreamDma
import spinal.lib.com.uart.UartCtrl
import spinal.lib.graphic.RgbConfig
import spinal.lib.graphic.vga.{AvalonMMVgaCtrl, VgaCtrl}
import spinal.lib.com.i2c._


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
    val b = StreamArbiterCore().inOrder.noLock.build(a,10)
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
    val address = in UInt(4 bits)
    val writeData = in Bits(8 bits)
    val chipSelect = in Bool
    val writeEnable = in Bool
    val readData = out Bits(8 bits)

    val mem = Mem(Bits(8 bits),16)
    readData := mem.writeOrReadSync(address,writeData,chipSelect,writeEnable)




    address := 0
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
    val t0 = out(B(5 -> true, default -> false))
    val t1 = out Bits (10 bits)
    t1 := B(5 -> true, default -> false)


    val t2 = out(B(5 -> true, default -> False))
    val t3 = out Bits (10 bits)
    t3 := B(5 -> true, default -> False)
  }


  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PlayI2CMasterHAL {

  class TopLevel_I2CMasterHAL extends Component {

    val generic = I2CMasterHALGenerics()

    val io = new Bundle {
      val i2c    = master( I2C() )
      val config = in( I2CMasterHALConfig(generic) )
      val cmd    = slave Stream(I2CMasteHALCmd(generic))
      val rsp    = master Flow(I2CMasterHALRsp (generic))
    }


    val myMasterI2C = new I2CMasterHAL(generic)
    myMasterI2C.io.config.setFrequency(1e6)
    io <> myMasterI2C.io
  }

  def main(args: Array[String]) {
    SpinalConfig(
      mode = Verilog,
      dumpWave = true,
      defaultConfigForClockDomains = ClockDomainConfig(clockEdge = RISING, resetKind = ASYNC, resetActiveLevel = LOW),
      defaultClockDomainFrequency = FixedFrequency(50e6)
    ).generate(new TopLevel_I2CMasterHAL).printPruned
  }
}
