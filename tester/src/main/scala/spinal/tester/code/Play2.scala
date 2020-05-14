package spinal.tester.code


import java.io.{PrintWriter, ByteArrayOutputStream}
import java.util

import spinal.core.internals.Operator.UInt.Add
import spinal.core._
import spinal.core.internals._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3SlaveFactory, Apb3}
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}
import spinal.lib.bus.avalon._
import spinal.lib.bus.amba4.axilite.{AxiLite4SpecRenamer, AxiLite4Config, AxiLite4}
import spinal.lib.experimental.bus.neutral.NeutralStreamDma
import spinal.lib.com.uart._
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
  val myBits = Bits()
  val myBool = Bool
  val yolo =  myBits.asBools.map(_ ^ myBool).asBits()
  val yolo2 = myBits ^ B(myBits.range -> myBool)
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
  class Top extends Component{
    val a = in SInt(8 bits)
    val b = out(a(10))
  }
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Top)
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
    }

    mapClockDomain(clock=io.clk)
    replaceStdLogicByStdULogic()
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
    val ram_1w_1r = new Ram_1w_1r(8,16)

    io.wr.en <> ram_1w_1r.io.wr.en
    io.wr.addr <> ram_1w_1r.io.wr.addr
    io.wr.data <> ram_1w_1r.io.wr.data
    io.rd.en   <> ram_1w_1r.io.rd.en
    io.rd.addr <> ram_1w_1r.io.rd.addr
    io.rd.data <> ram_1w_1r.io.rd.data


  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

/*
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
    val yolo = Vec(Bits(32 bits), 4)
    val titi = yolo.map(_)
    (yolo,titi).zipped.foreach(_ := _.resized)
    titi

  }

  def main(args: Array[String]): Unit = {
    //SpinalVhdl(new TopLevel)
    SpinalVhdl(new TopLevel)
  }
}*/

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
    readData := mem.readWriteSync(address,writeData,chipSelect,writeEnable)




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
    val x = in Bits(5 bits)
    when(a.lsb) {
      when(a.msb) {
       assert(tmp === 8, "Yolo" :: tmp :: a ::" miaou" :: Nil, NOTE)
        report("Yolo" :: tmp :: a ::" miaou" ::  "   " :: (x | x) ::Nil)
        report("miaou " :: tmp :: Nil)
       report("misou")
      }
    }
  }

  def main(args: Array[String]): Unit = {
   // SpinalVhdl(new TopLevel)
    SpinalVerilog(new TopLevel)
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
  import spinal.core.sim._
  import spinal.core._
  import spinal.sim._
  import spinal.lib._
  import spinal.lib.fsm._
  import scala.util.Random

  class BootFail extends Component {
    val io = new Bundle {
      val trig = in Bool
      val sig = out Bool
    }

    io.sig := False
    val sm = new StateMachine {
      val idle: State = new State with EntryPoint {
        whenIsActive {
          when (io.trig) {
            goto(c1)
          }
        }
      }
      val c1: State = new State {
        whenIsActive {
          io.sig := True
          goto(idle)
        }
      }
      val unreachableState: State = new State {
        whenIsActive {
          assert(False, "wound up in unreachableState", FAILURE)
          io.sig := False
          goto(idle)
        }
      }
    }
  }

  def main(args: Array[String]) {
    val compiledSim = SimConfig.withWave.compile(new BootFail)

    for (i <- 0 to 1000000) {
      print("running case %d\n".format(i))
      // examples of seeds that lead to failure:
      // 814316022, 1851891454, 533866718, 1197810070
      compiledSim.doSimUntilVoid("case") { dut =>
        dut.clockDomain.forkStimulus(period=10)

        val trigval = i % 2 == 0
        dut.io.trig #= trigval

        dut.clockDomain.waitSampling(10)
        simSuccess()
      }
    }
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
    val sel2 = in UInt(2 bits)
    val sel3 = in Bool
    val result = out UInt(8 bits)

//    result := 5
    switch(sel){
      is(MyEnum.a){
        result := 0
      }
      is(MyEnum.b){
        switch(sel2){
          is(0){
            result := 0
          }
          is(1){
            result := 1
          }
          is(2) {
            result := 2
          }
          is(3){
            result := 3
          }
        }
      }
      is(MyEnum.c){
        result := 2
      }
    }


    val result2 = out UInt(8 bits)
    switch(sel2){
      is(0){
        result2 := 0
      }
      is(1){
        result2 := 1
      }

//      is(1){
//        result2 := 1
//      }
      is(2){
        result2 := 2
      }
//      is(1){
//        result2 := 1
//      }
      is(3){
        result2 := 3
      }
    }



    val result3 = out UInt(8 bits)
    result3 := sel2.mux(
      0 -> U"x00",
      1 -> U"x01",
      2 -> U"x02",
//      2 -> U"x02",
//      default -> U"xFF",
//      default -> U"xFF"
      3 -> U"x03"
    )


    val result4 = out UInt(8 bits)
    switch(sel3){
      is(False){
        result4 := 0
      }
      is(True){
        result4 := 1
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
    }elsewhen(sel(4)){
      result := 6
    }

    when(sel(0)){
      result := 0
    }otherwise{
      result := 1
      result2 := 1
      when(sel(1)){
        result := 2
      }elsewhen(sel(2)){
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


/*
object PlayI2CIoLayer{

  class I2CIoLayerTester extends Component{

    val slaveGeneric  = I2CIoSlaveGenerics()
    val masterGeneric = I2CIoMasterGenerics()

    val io = new Bundle{
      val ioSlave = new Bundle {
        val cmd  = master  Flow ( I2CIoSlaveCmd() )
        val rsp  = slave Stream ( I2CIoSlaveRsp() )
      }
      val ioMaster = new Bundle {
        val cmd    = slave Stream(I2CIoMasterCmd())
        val rsp    = master Flow (I2CIoMasterRsp ())
      }

      // output sda and scl in order to monitor the i2c bus
      val sda = out Bool
      val scl = out Bool
    }

    val i2cSlave  = new I2CIoSlave(slaveGeneric)
    val i2cMaster = new I2CIoMaster(masterGeneric)


    i2cSlave.io.cmd  <> io.ioSlave.cmd
    i2cSlave.io.rsp  <> io.ioSlave.rsp
    i2cMaster.io.cmd <> io.ioMaster.cmd
    i2cMaster.io.rsp <> io.ioMaster.rsp
    i2cMaster.io.config.setTimerFrequency(2 MHz)
    i2cMaster.io.config.setFrequencySampling(5 MHz)
    i2cSlave.io.config.setFrequencySampling(5 MHz)

    io.sda := i2cMaster.io.i2c.sda.read
    io.scl := i2cMaster.io.i2c.scl.read


    interconnect(Seq(i2cMaster.io.i2c.scl, i2cSlave.io.i2c.scl))
    interconnect(Seq(i2cMaster.io.i2c.sda, i2cSlave.io.i2c.sda))



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
        defaultClockDomainFrequency  = FixedFrequency(50 MHz)
    ).generate(new I2CIoLayerTester()).printPruned()
  }
}*/




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
//  class TopLevel extends Component {
//    val io = new Bundle{
//      val a,b    = in UInt(8 bits)
//      val result = out UInt(8 bits)
//
//      val cmd = slave Stream(Packet())
//      val rsp = master Stream(Packet())
//    }
//
//    io.result := io.a + io.b
//
//    val fifo = new StreamFifo(Packet(),64)
//    fifo.io.push << io.cmd
//    fifo.io.pop  >> io.rsp
//
//    fifo.noIoPrefix()
//  }
  class Sub extends Component{
    val a =  True.keep()
  }
  class TopLevel extends Component{
    val areaList = List.fill(2)( new Area{
      val area = new Area {
        val subs = List.fill(4)(new Sub)
      }
    })
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
  /*class I2CHAL extends Component{

    val slaveGeneric  = I2CIoSlaveGenerics()
    val masterGeneric = I2CIoMasterGenerics()

    val io = new Bundle{
      val ioSlave = new Bundle {
        val cmd  = master  Flow ( I2CIoSlaveCmd() )
        val rsp  = slave Stream ( I2CIoSlaveRsp() )
      }
      val ioMaster = new Bundle {
        val cmd    = slave Stream(I2CIoMasterCmd())
        val rsp    = master Flow (I2CIoMasterRsp ())
      }
    }

    val i2cSlave  = new I2CIoSlave(slaveGeneric)
    val i2cMaster = new I2CIoMaster(masterGeneric)
    val simSDA    = new SimOpenDrain()
    val simSCL    = new SimOpenDrain()

    i2cSlave.io.cmd  <> io.ioSlave.cmd
    i2cSlave.io.rsp  <> io.ioSlave.rsp
    i2cMaster.io.cmd <> io.ioMaster.cmd
    i2cMaster.io.rsp <> io.ioMaster.rsp
    i2cMaster.io.config.setTimerFrequency(2 MHz)

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
      defaultClockDomainFrequency  = FixedFrequency(50 MHz)
    ).generate(new I2CHAL()).printPruned()
  }*/
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
  case class MemConfig(dualPorted : Boolean)
  case class MyBus() extends Bundle with IMasterSlave{
    val adr = UInt(16 bits)
    val writeData = Bits(16 bits)
    val readData = Bits(16 bits)

    override def asMaster(): Unit = {
      out(adr,writeData)
      in(readData)

    }
  }

  case class CoreMem(cfg : MemConfig) extends Component{
    val io = new Bundle{
      val portA = MyBus()
      val portB = if(cfg.dualPorted) MyBus() else null
    }
  }

  case class MyBundle() extends Bundle{
    val a,b,c = SInt(3 bits)
  }
  class TopLevel extends Component {
    def doIt[T <: Data](that : T) : T = {
      val uint = that.asBits.asUInt
      val masked = uint & ~(uint - 1)
      val ret = cloneOf(that)
      ret.assignFromBits(masked.asBits)
      ret
    }
    val result = out(doIt(in(MyBundle())))
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
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
    val io = new Bundle {
      val x = in UInt(32 bits)
    }
    val y = Vec(io.x)
  }

  def main(args: Array[String]) {
    SpinalVerilog(new TopLevel)
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
    SpinalConfig().withPrivateNamespace.generateVhdl(new TopLevel())
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

object PlaySV{

  class Parameters{
    def toSystemVerilog(): Unit ={

    }
  }



  case class SocParameters( a : Int,
                            b : Int,
                            c : Int) extends Parameters

}


object Play1adasd {

  class Logic {
    val a, b, c, d, e, f, g, h, i, j = Bool()
    val x,y,z = Reg(Bool())
    b := True
    a := a || c
    x := d || y
    c := b

    when(c) {
      e := d
      when(d) {
        f := e
        e := f
      }
      b := f
    }elsewhen(a) {
      val x = Bool()
      x := a || b
      i := g || x

    } otherwise {
      b := j
    }
  }
  class TopLevel extends Component{
    var l = ArrayBuffer[Logic]()
    l.sizeHint(1100000)
    SpinalProgress("TOP START")
    var idx = 0
    while(idx < 500000) {
      idx += 1
      l += new Logic
      if(idx % 10000 == 0) println(idx)
    }
    l = null
    SpinalProgress("TOP END")
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel

    var statementCount, expressionCount = 0

    print("DONE " + toplevel.getName())
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
    val mem = Mem(Rgb(rgbConfig),1 << 16)

////    val writePort = in(mem.writePort)
//    val writeAddr = in UInt(17 bits)
//    val writeData = in UInt(8 bits)
//    val writeEnable = in Bool()
//    when(writeEnable) {
//      mem.writeMixedWidth(writeAddr, writeData)
////      mem.writeMixedWidth(writeAddr, writeData)
//    }
//
//    val readAsyncAddr = in UInt(16 bits)
//    val readAsyncData = out(mem.readAsync(readAsyncAddr))
//    val readAsyncMixedWidthAddr = in UInt(17 bits)
//    val readAsyncMixedWidthData = out UInt(8 bits)
//    mem.readAsyncMixedWidth(readAsyncMixedWidthAddr,readAsyncMixedWidthData)
//
//    val readSyncPort = slave(mem.readSyncPort)
//    val readSyncMixedWidthEnable = in Bool
//    val readSyncMixedWidthAddr = in UInt(17 bits)
//    val readSyncMixedWidthData = out UInt(8 bits)
//    mem.readSyncMixedWidth(readSyncMixedWidthAddr,readSyncMixedWidthData,readSyncMixedWidthEnable)
//

    val readWrite = new Area {
      val en, wr = in Bool
      val addr = in UInt (16 bits)
      val wrData = in(Rgb(rgbConfig))
      val wrMask = in Bits (4 bits)
      val rdData = out(Rgb(rgbConfig))
      rdData := mem.readWriteSync(addr, wrData, en, wr, wrMask)
    }

    val readWriteMixedWidth = new Area {
      val en, wr = in Bool
      val addr = in UInt (18 bits)
      val wrData = in(Bits(4 bits))
      val wrMask = in Bits (4 bits)
      val rdData = out(Bits (4 bits))
      rdData := mem.readWriteSyncMixedWidth(addr, wrData, en, wr, wrMask)
    }

    val clockBArea = new ClockingArea(ClockDomain(clockB)){
      val readSyncAddr = in UInt(16 bits)
      val readSyncEn = in Bool
//      val readSyncPort = out(mem.readSyncCC(readSyncAddr,readSyncEn))
    }

    mem.generateAsBlackBox
  }

  def main(args: Array[String]) {
    SpinalConfig()
      .addStandardMemBlackboxing(blackboxOnlyIfRequested)
      .generateVhdl(new TopLevel)

    SpinalConfig()
      .addStandardMemBlackboxing(blackboxOnlyIfRequested)
      .generateVerilog(new TopLevel)

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

    override def asMaster(): Unit = {
      out(PADDR,PSEL,PENABLE,PWRITE,PWDATA)
      in(PREADY,PRDATA)
      if(useSlaveError) in(PSLVERROR)
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

    override def asMaster(): Unit = {
      out(PADDR,PSEL,PENABLE,PWRITE,PWDATA)
      in(PREADY,PRDATA)
      if(config.useSlaveError) in(PSLVERROR)
    }
  }


  class MyBundleBroken extends Bundle{
    val publicElement = Bool
    val privateElement = Bool

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



//object PlayRegTriplify{
//  def triplifyReg(regOutput : BaseType) : Unit = {
//    val originalReg = regOutput.input.asInstanceOf[Reg]
//
//    //Create 3 equivalent registers
//    val regs = for(i <- 0 to 2) yield {
//      val baseType = regOutput.clone()
//      baseType.input = Node.cloneReg(baseType,originalReg)
//      baseType.setPartialName(regOutput,i.toString)
//      baseType
//    }
//
//    regOutput.input = null
//    regOutput.compositeAssign = null
//
//    regOutput match {
//      case regOutput : Bool => {
//        val r0 = regs(0).asInstanceOf[Bool]
//        val r1 = regs(1).asInstanceOf[Bool]
//        val r2 = regs(2).asInstanceOf[Bool]
//        regOutput.assignFrom((r0 & r1) | (r0 & r2) | (r1 & r2))
//      }
//      case regOutput : Bits => {
//        val r0 = regs(0).asInstanceOf[Bits]
//        val r1 = regs(1).asInstanceOf[Bits]
//        val r2 = regs(2).asInstanceOf[Bits]
//        regOutput.assignFrom((r0 & r1) | (r0 & r2) | (r1 & r2))
//      }
//      case regOutput : UInt => {
//        val r0 = regs(0).asInstanceOf[UInt]
//        val r1 = regs(1).asInstanceOf[UInt]
//        val r2 = regs(2).asInstanceOf[UInt]
//        regOutput.assignFrom((r0 & r1) | (r0 & r2) | (r1 & r2))
//      }
//      case regOutput : SInt => {
//        val r0 = regs(0).asInstanceOf[SInt]
//        val r1 = regs(1).asInstanceOf[SInt]
//        val r2 = regs(2).asInstanceOf[SInt]
//        regOutput.assignFrom((r0 & r1) | (r0 & r2) | (r1 & r2))
//      }
//    }
//
//    //Allow to reassign the triplified register even after this call
//    regOutput.compositeAssign = new Assignable {
//      override def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = {
//        regs.foreach(_.input.asInstanceOf[Reg].assignFrom(that))
//      }
//    }
//  }
//
//  class TopLevel extends Component {
//    val cond = in Bool
//    val a,b = in UInt(8 bits)
//    val result = out UInt(8 bits)
//    val counter = Reg(UInt(8 bits))
//
//
//    when(cond){
//      counter := a + b
//    }
//    when(counter > 54){
//      counter(4) := False
//    }
//
//    when(counter === 34){
//      counter := 3
//    }
//
//
//    triplifyReg(counter)
//
//    result := counter
//  }
//
//  def main(args: Array[String]) {
//    SpinalConfig().generateVhdl(new TopLevel)
//  }
//}

object PlayBigDecimal{
  def main(args: Array[String]) {
    val a = BigDecimal(1)
    val b = BigDecimal(3)

    val x = a/b



    import spinal.core._
    val frequency = 100 MHz
    val periode = 100 us
    val cycles = frequency*periode
    println(cycles)

    val timeoutLimit = 3 mn
    val timeoutCycles = frequency*timeoutLimit
    println(timeoutCycles)


  }
}
//val context = new AssignmentLevel(process.nodes.map(n => AssignmentLevelCmd(n,n.getInput(0))))

object PlaySwitchEmit{
  class TopLevel extends Component {
    val sel = in UInt(2 bits)
    val x,y,z = out UInt(8 bits)

    x := 0
    y := 0
    switch(sel){
      is(0) {
        x := 1
        z := 0
      }
      is(1){
        y := 1
        z := 1
      }
      is(2){
        y := 1
        z := 2
      }
      default{
      //is(3){
        y := 1
        z := 3
      }
    }
  }

  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new TopLevel)
  }
}


object PlayBufferCC{
  class TopLevel extends Component {
    val input = in Bits(8 bits)
    val output = out Bits(8 bits)
    output := BufferCC(input)
  }

  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new TopLevel).printPruned()
  }
}



object MemTest2 {

  class TopLevel extends Component{

    val io = new Bundle{
      val rw      = in Bool
      val cs      = in Bool
      val dataIn  = in Bits(8 bits)
      val addr    = in UInt(8 bits)
      val dataOut = out Bits(8 bits)
    }

    val memory = Mem(Bits(8 bits), 255)
    io.dataOut := 0

    when(io.cs){
      when(io.rw){
        memory(io.addr) := io.dataIn
      }otherwise{
        io.dataOut := memory.readSync(address = io.addr)
      }
    }

      memory.generateAsBlackBox()
  }
  def main(args:Array[String]){
    SpinalConfig(mode=VHDL).addStandardMemBlackboxing(blackboxOnlyIfRequested).generate(new TopLevel).printPruned()
  }
}


object PlayDualPort{
  class TopLevel(wordWidth : Int,
                 wordCount : Int) extends Component {

    val addressWidth = log2Up(wordCount)
    val io = new Bundle {
      val portA = new Bundle {
        val en = in Bool
        val wr = in Bool
        val addr = in UInt (addressWidth bits)
        val wrData = in Bits (wordWidth bits)
        val rdData = out Bits (wordWidth bits)
      }
      val portB = new Bundle {
        val en = in Bool
        val wr = in Bool
        val addr = in UInt (addressWidth bits)
        val wrData = in Bits (wordWidth bits)
        val rdData = out Bits (wordWidth bits)
      }
    }

    val mem = Mem(Bits(wordWidth bits),wordCount)
    io.portA.rdData := mem.readWriteSync(
      enable  = io.portA.en,
      address = io.portA.addr,
      write   = io.portA.wr,
      data    = io.portA.wrData
    )
    io.portB.rdData := mem.readWriteSync(
      enable  = io.portB.en,
      address = io.portB.addr,
      write   = io.portB.wr,
      data    = io.portB.wrData
    )
  }

  def main(args: Array[String]) {
    SpinalConfig()
     // .addStandardMemBlackboxing(blackboxAll)
      .generateVhdl(new TopLevel(wordWidth = 32,wordCount = 1024))
  }
}

object Play2DEbug{


//  class TopLevel extends Component{
//    val a = in Bits(4 bits)
//    val x = out Bits(4 bits)
//
//    x := a.resized
//
//    val b = in UFix(8 exp,4 bits)
//    val y = out UFix(8 exp,4 bits)
//
//    y := b.truncated
//
//  }

  object AluOpcode extends SpinalEnum {
    val ADDA,MUL,MAC,NOP = newElement
  }
  class TopLevel(Dwidth: Int, FracWidth:Int) extends Component {

    val io = new Bundle {
      val a = in SInt (Dwidth bits)
      val b = in SInt (Dwidth bits)
      val opcode = in(AluOpcode)
      val P = out SInt (Dwidth bits)
    }
    val a, b = Reg(SFix(6 exp, 18 bits)) init (0)
    val P = Reg(SFix(12 exp, -12 exp)) init (0) //accumulator register up to 2*Input width
    a.raw := io.a
    b.raw := io.b
    val opcode = Reg(AluOpcode) init (AluOpcode.NOP)
    opcode := io.opcode
    switch(opcode) {
      is(AluOpcode.ADDA) {
        P := a
        io.P := P.raw(Dwidth - 1 downto 0)
      }
      is(AluOpcode.MUL) {
        P := (a * b).truncated
        io.P := P.raw(Dwidth - 1 downto 0)
      }
      is(AluOpcode.MAC) {
        P := (P + a * b).truncated
        io.P := P.raw(Dwidth - 1 downto 0)
      }
      is(AluOpcode.NOP) {
        P := P
        io.P := P.raw(Dwidth - 1 downto 0)
      }
      default {
        P := P
        io.P := P.raw(Dwidth - 1 downto 0)
      }
    }
  }
  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel(18,10))
  }
}

object PlyBusSlaveFactory32{


  class TopLevel extends Component{
    val apb = slave(Apb3(addressWidth = 8,dataWidth = 16))
    val ctrl = Apb3SlaveFactory(apb)
    val r = Reg(UInt(20 bits))
    ctrl.writeMultiWord(r,4)
    ctrl.readMultiWord(r,4)

   val uartCtrlConfig = UartCtrlMemoryMappedConfig(
      uartCtrlConfig = UartCtrlGenerics(
        dataWidthMax      = 8,
        clockDividerWidth = 20,
        preSamplingSize   = 1,
        samplingSize      = 5,
        postSamplingSize  = 2
      ),
      txFifoDepth = 16,
      rxFifoDepth = 16
    )
  val uart = master(Uart())
  val interrupt = out Bool


  val uartCtrl = new UartCtrl(uartCtrlConfig.uartCtrlConfig)
  uart <> uartCtrl.io.uart

  val bridge = uartCtrl.driveFrom16(ctrl,uartCtrlConfig,0x80)
  interrupt := bridge.interruptCtrl.interrupt

  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}



object PlayMissingSensitivity{
  object State extends SpinalEnum{
    val a,b,c = newElement()
  }

  class TopLevel extends Component{
    val state = in(State)
    val output = out UInt(8 bits)
    output := 0
    switch(state){
      is(State.a){
        output := 1
      }
      is(State.b){
        output := 2
      }
      is(State.c){
        output := 3
      }
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}






object PlaySlowArea{
  class TopLevel extends Component{
    val areaStd = new Area {
      val counter = out(CounterFreeRun(16).value)
    }
    val areaDiv4 = new SlowArea(4){
      val counter = out(CounterFreeRun(16).value)
    }

    val area50Mhz = new SlowArea(50 MHz){
      val counter = out(CounterFreeRun(16).value)
    }
  }

  def main(args: Array[String]) {
    new SpinalConfig(
      defaultClockDomainFrequency = FixedFrequency(100 MHz)
    ).generateVhdl(new TopLevel)
  }
}



object PlayAutoconncetRec{

  class Inner extends Component {
    val a = in Bits(8 bits)
    val result = out Bits(8 bits)
    a <> result
  }
  class TopLevel extends Component{
    val a,b = in Bits(8 bits)
    val result = out Bits(8 bits)

    val x = new Inner

    x.a(7 downto 1) <> a(7 downto 1)
    x.a(0) <> True

    val c = Bool
    result := b & x.result
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}

object Debug425{
  class Wrapper() extends Component{

    val io = new Bundle{

      val bus_clk = in Bool
      val bus_rst = in Bool
    }

    val axiClockDomain = ClockDomain.external("axi")
    val busClockDonmain = ClockDomain(io.bus_clk, io.bus_rst)


    val axiReg = axiClockDomain{
      Counter(8).value.keep()
    }

    val busReg = busClockDonmain{
      Counter(8).value.keep()
    }
  }


  class TopLevel extends Component{

    val io = new Bundle{
      val user_clk = in Vec(Bool, 7)
      val user_rstn  = in Vec(Bool, 7)

    }

    val wrapper = new Wrapper()
    wrapper.io.bus_clk <> io.user_clk(4)
    wrapper.io.bus_rst <> io.user_rstn(4)


  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}




object PlayMuxDyn{

  class TopLevel extends Component{
    val inputs = in Vec(UInt(8 bits),8)
    val sel    = in UInt(log2Up(inputs.length) bits)
    val output = out(inputs(sel))
  }

  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new TopLevel)
  }
}

object PlayMuxDynX{

  class TopLevel extends Component{
    val inputs = in Vec(UInt(8 bits),8)
    val sel    = in UInt(log2Up(inputs.length) bits)
    val output = out UInt(8 bits)
    output := inputs(sel)
  }

  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new TopLevel)
  }
}

object PlayMuxDyn2{

  class TopLevel extends Component{
    val a,b,c,d = in UInt(8 bits)
    val sel     = in UInt(2 bits)
    val output  = out(Vec(a,b,c,d)(sel))
  }

  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new TopLevel)
  }
}
object PlayNullPointer{

  class TopLevel extends Component{
    val result = Bool
//    result := a & b
    val a,b = Bool
  }

  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new TopLevel)
  }
}

object PlayForLoop{


  def main(args: Array[String]) {
    val (x,xSquare) = (for(i <- 0 to 4) yield{
      val iSquare = i*i
      (i,iSquare)
    }).unzip

    println(s"x=$x")
    println(s"xSquare=$xSquare")
  }
}

object PlayCrossHearchy{

//  class TopLevel extends Component{
//    val a,b = in Bool
//    val result = out Bool()
//    result := a & b
//
//    a := True
//  }
  class ComponentX extends Component{

    val X = Bool

  }

  class ComponentY extends Component{

    val componentX = new ComponentX
    val Y = Bool
    componentX.X := Y //This assignment is not legal

  }
  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new ComponentY)
  }
}


object PlayPll{
  class PLL extends BlackBox{
    val io = new Bundle{
      val clk_in = in Bool
      val clk_out = out Vec(Bool,3)
    }

    noIoPrefix()
  }

  class TopLevel extends Component{
    val io = new Bundle {
      val aReset     = in Bool
      val clk_100Mhz = in Bool
    }
    val pllBB = new PLL
    pllBB.io.clk_in := io.clk_100Mhz
    val clockDomains = pllBB.io.clk_out.map(c => ClockDomain(c,io.aReset))

    val pllClk0Area = new ClockingArea(clockDomains(0)){
      val counter = CounterFreeRun(4)
    }

    val pllClk1Area = new ClockingArea(clockDomains(1)){
      val counter = CounterFreeRun(8)
    }

    val pllClk2Area = new ClockingArea(clockDomains(2)){
      val counter = CounterFreeRun(16)
    }
  }
  
  def main(args: Array[String]) {
    SpinalVhdl({
      val toplevel = new TopLevel
      toplevel.pllClk0Area.counter.value.keep()
      toplevel.pllClk1Area.counter.value.keep()
      toplevel.pllClk2Area.counter.value.keep()
      toplevel
    })
  }
}


object PlayPll2{
  class PLL extends BlackBox{
    val io = new Bundle{
      val clkIn    = in Bool
      val clkOut   = out Bool
      val isLocked = out Bool
    }

    noIoPrefix()
  }

  class TopLevel extends Component{
    val io = new Bundle {
      val aReset    = in Bool
      val clk100Mhz = in Bool
      val result    = out UInt(4 bits)
    }

    // Create an Area to manage all clocks and reset things
    val clkCtrl = new Area {
      //Instanciate and drive the PLL
      val pll = new PLL
      pll.io.clkIn := io.clk100Mhz

      //Create a new clock domain named 'core'
      val coreClockDomain = ClockDomain.internal("core")

      //Drive clock and reset signals of the coreClockDomain previously created
      coreClockDomain.clock := pll.io.clkOut
      coreClockDomain.reset := ResetCtrl.asyncAssertSyncDeassert(
        input = io.aReset || ! pll.io.isLocked,
        clockDomain = coreClockDomain
      )
    }

    //Create an ClockingArea which will be under the effect of the clkCtrl.coreClockDomain
    val core = new ClockingArea(clkCtrl.coreClockDomain){
      //Do your stuff which use coreClockDomain here
      val counter = Reg(UInt(4 bits)) init(0)
      counter := counter + 1
      io.result := counter
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl({
      val toplevel = new TopLevel
      toplevel
    })
  }
}


object PlaRamMux{

  case class J1Config(adrWidth : Int, wordSize : Int)
  class TopLevel(cfg : J1Config) extends Component {

    // Check the generic parameters
    assert(Bool(cfg.wordSize >= cfg.adrWidth), "Error: The width of an address is too large", FAILURE)

    // I/O ports
    val io = new Bundle {

      // Instruction port
      val memInstrAdr = in UInt(cfg.adrWidth bits)
      val memInstr    = out Bits(cfg.wordSize bits)

      // Memory port
      val memWriteEnable = in Bool
      val memAdr         = in UInt(cfg.wordSize bits)
      val memWrite       = in Bits(cfg.wordSize bits)
      val memRead        = out Bits(cfg.wordSize bits)

    }.setName("")

    // Generate a list holding the lowest memory block (holding the instructions to be executed)
    val lowMem = Mem(Bits(cfg.wordSize bits),1 << 16)

    // Create the instruction port (read only) for the instruction memory
    io.memInstr := lowMem.readSync(address = io.memInstrAdr, readUnderWrite = readFirst)

    // Calculate the number of needed rams
    def noOfRAMs = (1 << (cfg.wordSize - cfg.adrWidth))

    // Holds a complete list of memory blocks (start with first block)
    val ramList = if (noOfRAMs >= 1) {

      // Add the additional memory blocks into a list
      List(lowMem) ++ List.fill(noOfRAMs - 1)(Mem(Bits(cfg.wordSize bits), 1 << cfg.adrWidth))

    } else {

      // We have only one memory block
      List(lowMem)

    }

    // Convert the list to a spinal vector
    val rPortsVec = Vec(for((ram,i) <- ramList.zipWithIndex) yield {

      // Create the write port of the ith RAM
      ram.write(enable  = io.memWriteEnable && (U(i) === io.memAdr(cfg.wordSize - 1 downto cfg.adrWidth)),
                address = io.memAdr(cfg.adrWidth - 1 downto 0),
                data    = io.memWrite)

      // Create the read port of the ith RAM
      ram.readSync(address   = io.memAdr(cfg.adrWidth - 1 downto 0),
                   readUnderWrite = readFirst)
    })

    // Multiplex the output
    io.memRead := rPortsVec(RegNext(io.memAdr(cfg.wordSize - 1 downto cfg.adrWidth)))


  }
  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new TopLevel(J1Config(16,16)))
  }
}


object PlayAlu{

  class TopLevel extends Component{
    val a,b = in Bits(32 bits)
    val doSub = in Bool
    val doUnsignedLess = in Bool
    val addSub = out((a.asSInt + Mux(doSub, ~b, b).asSInt + Mux(doSub,S(1),S(0))).asBits)


    // SLT, SLTU
    val less  = out(Mux(a.msb === b.msb, addSub.msb,
      Mux(doUnsignedLess, b.msb, a.msb)))


  }
  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new TopLevel)
  }
}

object PlayDontCareEnum{
  object State extends SpinalEnum{
    val A,B,C = newElement()
  }

  class TopLevel extends Component{
    val x = out(State()).assignDontCare()
    val y = out(State(binarySequential)).assignDontCare()
    val z = out(State(binaryOneHot)).assignDontCare()
    z := State.A
    addPrePopTask(() => z := State.B)
  }
  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new TopLevel)
    SpinalConfig().generateVerilog(new TopLevel)
  }
}
