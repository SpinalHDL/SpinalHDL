package spinal.tester.code

/**
  * Created by PIC32F_USER on 20/09/2015.
  */

import java.io.InputStream
import java.util.concurrent.CyclicBarrier

import _root_.com.sun.xml.internal.messaging.saaj.util.{ByteOutputStream, ByteInputStream}
import spinal.core._
import spinal.demo.mandelbrot.{MandelbrotSblDemo, MandelbrotCoreParameters}
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3SlaveController, Apb3Config, Apb3Slave}
import spinal.lib.bus.sbl.{SblConfig, SblReadRet, SblReadCmd, SblWriteCmd}
import spinal.lib.com.uart._
import spinal.lib.graphic.Rgb
import spinal.lib.graphic.vga.Vga


trait BundleA extends Bundle {
  val a = Bool
}

trait BundleB extends Bundle {
  val b = Bool
}

trait BundleC extends Bundle {
  val c = Bool
}

trait BundleD extends Bundle {
  val d = Bool
}

class Stage0 extends Bundle with BundleA with BundleB with BundleC

class Stage1 extends Bundle with BundleA with BundleB with BundleD

class Play1 extends Component {
  val io = new Bundle {
    val input = slave Stream (new Stage0)
    val output = master Stream (new Stage1)
  }


  io.input.translateInto(io.output)((to, from) => {
    to.assignSomeByName(from)
    to.d := False
  })
}

object Play1 {
  def main(args: Array[String]): Unit = {
    SpinalVhdl(new Play1)
  }
}


class ComplexBundle extends Bundle {
  val a = Bits(12 bit)
  val b = UInt(50 bit)
  val c = Bool
  val d = Vec(Bits(8 bit), 3)
}


class Play2 extends Component {
  val busConfig = new Apb3Config(16, 32)
  val bus = slave(new Apb3Slave(busConfig))
  val controller = new Apb3SlaveController(bus)

  val myReadSignal = in(new ComplexBundle);
  controller.read(myReadSignal, 0x10)
  //  val myWriteOnlyReg = out(controller.writeOnlyReg(new ComplexBundle,0x20))
  //  val myWriteReadReg = out(controller.writeReadReg(new ComplexBundle,0x30))
  val myPushStreamBits = master(controller.writeStream(0x40))
  val myPushStreamComplex = master(controller.writeStreamOf(new ComplexBundle, 0x50))
  val myPopStreamComplex = slave(Stream(new ComplexBundle));
  controller.readStream(myPopStreamComplex, 0x60)

}

object Play2 {
  def main(args: Array[String]): Unit = {
    SpinalVhdl(new Play2)
  }
}


class Play3 extends Component {


  //  val c = out(a & b)
  //  lazy val a = in Bool
  //  lazy val b = in Bool
  //  val areaC = new Area {
  //    lazy val c  : Bool = areaAB.a & areaAB.b
  //    lazy val d : Bool = True
  //  }
  //  val areaAB = new Area {
  //    lazy val a : Bool = in Bool
  //    lazy val b : Bool = in Bool
  //    lazy val c : Bool  =  areaC.d
  //  }
  //
  //  out(areaC.c)


  //  val arrayType = Vec(Bool,7)
  //  //val arrayIn = in Vec(Vec(Bool,5),10)
  //  //val arrayIn = in Vec(arrayType,10)
  //  val arrayIn = in Vec(UInt(3 bit), UInt(5 bit),UInt(7 bit))
  //  val arrayOut = out cloneOf(arrayIn)
  //  arrayOut := arrayIn
  //
  //  val uList = List(U(4),U(5))
  //  val count = out (SetCount(B"b1100"))
  //  val count2 = out(uList.sContains(U(0)))
  //
  //
  //  val normalVec = Vec(UInt(4 bit),10)
  //
  //  val containZero = normalVec.sContains(0)
  //  val existOne = normalVec.sExists(_ === 1)
  //  val (firstTwoValid,firstTwoIndex) = normalVec.sFindFirst(_ === 2)
  //
  //
  //  in(normalVec)
  //  out(firstTwoValid)
  //  out(firstTwoIndex)
}

object Play3 {
  def main(args: Array[String]): Unit = {
    SpinalVhdl(new Play3)
  }
}


object play {

  import scala.tools.nsc.interpreter.IMain
  import scala.tools.nsc.Settings

  private def genClass[T](): T = {
    val settings = new Settings()
    settings.embeddedDefaults(this.getClass.getClassLoader())
    val interpreter = new IMain(settings)

    interpreter.compileString("class A{" +
      "val a = 2" +
      "}")
    val clazz = interpreter.classLoader.loadClass("A")
    clazz.newInstance().asInstanceOf[T]
  }

  def main(args: Array[String]) {
    val a = genClass()
    print(a)
  }
}


abstract class SemiGroup[A] {
  def add(x: A, y: A): A
}

abstract class Monoid[A] extends SemiGroup[A] {
  def unit: A
}

object ImplicitTest extends App {

  implicit object StringMonoid extends Monoid[String] {
    def add(x: String, y: String): String = x concat y

    def unit: String = ""
  }

  implicit object IntMonoid extends Monoid[Int] {
    def add(x: Int, y: Int): Int = x + y

    def unit: Int = 0
  }

  def sum[A](xs: List[A])(implicit m: Monoid[A]): A =
    if (xs.isEmpty) m.unit
    else m.add(xs.head, sum(xs.tail))

  println(sum(List(1, 2, 3)))
  println(sum(List("a", "b", "c")))


}

class Titi[A <: Int]() {
  val s: Symbol = 'x

  val v = Vec(True, True, True)
  val b = Bool
  b := v.reduceBalancedSpinal(_ | _)


}


object Yolo {

  class C2 {
    var ref: () => Unit = null

    def doit = ref()
  }

  class C1(arg: Int) {
    def dothat(): Unit = {
      print("dothat" + arg)
    }
  }

  def main(args: Array[String]) {
    val c2 = new C2
    val c1 = new C1(2)

    c2.ref = c1.dothat
    c2.doit
  }
}

object Yolo2 {

  class PassedImplicits {
    val a = "a"
    val b = "b"
  }

  def execute[T](blockOfCode: PassedImplicits => T): Unit = {
    blockOfCode(new PassedImplicits())
  }


  def f1(str1: String)(implicit str2: String): Unit = {
    println(str1 + str2)
  }


  def main(args: Array[String]) {
    implicit val str2 = "asd"
    f1("a ")

    execute { impl =>
      import impl._
      println(a + b)
    }

    val list = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9)
    def sum(list: Seq[Int]): Int = {
      list.size match {
        case 0 => return 0
        case 1 => return list.head
        case _ => {
          val (a, b) = list.splitAt(list.size / 2)
          println(a.mkString(",") + " + " + b.mkString(","))
          return sum(a) + sum(b)
        }
      }
    }
    println(sum(list))
  }
}

class BundleBase {

}

class Play5(p: Int) extends Component {
  var cnt = 0
  val stream = new Bundle {
    outer =>
    val a = UInt(p bit)
    //def b = print(Play5.this)
    val c = new BundleBase with Cloneable {
      val e = UInt()
      val f = cnt
      cnt += 1

      // def b = print(Play5.this)
      override def clone(): AnyRef = super.clone()

      def clone2(): this.type = clone().asInstanceOf[this.type]
    }
  }


  //val stream = new Bundle{ val a = UInt()}

  println(stream.c.clone2().f)
  println(stream.c.clone2().f)
  println(stream.c.clone2().f)
  println(stream.c.clone2().f)
  println(cnt)


  def toto: Bundle = {
    val x = "yolo"
    val y = new Bundle {
      val a = Bits()

      def pp = x + Math.sin(2.0)
    }
    return y
  }

  val xx = toto

  xx.clone()

}

object Play5 {
  def main(args: Array[String]): Unit = {
    def a() = 2
    def b = 3

    SpinalVhdl(new Play5(5))
  }
}


object Play6 {


  import spinal._

  class Comp extends Component {
    val io = new Bundle() {
      val cond = in Bool
      val input = in UInt (4 bit)
      val output = out Bool
    }

    var carry = Bool(false)
    for (bit <- io.input.toBools) {
      when(io.cond) {
        carry \= carry & bit
      }
    }
    io.output := carry

  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new Comp)
  }
}


object Play7 {

  def grayCounter(n: Int, enable: Bool): UInt = {
    val gray = RegInit(U(0, n bit))
    var even = RegInit(True)
    val word = Cat(True, gray(n - 3, 0), even)
    when(enable) {
      var found = False
      for (i <- 0 until n) {
        when(word(i) && !found) {
          gray(i) := !gray(i)
          found \= True
        }
      }
      even := !even
    }
    return gray
  }


  class GrayCounter(n: Int) extends Component {
    val enable = in Bool
    val gray = out UInt (n bit)

    gray := grayCounter(n, enable)
    /*
        val grayReg = Reg(UInt(n bit)) init(0)
        var even = RegInit(True)
        val word = Cat(True,grayReg(n-3,0).toBools,even)
        var found = False
        when(enable){
          for(i <- 0 until n){
            when(word(i) && !found){
              grayReg(i) := ! grayReg(i)
              found \= True
            }
          }
          even := !even
        }

        gray := grayReg*/
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new GrayCounter(4))
  }
}


object PlayFix {


  class TopLevel extends Component {
    val ufix = UFix(8 exp, 12 bit)
    val uint = UInt(3 bit)
    ufix := toUFix(uint)
    val uintBack = toUInt(ufix)

    val sfix = SFix(7 exp, 12 bit)
    val sint = SInt(3 bit)
    sfix := toSFix(sint)
    val sintBack = toSInt(sfix)


    in(uint)
    out(ufix)
    out(uintBack)

    in(sint)
    out(sfix)
    out(sintBack)
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}


object PlayMux {

  //  class TopLevel extends Component {
  //    val sel = in UInt(3 bit)
  //    val input = in Vec (UInt(8 bit),8)
  //    val output = out UInt(8 bit)
  //
  //    output := input(sel)
  //  }

  class TopLevel extends Component {
    val sel = in UInt (3 bit)
    val input = in Vec(UInt(8 bit), 8)
    val output = out UInt (8 bit)

    output := input(0)
    for (i <- output.range) {
      if (i != 0) {
        when(sel === i) {
          output := input(i)
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PlayApb {


  class TopLevel extends Component {
    val apbConfig = new Apb3Config(16,32)
    val bus = slave(new Apb3Slave(apbConfig))
    val busCtrl = new Apb3SlaveController(bus) //This is a APB3 slave controller builder tool

    val outputs = Vec(i => out(busCtrl.writeReadReg(UInt(32 bit),i*4)),8)

    val b = out(new Bundle{
      val a,b,c,d = Bool
      val e = Bool
    })
    b.assignFromBits(B"b000000000")

  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PlayDefault {

  class SubLevel extends Component {
    val input = in(Bool) default (False)
    val output = out(Bool)
    val internal = Bool default (True)
    output := input && internal
  }

  class TopLevel extends Component {
    val sub = new SubLevel

    val output = out(Bool)
    output := sub.output
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PlayFsm {

  class FSM {
    def entry(state: State): Unit = {

    }
  }

  class State {
    def onEntry = {}

    def onRun = {}

    def onExit = {}

    //    val onEntry = False
    //    val onRun = False
    //    val onExit = False
  }

  class TopLevel extends Component {
    val fsm = new FSM {
      val stateA = new State {
        override def onRun = {
          entry(stateB)
        }
      }

      val stateB = new State {

      }
    }
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PlayFsm2 {

  class FSM {
    def entry(state: State): Unit = {

    }
  }

  class State {
    val onEntry = False
    val onRun = False
    val onExit = False
  }

  class TopLevel extends Component {
    val fsm = new FSM {
      val stateA, stateB = new State
      //when(stateA.)

    }
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PlayFsmRef {

  class TopLevel extends Component {
    val input = master Stream (UInt(8 bit))
    val output0 = master Stream (UInt(8 bit))
    val output1 = master Stream (UInt(8 bit))

    object State extends SpinalEnum {
      val s0, s1 = ordered()
    }

    val fsm = new Area {

      import State._

      val stateNext = State()
      val state = RegNext(stateNext) init (s0)

      output0.valid := False
      output1.valid := False
      stateNext := state
      switch(state) {
        is(s0) {
          output0.valid := input.valid
          output0.data := input.data
          input.ready := output0.ready
          when(input.valid && output0.ready) {
            stateNext := s1
          }
        }
        default {
          //is(s1){
          output1.valid := input.valid
          output1.data := input.data
          input.ready := output1.ready
          when(input.valid && output1.ready) {
            stateNext := s0
          }
        }
      }

    }
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}


object ApbUartPlay {

  class ApbUartCtrl(apbConfig: Apb3Config) extends Component {
    val io = new Bundle {
      val bus = slave(new Apb3Slave(apbConfig))
      val uart = master(Uart())
    }
    val busCtrl = new Apb3SlaveController(io.bus) //This is a APB3 slave controller builder tool

    val config = busCtrl.writeOnlyRegOf(UartCtrlConfig(), 0x00)
    //Create a write only configuration register at address 0x00
    val clockDivider = busCtrl.writeOnlyRegOf(UInt(20 bit), 0x10)
    val writeStream = busCtrl.writeStreamOf(Bits(8 bit), 0x20)
    val readStream = busCtrl.readStreamOf(Bits(8 bit), 0x30)

    val uartCtrl = new UartCtrl(8, 20)
    uartCtrl.io.config := config
    uartCtrl.io.clockDivider := clockDivider
    uartCtrl.io.write <-< writeStream //Pipelined connection
    uartCtrl.io.read.toStream.queue(16) >> readStream //Queued connection
    uartCtrl.io.uart <> io.uart
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new ApbUartCtrl(new Apb3Config(16, 32)))
  }
}

object OverloadPlay {

  class OverloadPlay(frameAddressOffset: Int, p: MandelbrotCoreParameters, coreClk: ClockDomain, vgaMemoryClk: ClockDomain, vgaClk: ClockDomain) extends Component {
    for (i <- 0 until 10) {
      val memoryBusConfig = SblConfig(30, 32)
      val rgbType = Rgb(8, 8, 8)

      val i = new MandelbrotSblDemo(frameAddressOffset, p, coreClk, vgaMemoryClk, vgaClk)
      val uart = master(Uart())

      val mandelbrotWriteCmd = master Stream SblWriteCmd(memoryBusConfig)

      val vgaReadCmd = master Stream SblReadCmd(memoryBusConfig)
      val vgaReadRet = slave Flow SblReadRet(memoryBusConfig)

      val vga = master(Vga(rgbType))

      i.io.uart <> uart
      i.io.mandelbrotWriteCmd <> mandelbrotWriteCmd
      i.io.vgaReadCmd <> vgaReadCmd
      i.io.vgaReadRet <> vgaReadRet
      i.io.uart <> uart
      i.io.vga <> vga

    }
  }

  def main(args: Array[String]): Unit = {
    //Console.in.read

    for (i <- 0 until 1) {
      val report = SpinalVhdl({
        val vgaClock = ClockDomain("vga")
        val vgaMemoryClock = ClockDomain("vgaMemory")
        val coreClock = ClockDomain("core", FixedFrequency(100e6))
        new OverloadPlay(0, new MandelbrotCoreParameters(256, 64, 640, 480, 7, 17 * 3), coreClock, vgaMemoryClock, vgaClock)
      })

      /*while(true){
        Thread.sleep(1000)
        println(report.topLevel )
      }*/
    }
  }
}


object MessagingPlay {

  class TopLevel extends Component {
    val o = out(Bool)
    when(True) {
      o := True
    }
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)

  }
}


object vhd_dirext_play {
  def main(args: Array[String]) {
    import scala.sys.process._
    import java.io._


    val writer = new PrintWriter(new File("in.txt"))
    for (i <- 0 until 1000000) {
      writer.write(i + "\n")
    }
    writer.flush()
    writer.close()
    println("start")

    //    (s"ghdl -a --ieee=synopsys vhdl_direct.vhd" !)
    //    (s"ghdl -e --ieee=synopsys vhdl_direct" !)
    //    (s"ghdl -r --ieee=synopsys vhdl_direct" !)

    (s"vlib vhdl_direct" !)
    (s"vcom vhdl_direct.vhd" !)
    ("vsim -c -do \"run 1 ms\" work.vhdl_direct" !)

    print("DONE")
  }

}


object vhd_stdio_play {
  def main(args: Array[String]) {
    import scala.sys.process._
    import java.io.File
    // ("ghdl" #> new File("test.txt") !)
    val in = new ByteOutputStream()
    val out = new ByteInputStream()
    val err = new ByteInputStream()
    //scala.concurrent.SyncVar[java.io.OutputStream];
    val stopAt = 1000 * 1000

    val array = new Array[Byte](1000)
    val barrier = new CyclicBarrier(2)
    //    val io = new ProcessIO(in, out, err)
    //    //  cmd.write("asd")
    (s"ghdl -a --ieee=synopsys vhdl_file.vhd" !)
    (s"ghdl -e --ieee=synopsys vhdl_file" !)
    val process = Process("ghdl -r --ieee=synopsys vhdl_file")
    val io = new ProcessIO(
      in => {
        for (i <- 0 until stopAt) {
          // while(cnt != i){}
          //println("a")
          in.write(i + "\n" getBytes "UTF-8")
          in.flush()
          barrier.await()
          //Thread.sleep(500)

        }
        in.close()
        println("finish")
      }


      ,
      out => {
        var cnt = 0
        var bufferIndex = 0
        var lastTime = System.nanoTime()
        while (cnt != stopAt) {
          if (out.available() != 0) {
            bufferIndex += out.read(array, bufferIndex, out.available())
            if (array.slice(0, bufferIndex).contains('\n')) {
              bufferIndex = 0

              val i = new String(array, "UTF-8").substring(0, array.indexOf('\r')).toInt
              assert(i == cnt)
              barrier.await()
              cnt += 1
              if (i % 10000 == 0) {
                println(10000.0 / (System.nanoTime() - lastTime) / 1e-9)
                lastTime = System.nanoTime()
              }
            }
          }
        }
        out.close()
        //scala.io.Source.fromInputStream(out).getLines.foreach(println)
      },
      err => {
        scala.io.Source.fromInputStream(err).getLines.foreach(println)
      })
    process.run(io)
    //    val p = Process("ghdl -r --ieee=synopsys vhdl_file")
    //    p.run(io)
    //    p.run()
    //    // (s"ghdl -r --ieee=synopsys vhdl_file" #> cmd !)
    print("DONE")
  }

}


object vhd_stdio_play2 {
  def main(args: Array[String]) {
    import scala.sys.process._
    import java.io.File
    // ("ghdl" #> new File("test.txt") !)
    var in: java.io.OutputStream = null
    var out: java.io.InputStream = null
    var err: java.io.InputStream = null
    //scala.concurrent.SyncVar[java.io.OutputStream];

    val array = new Array[Byte](1000)
    val barrier = new CyclicBarrier(4)
    //    val io = new ProcessIO(in, out, err)
    //    //  cmd.write("asd")
    (s"ghdl -a --ieee=synopsys vhdl_file.vhd" !)
    (s"ghdl -e --ieee=synopsys vhdl_file" !)
    val process = Process("ghdl -r --ieee=synopsys vhdl_file")
    //
    //    (s"vlib work" !)
    //    (s"vcom -check_synthesis vhdl_file.vhd" !)
    //    val process = Process("vsim -c work.vhdl_file")
    val io = new ProcessIO(
      inX => {
        in = inX
        barrier.await()
        barrier.await()
        inX.close()
        println("finish")
      }
      ,
      outX => {
        out = outX
        barrier.await()
        barrier.await()
        outX.close()
        println("finish")
      },
      errX => {
        err = errX
        barrier.await()
        barrier.await()
        errX.close()
        println("finish")
      })
    process.run(io)
    barrier.await()
    var cnt = 0
    var bufferIndex = 0
    var lastTime = System.nanoTime()
    for (i <- 0 until 100 * 1000) {
      in.write(i + "\n" getBytes "UTF-8")
      in.flush()
      var done = false
      while (!done) {
        if (out.available() != 0) {
          bufferIndex += out.read(array, bufferIndex, out.available())
          if (array.slice(0, bufferIndex).contains('\n')) {
            bufferIndex = 0

            val i = new String(array, "UTF-8").substring(0, array.indexOf('\r')).toInt
            assert(i == cnt)
            cnt += 1
            if (i % 10000 == 0) {
              println(10000.0 / (System.nanoTime() - lastTime) / 1e-9)
              lastTime = System.nanoTime()
            }
            done = true
          }
        }
      }
    }


    barrier.await()

    //    val p = Process("ghdl -r --ieee=synopsys vhdl_file")
    //    p.run(io)
    //    p.run()
    //    // (s"ghdl -r --ieee=synopsys vhdl_file" #> cmd !)
    print("DONE")
  }

}


object vhd_stdio_play3 {
  def main(args: Array[String]) {
    import scala.sys.process._
    import java.io.File
    // ("ghdl" #> new File("test.txt") !)
    var in: java.io.OutputStream = null
    var out: java.io.InputStream = null
    var err: java.io.InputStream = null
    //scala.concurrent.SyncVar[java.io.OutputStream];

    val array = new Array[Byte](100000)
    val barrier = new CyclicBarrier(4)
    //    val io = new ProcessIO(in, out, err)
    //    //  cmd.write("asd")
    //    (s"ghdl -a --ieee=synopsys vhdl_file.vhd" !)
    //    (s"ghdl -e --ieee=synopsys vhdl_file" !)
    //    val process = Process("ghdl -r --ieee=synopsys vhdl_file")
    //
    (s"vlib work" !)
    (s"vcom vhdl_file.vhd" !)
    val process = Process("vsim -c work.vhdl_file")
    val io = new ProcessIO(
      inX => {
        in = inX
        barrier.await()
        barrier.await()
        inX.close()
        println("finish")
      }
      ,
      outX => {
        out = outX
        barrier.await()
        barrier.await()
        outX.close()
        println("finish")
      },
      errX => {
        err = errX
        barrier.await()
        barrier.await()
        errX.close()
        println("finish")
      })
    process.run(io)
    barrier.await()
    var cnt = 0
    var bufferIndex = 0
    var lastTime = System.nanoTime()
    Thread.sleep(2000)
    in.write("run 1 ms\n" getBytes "UTF-8")
    in.flush()
    Thread.sleep(2000)
    for (i <- 0 until 100 * 1000) {
      in.write(i + "\n" getBytes "UTF-8")
      in.flush()
      var done = false
      //  while (!done) {
      // if (out.available() != 0) {
      bufferIndex += out.read(array, bufferIndex, out.available())
      if (array.slice(0, bufferIndex).contains('\n')) {
        bufferIndex = 0
        print(new String(array, "UTF-8"))
        // val i = new String(array, "UTF-8").substring(0, array.indexOf('\r')).toInt
        //  assert(i == cnt)
        cnt += 1
        if (cnt % 10000 == 0) {
          println(10000.0 / (System.nanoTime() - lastTime) / 1e-9)
          lastTime = System.nanoTime()
        }
        done = true
      }
      //     }
      // }
    }


    barrier.await()

    //    val p = Process("ghdl -r --ieee=synopsys vhdl_file")
    //    p.run(io)
    //    p.run()
    //    // (s"ghdl -r --ieee=synopsys vhdl_file" #> cmd !)
    print("DONE")
  }
}
