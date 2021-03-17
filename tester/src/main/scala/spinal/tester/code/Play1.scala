package spinal.tester.code


import java.io.InputStream
import java.util.concurrent.CyclicBarrier

import spinal.core._
import spinal.core.fiber._
import spinal.demo.mandelbrot.{MandelbrotCoreParameters, MandelbrotSblDemo}
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3Gpio}
import spinal.lib.bus.amba4.axi.{Axi4, Axi4SpecRenamer}
import spinal.lib.bus.amba4.axilite.AxiLite4.prot
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.avalon.AvalonMM
import spinal.lib.eda.bench.{AlteraStdTargets, Bench, Rtl, XilinxStdTargets}
import spinal.lib.experimental.bus.sbl.{SblConfig, SblReadCmd, SblReadRet, SblWriteCmd}
import spinal.lib.com.uart._
import spinal.lib.cpu.riscv.impl.build.RiscvAvalon
import spinal.lib.cpu.riscv.impl._
import spinal.lib.cpu.riscv.impl.extension.{BarrelShifterFullExtension, DebugExtension, DivExtension, MulExtension}
import spinal.lib.experimental.MacrosClass
import spinal.lib.graphic.{Rgb, RgbConfig}
import spinal.lib.graphic.vga.{Vga, VgaCtrl}
import spinal.lib.io.TriStateArray

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


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



object Play1 {
  case class Sub() extends Component {
    val io = inout(Analog(Bool))

    io := True
  }

  case class TopLevel() extends Component {
    val io = inout(Analog(Bits(2 bits)))

    val s0, s1 = Sub()

    io(0) := s0.io
    io(1) := s1.io
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}


class ComplexBundle extends Bundle {
  val a = Bits(12 bit)
  val b = UInt(50 bit)
  val c = Bool
  val d = Vec(Bits(8 bit), 3)

//  override def clone() : this.type = new ComplexBundle
}




object play {


//  trait HandleCoreSubscriber[+T]{
//    def changeCore[T2 >: T](core : HandleCore[T2]) : Unit
//    def lazyDefault (): T
//    def lazyDefaultAvailable : Boolean
//  }
//
//  class HandleCore[+T]{
//    private var loaded = false
//    private var value : Any = null.asInstanceOf[T]
//
//    val subscribers = mutable.HashSet[HandleCoreSubscriber[Any]]()
//
//    def get : T = {
//      if(!loaded){
//        subscribers.count(_.lazyDefaultAvailable) match {
//          case 0 =>
//          case 1 => load(subscribers.find(_.lazyDefaultAvailable).get.lazyDefault())
//          case _ => SpinalError("Multiple handle default values")
//        }
//      }
//      value.asInstanceOf[T]
//    }
//    def load(value : Any): T = {
//      this.value = value
//      loaded = true
//      value.asInstanceOf[T]
//    }
//
//    def merge(that : HandleCore[Any]): Unit ={
//      (this.loaded, that.loaded) match {
//        case (false, _) => this.subscribers.foreach(_.changeCore(that))
//        case (true, false) => that.subscribers.foreach(_.changeCore(this))
//        case _ => ???
//      }
//    }
//
//    def isLoaded = loaded || subscribers.exists(_.lazyDefaultAvailable)
//  }

//  class Handle[+T] extends HandleCoreSubscriber[T]{
//
//
//    override def changeCore[T2 >: T](core: HandleCore[T2]): Unit = ???
//
//
//    override def lazyDefault(): T = ???
//
//    override def lazyDefaultAvailable: Boolean = ???
//  }


//  class Handle[-T] {
////    var value : T = null.asInstanceOf[T]
//
//    def load[T2 <: T](x : Handle[T2]) : T2 = {
//      null.asInstanceOf[T2]
//    }
//
//
//    def load2(x : T) : Unit = {
//    }
//
////    def get: T = ???
//
//  }
//
////  val x = new Handle[String]
////  def miaou[T2 <: A](a : Handle[T2]) = {
////
////  }
////
////  miaou(new Handle[String])
//
//
//
//  class A
//  class B extends A
//  val a = new Handle[A]
//  val b = new Handle[B]
////  miaou[A](b)
//  a.load(b)
//  a.load(a)
////  a.load(c)



//
//  class Handle[T] {
//    //    var value : T = null.asInstanceOf[T]
//
//    def load[T2 <: T](x : Handle[T2]) : T2 = {
//      null.asInstanceOf[T2]
//    }
//
//
//    def load2(x : T) : Unit = {
//    }
//
//    //    def get: T = ???
//
//  }



  class Handle[T] {
    //    var value : T = null.asInstanceOf[T]



    def load[T2 <: T](x : Handle[T2]) : T2 = {
      null.asInstanceOf[T2]
    }


    def load2(x : T) : Unit = {
    }

    //    def get: T = ???

  }


  //  val x = new Handle[String]

  //
  //  miaou(new Handle[String])



  def main(args: Array[String]) {
    class A  extends Nameable
    class B extends A
    def miaou[T2 <: A](a : Handle[T2]) = {

    }
    val a = new Handle[A]
    val b = new Handle[B]
      miaou(b)
    a.load(b)
    a.load(a)
    //  a.load(c)

    implicit def trtr[T, T2 <: T](h : Handle[T2]) : Handle[T] = h.asInstanceOf[ Handle[T]]

    def miaou2() : Handle[A] = a
    def miaou3() : Handle[A] = b
  miaou3

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
  SFix(4 exp, -4 exp) := 16.2
}

object Play5 {
  def main(args: Array[String]): Unit = {


    SpinalVhdl(new Play5(5))
  }


}

object PlayFifo {
  class TopLevel extends Component{
    val fifo = new StreamFifo(Bool,8)
    fifo.io.clone.clone
  }
  def main(args: Array[String]): Unit = {

    SpinalVhdl(new TopLevel())
  }
}


object PlayUtilities{
  class TopLevel() extends Component{
    val x,y = in SInt(4 bits)
    val result = out(x.resize(widthOf(x) + 1) + y.resize(widthOf(y) + 1))

  }
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new TopLevel())
  }
}

object PlayCheckBundles {
  class TopLevel extends Component{
    case class A() extends Bundle{
      val a = Bool
    }

    case class B() extends Bundle{
      val b = Bool
    }


    val sA = Stream(new A)
    val sB = Stream(new A)
    sA.payload := sB
  }
  def main(args: Array[String]): Unit = {

    SpinalVhdl(new TopLevel())
  }
}


object PlayBetterError {


  class TopLevel extends Component{
    val cond = in Bool
    val a,b = in UInt(4 bits)
    val c = in UInt(5 bits)
    val d = in UInt(6 bits)
    val e = in UInt(7 bits)
    val f = in UInt(8 bits)
    val result = out UInt(4 bits)
    val reg = Reg(UInt(4 bits))
    //Case 1
//    result := c

    //Case 2
//    result := c
//    result(1 downto 0) := b.resize(4)

    //Case 3
//    when(cond){
//      result := c
//    }elsewhen(cond){
//      when(cond){
//        result := a
//      }otherwise{
//        result := e
//      }
//    }otherwise{
//      result := f
//    }

    //Case 4
//    val sub = new  Component{
//      val input = in UInt(4 bits)
//      val output = out UInt(4 bits)
//      val toto = U(0,4 bits)
//      input := toto
//      output := input
//    }
//    result := sub.output

    //Case 5
//    val sub = new Component{
//      val input = in UInt(4 bits)
//      val output = out UInt(4 bits)
//    }
//    sub.input := a
//    sub.output := a
//    result := sub.output

    //Case 6
//    val sub = new Component{
//      val input = in UInt(4 bits)
//      val output = out UInt(4 bits)
//      val tmp = UInt(4 bits)
//      output := tmp
//    }
//    sub.input := a
//    sub.tmp := 3
//    result := sub.output


    //Case 7
//      val sub = new Component{
//        val input = in UInt(4 bits)
//        val output = out UInt(4 bits)
//        val tmp = UInt(4 bits)
//        output := tmp
//      }
//      sub.input := a
//      when(cond){
//        sub.tmp := b
//      }otherwise{
//        sub.tmp := 4
//      }
//      result := sub.output
//

      //Case 8
//      val sub = new Component{
//        val input = in UInt(4 bits)
//        val output = out UInt(4 bits)
//        val tmp = UInt(4 bits)
//        tmp := 3
//        output := tmp
//      }
//      sub.input := a
//      when(cond){
//        sub.tmp := 3
//      }
//      result := sub.output


    //Case 9
//    reg := c
//    when(cond){
//      reg := d
//    }
//    result := reg

    //Case 10
    val subA = new Component{
      val input = in UInt(4 bits)
      val output = out UInt(4 bits)
      output := input
    }
    val subB = new Component{
      val input = in UInt(4 bits)
      val output = out UInt(4 bits)
      output := input
    }
    subA.input := (a << 1)
    subB.input := (subA.output << 1)
    result := subB.output



    //Case 11
//    val sub = new Component{
//      val input = in UInt(4 bits)
//      val output = out UInt(4 bits)
//      val tmp = UInt(4 bits)
//      output := tmp << 1
//    }
//    sub.input := a
//    //    sub.tmp := 3
//    result := sub.output

    //Case 12
//    val sub = new Sub12(4)
//    result := sub.output

    //Case13
//    val sub = new Component{
//      val output = (U"0010")
//
//    }
//
//    val sub2 = new Component{
//      val output = out UInt(4 bits)
//      output :=sub.output.pull()
//
//    }
//
//    result := sub2.output.pull()

    //Case 14
//    val sub = new Component{
//      val sub = new Component{
//        val sub = new Component{
//          val sub = new Component{
//            val output = out( U"0010")
//          }
//        }
//      }
//    }
//
//    val sub2 = new Component{
//      val sub2 = new Component{
//        val sub2 = new Component{
//          val sub2 = new Component{
//            val output = UInt(4 bits).keep()
//            output := sub.sub.sub.sub.output.pull()
//          }
//        }
//      }
//    }
//
//    result := sub2.sub2.sub2.sub2.output.pull()

    //Case 15
//    val sub = new Component{
//      val sub = new Component{
//        val sub = new Component{
//          val sub = new Component{
//            val output = RegNext(U"0010").keep()
//          }
//        }
//      }
//    }
//    val sub2 = new Component{
//      val sub = new Component{
//        val sub = new Component{
//          val sub = new Component{
//            val output = RegNext(U"0010").keep()
//          }
//        }
//      }
//    }
//    result := 0


    //Case16
//    val sub = new Component{
//      val input = in UInt(4 bits)
//      val output = out UInt(4 bits)
//      output := input
//    }
//    when(cond) {
//      sub.input := a
//    }otherwise{
//      sub.input := b
//    }
//    result := sub.output

    //Case 17
//    def doIt: Unit ={
//      val x,y,z = UInt(4 bits)
//      x := a
//      y := x
//      z := y
//      result := z
//    }
//    doIt
    
    
    //Case 18
   // val bits = B"0011"
//    val bits = a.asBits
//    val myReg = RegNext(bits)init(0)
//    result := myReg.asUInt

    //Case 19
//    val subA = new Component{
//      val io = new Bundle {
//        val xx = new Bundle {
//          val input = in UInt (4 bits)
//          val output = out UInt (5 bits)
//        }
//      }
//      io.xx.output := io.xx.input << 1
//    }
//    val subB = new Component{
//      val input = in UInt(4 bits)
//      val output = out UInt(4 bits)
//      output := input
//    }
//    subA.io.xx.input := a
//    subB.input := subA.io.xx.output
//    result := subB.output


    //Case 20
//    val subA = new Component{
//      val input = in UInt (4 bits)
//      val output = out UInt (4 bits)
//    }
//    val subB = new Component{
//      val input = in UInt(4 bits)
//      val output = out UInt(4 bits)
//      output := input
//    }
//    val tmp = UInt(4 bits)
//
//    subA.input <> tmp
//    subA.input <> subB.output
//    subA.input <> a
//
//    subA.output <> tmp
//    subA.output <> subB.input
//    subA.output <> result
//
//    a <> tmp
//    a <> subA.input
//    a <> result
//
//    result <> a
//    result <> subA.output
//    result <> tmp
//
//    result := 0


  }
  class Sub12(val depth : Int) extends Component{
    val output = out UInt(4 bits)
    depth match{
      case 0 => output := 3
      case _ => {
        val sub = new Sub12(depth-1)
        output := sub.output
      }
    }
  }
  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel())
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
    for (bit <- io.input.asBools) {
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
    val word = Cat(True, gray(n - 3 downto  0), even)
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
    ufix := uint.toUFix
    val uintBack = U(ufix)

    val sfix = SFix(7 exp, 12 bit)
    val sint = SInt(3 bit)
    sfix := (sint).toSFix
    val sintBack = S(sfix)


    val ast = BigDecimal(2)
    val toto = ast * 2
    val tata = ast * 2.0
    val ypli : BigDecimal = 4
    val a : BigInt = 2
    ufix.:=(2)
    ufix := 2.0
    ufix := 2f
    ufix := toto
    sfix > (1)

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




object PlayOverride {

  class TopLevel extends Component {
    val cmd = in UInt(4 bits)
    val rsp = out UInt(4 bits)

    val myArea = new Area{
      val toto = RegNext(cmd)
      toto := cmd
    }
    rsp := 0
    rsp := cmd
    True := False
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}


object PlayRecursive {

  class Mul(width : Int) extends Component {
    val io = new Bundle() {
      val a,b    = in  UInt(width bits)
      val result    = out UInt(width*2 bits)
    }

    val logic = if(width == 32) new Area{
      io.result := io.a*io.b
    } else new Area{
      val ll,lh,hl,hh = new Mul(width/2)
      val a = io.a.subdivideIn(2 slices)
      val b = io.b.subdivideIn(2 slices)
      ll.io.a := a(0)
      ll.io.b := b(0)
      lh.io.a := a(1)
      lh.io.b := b(0)
      hl.io.a := a(0)
      hl.io.b := b(1)
      hh.io.a := a(1)
      hh.io.b := b(1)
      io.result := (hh.io.result << width)  +(hl.io.result << width / 2) + (lh.io.result << width / 2)  +  ll.io.result
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new Mul(128))
    val source = scala.io.Source.fromFile("Mul.vhd")
    val lines = try source.mkString finally source.close()
    println(lines.split(" end pkg_scala2hdl;\n").last)
  }

}

object PlayDontCare {

  class TopLevel extends Component {
    val a, b = in UInt (4 bit)
    val result = out UInt (4 bit)

    //result.assignDontCare()
    when(a > 2) {
      result := b
    }
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PlaySymplify {

  class TopLevel extends Component {
    val flush = in Vec(Bool,4)
    val readyParalel = out Vec(False,4)
    val readySerial = out Vec(False,4)

    for(i <- 0 to 3){
      if(i != 3)
        readySerial(i) := readySerial(i + 1)
      when(flush(i)){
        readySerial(i) := True
        for(i2 <- 0 to i){
          readyParalel(i2) := True
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}


//object PlayDebug22{
//  object AESCoreSpec {
//
//    def blockWidth  = 128 bits
//
//    def nbrRound(keySize: BitCount): Int = keySize.value match{
//      case 128 => 10
//      case 192 => 12
//      case 256 => 14
//      case _   => SpinalError(s"AES doesn't support the following key size $keySize")
//    }
//
//
//    def sBox = List(
//      0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
//      0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
//      0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
//      0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
//      0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
//      0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
//      0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
//      0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
//      0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
//      0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
//      0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
//      0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
//      0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
//      0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
//      0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
//      0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16
//    )
//
//
//    def sBoxInverse = List(
//      0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb,
//      0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb,
//      0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e,
//      0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25,
//      0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92,
//      0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84,
//      0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06,
//      0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b,
//      0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73,
//      0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e,
//      0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b,
//      0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4,
//      0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f,
//      0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef,
//      0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61,
//      0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d
//    )
//
//
//    def rcon = List(
//      0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a,
//      0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef, 0xc5, 0x91, 0x39,
//      0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 0x25, 0x4a, 0x94, 0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a
//    )
//
//  }
//  case class KeyManagerRsp_Std() extends Bundle{
//    def key_i = Bits(AESCoreSpec.blockWidth)
//  }
//
//  class KeyManagerCore_Std(keyWidth: BitCount) extends Component{
//
//    //assert(List(128, 192, 256).contains(keyWidth.value), s"AES doesn't support the following key size ${keyWidth.value}")
//    assert(List(128).contains(keyWidth.value), s"AES doesn't support the following key size ${keyWidth.value}")
//
//    val sBoxMem = Mem(Bits(8 bits), AESCoreSpec.sBox.map(B(_, 8 bits)))
//    val rconMem = Mem(Bits(8 bits), AESCoreSpec.rcon.map(B(_, 8 bits)))
//
//
//    val io = new Bundle{
//
//      val init     = in Bool
//      val update   = in Bool
//      val round    = in UInt(log2Up(AESCoreSpec.nbrRound(keyWidth)) bits)
//
//      val key      = in Bits(keyWidth)
//
//      val rsp      = out(Flow(KeyManagerRsp_Std()))
//    }
//
//    // Store the current key
//    val wx = Reg(Vec(Bits(32 bits), 4))
//
//
//    io.rsp.valid := False
//    io.rsp.key_i := wx.asBits // wx(3) ## wx(2) ## wx(1) ## wx(0)
//
//
//    val keyWord = io.key.subdivideIn(32 bits)
//
//    // Compute the new key value
//    val w0 = wx(0) ^ gFunc(rconMem(io.round), wx(3).asUInt)
//    val w1 = w0 ^ wx(1)
//    val w2 = w1 ^ wx(2)
//    val w3 = w2 ^ wx(3)
//
//    when(io.init){
//      for(i <- 0 until 4){
//        wx(i) := keyWord(i)
//      }
//    }
//
//    when(io.update){
//      wx(0) := w0
//      wx(1) := w1
//      wx(2) := w2
//      wx(3) := w3
//      io.rsp.valid := True
//    }
//
//
//    def gFunc(rc: Bits, word: UInt): Bits = {
//      val result = Bits(32 bits)
//
//      result( 7 downto  0) := sBoxMem(word(15 downto  8)) ^ rc
//      result(15 downto  8) := sBoxMem(word(23 downto 16))
//      result(23 downto 16) := sBoxMem(word(31 downto 24))
//      result(31 downto 24) := sBoxMem(word( 7 downto  0))
//
//      return result
//    }
//    /*
//      def hFunc(word: Bits): Bits ={
//        val result = cloneOf(word)
//        for((r,w) <- result.subdivideIn(32 bits).zip(word.subdivideIn(32 bits))) r := sBoxMem(w)
//        result
//      }
//    */
//  }
//
//  case class FirPolyPhaseConfig(Fsin : HertzNumber, Fsout : HertzNumber, Fcut : HertzNumber, nbtaps : Int){
//    lazy val coef = {
//      val values = new Array[Double](nbtaps)
//      for(i <- 0 until nbtaps){
//        Fsin / Fsout * 2.0 * Math.sin(Math.PI * i / 12)
//        val myDouble = Fsin.toDouble
//        values(i) = ???
//      }
//      values
//    }
//  }
//
//  class FirPolyPhaseImpl(config : FirPolyPhaseConfig, bitWidth : Int) extends Component {
//    val io = new Bundle {
//      //...
//    }
//    config.coef(???)
//    // ...
//  }
//
//  object Main {
//    def main(args: Array[String]): Unit = {
//      val config = FirPolyPhaseConfig(
//        Fsin = 100 MHz,
//        Fsout = 10 MHz,
//        Fcut = 2 MHz,
//        nbtaps = 8
//      )
//      SpinalVhdl(new FirPolyPhaseImpl(config, 10))
//      //SpinalVhdl(new KeyManagerCore_Std(128 bits)).printPruned().printUnused()
//    }
//  }
//}

//object PlayBug {
//  class TopLevel extends Component{
//    val ram = Mem(Bits(32 bits), 256)
//    val port1 = ram.writePort
//    val port2 = ram.writePort
//
//    val write1, write2 = in Bool()
//    val addrCounter = CounterFreeRun(256)
//
//    port1.address := addrCounter
//    port1.data := 0x33333333
//    port1.valid := write1
//
//    port2.address := addrCounter.valueNext
//    port2.data := 0x77777777
//    port2.valid := write2
//
//    val rport = ram.readSyncPort
//    rport.cmd.payload := port1.address
//    rport.cmd.valid := True
//  }
//
//  def main(args: Array[String]): Unit = {
//    SpinalConfig().generateVerilog(new TopLevel())
//    SpinalConfig().generateVhdl(new TopLevel())
//  }
//}

//addAttribute("ramstyle", "no_rw_check")

object PlayBug extends App{
  import spinal.core._
  new SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)).generateVerilog(new Component{
    val io=new Bundle{
      val sig0=out Bool() setAsReg() init(False)
      val sig1=out Bool () setAsReg() init(False)
    }
    val normalMode=new Area{
      io.sig0:= ~io.sig0
    }
    val slowMode=new SlowArea(25 MHz){
      io.sig1:= ~io.sig1
    }
  })
//  SpinalVerilog(new Component{
//    val mem = Mem(Bits(16 bits), 64)
//    mem.generateAsBlackBox()
//
//    val wr = new Area{
//      val enable = in Bool()
//      val address = in UInt(7 bits)
//      val data = in Bits(8 bits)
//      mem.writeMixedWidth(address, data, enable)
//    }
//
//    val rd = new Area{
//      val enable = in Bool()
//      val address = in UInt(8 bits)
//      val data = out Bits(4 bits)
//      mem.readSyncMixedWidth(address, data, enable)
//    }
//  })
}

object PlayRamInfer {
  class TopLevel extends Component{
    val a = new Area {
      val ram = Mem(Bits(8 bits), 1024)

      val write = slave(ram.writePort)
      val read = slave(ram.readSyncPort)
    }

    val b = new Area {
      val ram = Mem(Bits(8 bits), 1024)

      val write = slave(ram.writePort)
      val read_1 = slave(ram.readSyncPort)
      val read_2 = slave(ram.readSyncPort)
      val read_3 = slave(ram.readSyncPort)
    }

    val c = new Area {
      val ram = Mem(Bits(8 bits), 64)

      val write = slave(ram.writePort)
      val readAddress = in UInt(6 bits)
      val readData = out(ram.readAsync(readAddress))
    }

    val c2 = new Area {
      val ram = Mem(Bits(8 bits), 64)

      val write = slave(ram.writePort)
      val readAddress_1 = in UInt(6 bits)
      val readData_1 = out(ram.readAsync(readAddress_1))
      val readAddress_2 = in UInt(6 bits)
      val readData_2 = out(ram.readAsync(readAddress_2))
    }

    val d = new Area {
      val ram = Mem(Bits(8 bits), 1024)

      val address = in UInt(10 bits)
      val writeData = in Bits(8 bits)
      val readData = out Bits(8 bits)
      val enable = in Bool()
      val write = in Bool()
      readData := ram.readWriteSync(address, writeData, enable, write)
    }

    val e = new Area {
      val ram = Mem(Bits(8 bits), 1024)

      val address_1 = in UInt(10 bits)
      val writeData_1 = in Bits(8 bits)
      val readData_1 = out Bits(8 bits)
      val enable_1 = in Bool()
      val write_1 = in Bool()
      readData_1 := ram.readWriteSync(address_1, writeData_1, enable_1, write_1,readUnderWrite = writeFirst)

      val address_2 = in UInt(10 bits)
      val writeData_2 = in Bits(8 bits)
      val readData_2 = out Bits(8 bits)
      val enable_2 = in Bool()
      val write_2 = in Bool()
      readData_2 := ram.readWriteSync(address_2, writeData_2, enable_2, write_2)
    }

    val f = new Area {
      val ram = Mem(Bits(8 bits), 1024)

      val address_1 = in UInt(10 bits)
      val write_1 = in Bool()
      val dataWrite_1 = in Bits(8 bits)
      val dataRead_1 = out Bits(8 bits)
      ram.write(address_1, dataWrite_1, write_1)
      dataRead_1 := ram.readSync(address_1)


      val address_2 = in UInt(10 bits)
      val write_2 = in Bool()
      val dataWrite_2 = in Bits(8 bits)
      val dataRead_2 = out Bits(8 bits)
      ram.write(address_2, dataWrite_2, write_2)
      dataRead_2 := ram.readSync(address_2)
    }
  }

  def main(args: Array[String]): Unit = {
    SpinalConfig(device = Device.ALTERA).generateVerilog(new TopLevel())
    SpinalConfig(device = Device.ALTERA).generateVhdl(new TopLevel())
  }
}

object PlaySynth{
  def main(args: Array[String]) {
    val v = new Rtl {
      override def getName(): String = "ReedSolomonEncoderVerilog"
      override def getRtlPath(): String = "ReedSolomonEncoder.v"

    }


    val vhd = new Rtl {
      override def getName(): String = "ReedSolomonEncoderVhdl"
      override def getRtlPath(): String = "ReedSolomonEncoder.vhd"
    }

    val rtls = List(vhd, v)

    val targets = XilinxStdTargets(
      vivadoArtix7Path = "E:\\Xilinx\\Vivado\\2016.3\\bin"
    ) ++ AlteraStdTargets(
      quartusCycloneIIPath = "D:/altera/13.0sp1/quartus/bin64",
      quartusCycloneIVPath = "D:/altera_lite/15.1/quartus/bin64",
      quartusCycloneVPath  = "D:/altera_lite/15.1/quartus/bin64"
    )

    Bench(rtls, targets, "E:/tmp/")
  }
}


object PlayFragment{
  class TopLevel() extends Component{
    val io = new Bundle{
      val input  = slave(Stream(Fragment(Bits(8 bits))))
      val output = master(Stream(Fragment(Bits(8 bits))))
    }

    io.output << Vec[Bits](0x11, 0x22, 0x33, 0x44).reverse.foldLeft(io.input)(_.insertHeader(_))
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
    SpinalVerilog(new TopLevel)
  }
}

object PlayOpt {

  class TopLevel extends Component {
    val a = UInt(2 bits)
    a.assignDontCare()
    when(True) {
      switch(U(0)) {
        is(0) {
          a := in(UInt(2 bits))
        }
      }
    }
    val b = U(3)


  }

  def main(args: Array[String]): Unit = {
    SpinalVerilog(new TopLevel)
  }
}


object PlayCombLoop {

  class TopLevel extends Component {
    val input = in Bits(16 bits)

    val a,b = Bits(32 bits)

//    a.addTag(noCombinatorialLoopCheck)
    a := b(15 downto 0)  ## input(15 downto 0) //PASS
    b := a(31 downto 16) ## a(15 downto 0)    //PASS

//    a := b

//    b := a

    val output = out(Bits(16 bits))
    output := b(31 downto 16)
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}



object PlayIf {

  class TopLevel extends Component {
    val a, b = in UInt (4 bit)
    val result = out UInt (4 bit)

    result := 1
    when(a > 2) {
      result := 2
    } elsewhen(a > 3) {
      result(0) := True
      when(a > 50) {
        result := 5
      }
      result(2) := False
    } otherwise {
      result := 4
    }
    /*
        result := 1
        when(a > 2){
          result := 2
        }
        result(3) := False
        result(2) := False
        result(3) := False
        result(2) := False

        when(a > 2){
          result := 2
        }
        result(1) := False*/

  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

//object PlayVecBaseType {
//
//  class TopLevel extends Component {
//
//    val tmp = new VecBaseType(UInt(4 bit),Seq(4).toArray)
//    val output = out UInt(4 bit)
//
//
//    for(idx <- 0 to 3){
//      tmp.write(idx*2,Seq(idx))
//    }
//    var sum = UInt(4 bit)
//    sum := 0
//    for(idx <- 0 to 3){
//      sum = sum + tmp.read(Seq(idx))
//    }
//    output := sum
//  }
//
//  def main(args: Array[String]): Unit = {
//    SpinalVhdl(new TopLevel)
//  }
//}
object PlayZeroWidth {

  class TopLevel extends Component {
    out(in(Bits(0 bit)) === 0)
//    out(in(UInt(0 bit)) + in(UInt(0 bit)))
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}
object PlayVec {

  class TopLevel extends Component {
//    val sel = in UInt(2 bit)
//    val vecIn = in Vec(UInt(4 bit),4)
//    val vecOut = out Vec(UInt(4 bit),4)
//
//    vecOut := vecIn
//    vecOut(sel)(2 downto 0) := 0

    val n = 5
    val pow = 1<<n
    val sel,sel2 = in UInt(n bit)
    val vecIn = in Vec(Vec(UInt(4 bit),pow),pow)
    val vecOut = out Vec(Vec(UInt(4 bit),pow),pow)
    U(2).toString
    vecOut := vecIn
    vecOut(sel)(sel2) := 0
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}
object PlaySwitch {

  class TopLevel extends Component {
    val a, b = in UInt (4 bit)
    val result = out UInt (4 bit)
    switch(a) {
      is(0 to 5) {
        result := 2
      }
      is (9) {
        result := 2
      }
      default{
        result := 4
      }
    }

  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PlayLoop {

  class TopLevel extends Component {
          val io = new Bundle() {
            val input = in UInt(4 bit)
            val output = out UInt(4 bit)
          }
          val tmp =  UInt(4 bit)
          tmp(0) := io.input(0)
          tmp(1) := io.input(1) || tmp(0)
          tmp(2) := io.input(2) || tmp(1)
          tmp(3) := io.input(3) || tmp(2)
          io.output := tmp
//    val io = new Bundle() {
//      val input = in UInt (2 bit)
//      val output = out UInt (2 bit)
//    }
    //    val tmp =  UInt(2 bit)
    //    tmp(0) := tmp(1)
    //    tmp(1) := tmp(0)
    //    io.output := tmp
    val myFlow = Flow(Bool)
    val myStream = myFlow.toStream.stage
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}


//
//import org.scalameter.api._
//
//object RangeBenchmark extends Bench.LocalTime {
////  val time = measure {
////    for (i <- 0 until 100000) yield i
////  }
////  println(s"Total time: $time")
////  MandelbrotSblDemo.main(null)
////  val sizes = Gen.range("size")(300000, 1500000, 300000)
////
////  val ranges = for {
////    size <- sizes
////  } yield 0 until size
////
////  performance of "Range" in {
////    measure method "map" in {
////      using(ranges) in {
////        r => r.map(_ + 1)
////      }
////    }
////  }
//}
//object PlayBench {
//
//
//  def main(args: Array[String]): Unit = {
//    import org.scalameter._
//    val time = measure {
//      for (i <- 0 until 100000) yield i
//    }
//    println(s"Total time: $time")
//  }
//}

object PlayEnum {

  object MyEnum extends SpinalEnum {
    val s0, s1, s2, s3, s4, s5, s6, s7, s8, s9 = newElement()
  }

  class TopLevel extends Component {

    object MyEnum extends SpinalEnum(native) {
      val s0, s1, s2, s3, s4, s5, s6, s7, s8, s9 = newElement()
    }

    //    val input = in(MyEnum())
    //    val output = out(MyEnum(oneHot))
    //    val tmp = MyEnum()
    //    tmp := MyEnum.s3
    //    when(input === MyEnum.s4){
    //      tmp := MyEnum.s7
    //    }
    //    output := tmp
    val cond = in Bool()
    val input = in(MyEnum)


    val tmp = Reg(MyEnum(binarySequential))

    tmp := MyEnum.s3
    when(input === MyEnum.s4) {
      tmp := MyEnum.s7
    }
    when(input === MyEnum.s5) {
      tmp := Mux(cond, MyEnum.s6(binarySequential), MyEnum.s8(binarySequential))
    }
//    val output = out(MyEnum())
//    output := tmp

    out(tmp)
    out(tmp.asBits)
    out(input.asBits)
  }

  def main(args: Array[String]): Unit = {
    SpinalVerilog(new TopLevel)
  }
}
object PlayMul {

  class TopLevel extends Component {
    //out(RegNext(RegNext(RegNext(in(UInt(32 bit)))*RegNext(in(UInt(32 bit))))))


    val aSigned,bSigned = in Bool
    val a,b = in Bits(32 bit)
    val outLow = out Bits(32 bit)
    val outHigh = out Bits(32 bit)

    val aULow = RegNext(a)(15 downto 0).asUInt
    val bULow = RegNext(b)(15 downto 0).asUInt
    val aLow = RegNext(False ## a)(15 downto 0).asSInt
    val bLow = RegNext(False ## b)(15 downto 0).asSInt
    val aHigh = RegNext(((aSigned && a.msb) ## a(31 downto 16))).asSInt
    val bHigh = RegNext(((bSigned && b.msb) ## b(31 downto 16))).asSInt

    val mul_ll = RegNext(aULow*bULow)
    val mul_lh = aLow * bHigh
    val mul_hl = aHigh * bLow
    val mul_hh = RegNext(aHigh*bHigh)
    val mul_mm = RegNext(mul_lh + mul_hl)

    val resultLow = mul_ll.asSInt + ((False ## mul_mm).asSInt << 16)
    val resultHigh = RegNext(resultLow) + RegNext((mul_hh << 32))

    outLow := RegNext(resultLow(31 downto 0).asBits)
    outHigh := RegNext(resultHigh(63 downto 32).asBits)
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PlayAdd {

  class TopLevel extends Component {
    //out(RegNext(RegNext(RegNext(in(UInt(32 bit)))*RegNext(in(UInt(32 bit))))))


    val width = 48
    val a,b = in UInt(width bit)
    val result = out UInt(width bit)
    result := RegNext(RegNext(a) + RegNext(b))
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}




object PlayShift {

  class TopLevel extends Component {
    val input = in Bits (8 bit)
    val sel = in UInt (1 bit)
    val output = out(input(sel * 4, 4 bit))
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PlayDivide {
  case class DividerCmd(nWidth : Int, dWidth : Int) extends Bundle{
    val numerator = UInt(nWidth bit)
    val denominator = UInt(dWidth bit)
  }
  case class DividerRsp(nWidth : Int, dWidth : Int) extends Bundle{
    val quotient = UInt(nWidth bit)
    val remainder = UInt(dWidth bit)
    val error = Bool
  }

  class Divider(nWidth : Int, dWidth : Int) extends Component{
    val io = new Bundle{
      val cmd = slave Stream(DividerCmd(nWidth,dWidth))
      val rsp = master Stream(DividerRsp(nWidth,dWidth))
    }
    val done = RegInit(True)
    val waitRsp = RegInit(False)
    val counter = Counter(nWidth)
    val numerator = Reg(UInt(nWidth bit))
    //  val quotient = Reg(UInt(nWidth bit))
    val remainder = Reg(UInt(dWidth bit))
    val remainderShifted = (remainder ## numerator.msb).asUInt
    val remainderMinusDenominator = remainderShifted - io.cmd.denominator

    io.cmd.ready := False
    io.rsp.valid := False
    io.rsp.quotient := numerator
    io.rsp.remainder := remainder
    io.rsp.error := False

    when(done){
      when(io.cmd.valid && (!waitRsp || io.rsp.ready)){
        counter.clear()
        //      quotient := 0
        remainder := 0
        numerator := io.cmd.numerator
        when(io.cmd.denominator === 0) {
          io.rsp.error := True
          io.rsp.valid := True
          io.cmd.ready := io.rsp.ready
        }otherwise{
          done := False
        }
      }

      when(io.rsp.ready){
        waitRsp := False
      }

    }otherwise{
      counter.increment()
      remainder := remainderShifted.resized
      numerator := (numerator ## !remainderMinusDenominator.msb).asUInt.resized
      //    quotient := (quotient ## !remainderMinusDenominator.msb).asUInt.resized
      when(!remainderMinusDenominator.msb){
        remainder := remainderMinusDenominator.resized
      }
      when(counter.willOverflowIfInc){
        done := True
        waitRsp := True
        io.cmd.ready := True
      }
    }
  }
  //    var Q = 0
  //    var R = 0
  //    for(i <- n-1 to 0 by -1){
  //      R = R << 1
  //      R = R | ((N >> i)&1)
  //      if(R >= D){
  //        R = R-D
  //        Q = Q | (1 << i)
  //      }
  //    }
  class TopLevel extends Component {

    val a = in SInt(32 bit)
    out(Mux(a.msb,~a,a) + (False ## a.msb).asSInt)
//    val start = in Bool
//    val signed = in Bool
//    val numerator,denominator = in Bits (32 bit)
//    val quotient,remainder = out Bits(32 bit)
//
//
//    val rem = Reg(Bits(32 bit))
//    when(start){
//      //rem := dividend
//    }otherwise{
//
//    }
  }

  def main(args: Array[String]): Unit = {
//
//
  SpinalVhdl(new Divider(32,32).setDefinitionName("TopLevel"))
//    def div(N : Int,D_ : Long) : (Int,Int) = {
//
//   val n = 32

   println(10 % 3)
   println(10 % -3)
   println(-10 % 3)
   println(-10 % -3)

//   var Q = 0
//   var P: Long = N
//   val D = D_ << n
//   for (i <- n - 1 to 0 by -1) {
//     if (P >= 0) {
//       Q = Q | (1 << i)
//       P = (P << 1) - D
//     } else {
//       P = (P << 1) + D
//     }
//   }
//
//   Q = Q - (~Q)
//   if (P < 0) {
//     Q = Q - 1
//     P = P + D
//   }
//   P >>= n
//   return (Q,P.toInt)
// }
//
//
//    println(div(82,-7))
//    for(i <- 0 to 1000){
//      val a,b = Random.nextInt()
//      if(b != 0  && b > 0){
//        val (q,r) = div(a,b)
//        assert(q == a/b)
//        assert(r == a % b)
//      }
//    }

   // println(Q + " " + P)


//    var Q = 0
//    var R = 0
//    for(i <- n-1 to 0 by -1){
//      R = R << 1
//      R = R | ((N >> i)&1)
//      if(R >= D){
//        R = R-D
//        Q = Q | (1 << i)
//      }
//    }
//    var P = N
//    var D := D << n              * P and D need twice the word width of N and Q
//    for i = n-1..0 do        * for example 31..0 for 32 bits
//    if P >= 0 then
//      q[i] := +1
//    P := 2*P - D
//    else
//    q[i] := -1
//    P := 2*P + D
//    end if
//      end



  }
}

object PlaySimplif {

  class TopLevel extends Component {
    val input = in Bits (32 bit)
    val output = out Bits(2 bit)

    when(input === M"00001111----------------0000----"){
      output := 0
    }elsewhen(input === M"00001111----------------0001----"){
      output := 1
    }elsewhen(input === M"00001111----------------0010----"){
      output := 2
    }elsewhen(input === M"00001111----------------0011----"){
      output := 3
    }otherwise{
      output.assignDontCare()
    }
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}



object PlayStream {

  case class Struct() extends Bundle {
    val data = UInt(5 bit)
    val c = UInt(5 bit)
  }

  class TopLevel extends Component {
//    val cmd = master Stream (Struct())
//    cmd.valid := True
//    cmd.payload.data := 1
//    cmd.c === 2
//
//
//    val cmd2 = master Stream (UInt(4 bit))
//    cmd2.valid := True
    val decode = new Area{


        val source = slave Stream (new Bundle{
          val a = Bool
        })
        val sink = master (source.clone)

        source >> sink
    }
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}
object PlayBlink {
  class TopLevel extends Component {
    val io = new Bundle{
      val led = out Bool()
    }
    val counter = Reg(UInt(24 bit)) init(0)
    counter := counter + 1

    io.led := counter.msb
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
      val s0, s1 = newElement()
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
          output0.payload := input.payload
          input.ready := output0.ready
          when(input.valid && output0.ready) {
            stateNext := s1
          }
        }
        default {
          //is(s1){
          output1.valid := input.valid
          output1.payload := input.payload
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




object MessagingPlay {

  class TopLevel extends Component {
    val o = out(True)
    when(True) {
      o := True
    }

    val exeption = new Throwable()
    var str = exeption.getLocalizedMessage
    println(str)
    str = exeption.getMessage
    println(str)
    //exeption.printStackTrace()
    println(exeption.getStackTrace().apply(0).toString)
    println("spinal.tester.code.MessagingPlay$TopLevel.delayedEndpoint$spinal$tester$code$MessagingPlay$TopLevel$1(Play1.scala:74)")
    println("spinal.tester.code.MessagingPlay$TopLevel(Play1.scala:742)")
    println("spinal.tester.code.MessagingPlay$TopLevel(Play1.scala:742)")
    println("spinal.tester(Play1.scala:742)")
    println("spinal.tester.code.MessagingPlay(Play1.scala:742)")
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)

  }
}


object BlueVgaPlay {
  class TopLevel extends Component {
    val io = new Bundle{
      val vga = master(Vga(RgbConfig(8, 8, 8)))
    }

    val vgaCtrl = new VgaCtrl(io.vga.color.c, 12)
    vgaCtrl.io.softReset := False
    vgaCtrl.io.timings.setAs_h640_v480_r60 //Static timing for 640*480 pixel at 60HZ
    vgaCtrl.io.vga <> io.vga
    vgaCtrl.io.pixels.valid := True
    vgaCtrl.io.pixels.r := 255
    vgaCtrl.io.pixels.g := 0
    vgaCtrl.io.pixels.b := 0
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PerfPlay {
  class TopLevel extends Component {
    val inputs = in Vec(UInt(32 bit), 5000)
    val outputs = out Vec(UInt(32 bit), 5000)
    val cond = in Bool()
    for ((output, input) <- (outputs, inputs).zipped) {
      output := input
      when(cond) {
        output := input
        when(cond) {
          output := input
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}



//object VecBaseTypePlay {
//
//  class TopLevel extends Component {
//    val inputs = in Vec(UInt(4 bit))
//    val output = out(new VecNode)
//    output.inputs ++= inputs
//  }
//
//  def main(args: Array[String]): Unit = {
//    SpinalVhdl(new TopLevel)
//  }
//}

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

//
//object PlayMacro {
//  import spinal.core.MacroTest._
//
//  class TopLevel extends Component {
//    val e = enum('s1, 's2, 's3)
//
//    import e._
//
//    val e2 = enum('s1, 's2, 's3)
//
//    import e2._
//
//
//    println("ASD3")
//    out(True)
//
//    val s = e()
//    s := e.s1
//    out(s)
//    val s2 = e2()
//    s2 := e2.s1
//    out(s2)
//  }
//
//  def main(args: Array[String]) {
//    //createEnum("asd")
//    val a = bar("toto")
//    println(a.asd)
//
//
//    SpinalVhdl(new TopLevel)
//    println("Done")
//  }
//}
//
//object PlayMacroLib {
//  import spinal.core.MacroTest._
//
//  class TopLevel extends Component {
//
//  }
//
//  def main(args: Array[String]) {
//    var titi = 2
//    val a = new MacrosClass
//    val x = a.doit("asd")
//    print(x(2))
//
//    val y = a.doit2((x : Int) => x + 1)
//    print(y(2))
//
//    val z = a.doit3((x : Int) => x + 1)
//    print(z(2))
//
//  }
//}

object PlayMaskedLiteral {

  class TopLevel extends Component {
    val input = in UInt (4 bit)
    val a, b, c = in UInt (4 bit)


    // val output2 = out(U"0000")
    // output2 := input
    //  output2 assignMask M"10--"
    //    val output3 = out(M"10--" === input)


    val output4 = out(UInt(4 bit))

    output4.assignDontCare()
    when(input(0)) {
      output4 := a
    }
    when(input(1)) {
      output4 := b
    }
    //    switch(input){
    //      is(M"00--") {output4 assignMask M"1111"}
    //      is(M"01--") {output4 assignMask M"0101"}
    //      is(M"10--") {output4 assignMask M"0011"}
    //      is(M"11--") {output4 assignMask M"0001"}
    //    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}

object PlayCombLoop2 {

  class TopLevel extends Component {
    val input = in Bits(4 bit)
    val output = out Bits(4 bit)
    val tmp,tmp2,tmp3,tmp4= Bits(4 bit)
    val tmps = Bits(2 bit)
//    output := "0000"
//    when(input === "0011"){
//      output := "0001"
//    }
//    tmp := input
//    tmp(3 downto 2) := tmp2(3 downto 2)
//    tmp2(1 downto 0) := tmp(1 downto 0)
//    tmp2(3 downto 2) := tmp(1 downto 0)
//    output := 0
//    output(0) := tmp2(0)

//
//      tmp(1 downto 0) := input(1 downto 0)
//      tmp(3 downto 2) := tmps
//      tmps := tmp(1 downto 0)
//      output(3 downto 2) := tmp(3 downto 2)
//      output(1 downto 0) := tmps



      val inputStream = slave Stream Bits(4 bit)
      val outputStream = master Stream Bits(4 bit)
      outputStream << inputStream.haltWhen(outputStream.ready)
//  tmp2 := 0
//    tmp := 0
//    tmp2(3 downto 2) := tmp(2 downto 1)
//    tmp(2 downto 1):= tmp2(1 downto 0)
//    output := tmp
//    tmp2 := input & tmp4
//    tmp3 := input & tmp2
//    tmp4 := tmp3
//    tmp := tmp2 & tmp3
//    output := tmp

      tmp := 0
      tmp(1) := tmp(2)
      tmp(2) := tmp(1)
      output := tmp

//    val input = in UInt(80 bit)
//    val output = out UInt(80 bit)
//    val tmp = UInt(80 bit)
//
//
//    tmp(0) := True
//    tmp(48) := True
//    tmp(77) := True
//    tmp(78) := True
//    output := tmp
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}



object PlayCombLoop23 {

  class TopLevel extends Component {

    val toto = Bool
    val titi = UInt(8 bits)
    val tata = UInt(4 bits)


    toto := tata(2)
    titi := toto.asUInt + U"xFF"
    tata := titi(3 downto 0)


    out(RegNext(tata))

  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}

object PlayLiteral {

  class TopLevel extends Component {

//
//    val out1 = out (U((7 downto 0) -> False))
//    val out2 = out (U((7 downto 0) -> true))
//    val out3 = out (U((7 downto 0) -> "00001111"))
//    val out4 = out (U(1 -> false,default -> true))
//    val out5 = out (U(1 -> False,default -> True))


//    val cond = in Bool
//    val out8bit = out UInt(8 bit)
//    out8bit := U(0)
//    when(cond){
//      out8bit := 1
//    }otherwise{
//      out8bit := 2
//    }



    val mySInt =  out(S"x8000")
    val mySInt2 = out(S"16'x8000")
    val mySInt3 = out(S"d-32768")
    val mySInt4 = out(S"x7FFF")
    val mySInt5 = out(S"16'x7FFF")
    val mySInt6 = out(S"d32767")

    val myUInt =  out(U"x8000")
    val myUInt2 = out(U"16'x8000")
    val myUInt3 = out(U"16'd65535")
    val myUInt4 = out(U"x7FFF")
    val myUInt5 = out(U"16'x7FFF")
    val myUInt6 = out(U"16'd32767")
//    val output = out(Vec(
//      B"0000_1100",
//      B"h0C",
//      B"8'hC",
//      B"8'd12"
//    ))

//    val output2 = out(U(1 -> True, 0 -> False, (1 to 3) -> U"00"))
    //  output2(1,0) := U"00"
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}

//TODO switch test comb loop and uncomplet assignment, maybe switchnode should extend multiple assignment node ?
object PlaySwitch2 {

  class TopLevel extends Component {
    val cond = in Bool()
    val sel = in UInt (4 bit)
    val result = out UInt (4 bit)
    val result2 = out UInt (4 bit)

    //    result := 0
    //    when(cond){
    //      result := 1

    //    }

    result := U"0000"
    result2 := 1
    when(sel === U"1000") {
      result := 0
    }
    switch(sel) {
      is(U"1001") {
        result := U"0001" //TODO switch with resize
        result(1) := False

        result2 := 2
        when(sel === U"1000") {
          result2 := 0
        }
      }
      is(U"1010") {
        result := U"0010"
      }
      default {
        result := U"0011"
      }
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}

object PlayRecAssign {

  class TopLevel extends Component {
    val sel = in UInt(4 bit)
    val output = out UInt(16 bit)

    val tmp = UInt()
    tmp := 0
    tmp(4) := True
    output := tmp.resized
    output := 0
    output(8 downto 4) := 1
    output(8 downto 4)(2) := True
    output(8 downto 4)(2 downto 1) := 2
    output(8 downto 4)(2 downto 1)(1) := False
    output(8 downto 4)(sel) := True

    val outputVec = out Vec(UInt(16 bit),4)
    outputVec := outputVec.getZero
    outputVec(2)(8 downto 4) := 3
    outputVec(sel)(8 downto 4) := 4
    outputVec(sel)(8 downto 4)(2) := False
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}


object PlayMaskAssign {

  class TopLevel extends Component {
    val input = in(UInt(4 bit))
    val output = out(UInt(4 bit))
    output(3 downto 2) := input(1 downto 0)
    output(1 downto 0) := input(3 downto 2)
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}



object PlayClockDomain {

  class TopLevel extends Component {
    val coreClock = Bool
    val coreReset = Bool
    val coreClockDomain = ClockDomain(coreClock,coreReset)
    val coreArea = new ClockingArea(coreClockDomain){
      val coreClockedRegister = Reg(UInt(4 bit))
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}


object PlayExtract {

  class TopLevel extends Component {
    val input = in(UInt(8 bit))
    val output = out(UInt(4 bit))
    output := input(7 downto 1)(4 downto 1)
    output(3 downto 1) := 0
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}

object PlayFsm3 {

  object StateEnum extends SpinalEnum() {
    val sStart, sData, sParity, sStop = newElement()
  }

  class TopLevel extends Component {

    import StateEnum._

    val endData = in Bool()
    val endStop = in Bool()
    val valid = in Bool()
    val ready = out(Reg(Bool()))
    val data = in Bool()
    val tx = out(Reg(Bool()))


    val stateNext = StateEnum()
    val state = RegInit(sStart())

    stateNext := state
    state := stateNext
    ready := False
    switch(state) {
      is(sStart) {
        tx := True
        when(valid) {
          tx := False
          stateNext := sData
        }
      }
      is(sData) {
        tx := data
        when(endData) {
          stateNext := sParity
        }
      }
      is(sParity) {
        tx := !data
        stateNext := sStop
      }
      is(sStop) {
        tx := True
        when(endStop) {
          stateNext := sStart
        }
        ready := True
      }
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}


object PlayCase {
  class TopLevel extends Component {
    val input = in(UInt(4 bit))
    val output = out(UInt(4 bit))

    switch(input) {
      is(0) {
        output := 0
      }
      is(1) {
        output := 1
      }
      is(2) {
        output := 2
      }
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}

object PlayMux2 {

  class TopLevel extends Component {
    val output = out(Mux(in(Bool.setName("sel")),in(SInt(2 bit)),S(0)))
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}

object PlayAttributes {

  class SubComponent extends Component {
    val output = out(True).addAttribute("Yolo")
    val output2 = out(True).addAttribute("Yolo")
  }
  class TopLevel extends Component {
    val output = out(True).addAttribute("Yolo")
    val output2 = out(True).addAttribute("Yolo")

    val sub = new SubComponent()
    val subX = out Bool
    val suby = out Bool

    subX := sub.output
    suby := sub.output2
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}


object PlaySimplify {

  class TopLevel extends Component {
    val o1 = out UInt(4 bits)
    o1 := 0
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}

object PlayError8{

  class Sub extends Component {
    val io = new Bundle{
      val toto = Bool
      val toto2 = Bool
    }
    io.toto2 := io.toto
  }


  class TopLevel extends Component {
    val io = new Bundle{
      val toto = out Bool
    }
    val sub = new Sub

    io.toto := sub.io.toto2
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}


object PlaySel {

  class TopLevel extends Component {
    val a, b, c = in(UInt(4 bit))

    val output = out(Select(U"0000",
      (a > U"1000") -> a,
      (a > U"1100") -> b,
      (a > U"1010") -> c)
    )
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}


object XilinxPatch {
  def apply[T <: Component](c : T) : T = {
    //Patch things
    c.getGroupedIO(true).foreach{
      case axi : AxiLite4 => AxiLite4SpecRenamer(axi)
      case axi : Axi4 => Axi4SpecRenamer(axi)
      case _ =>
    }

    //Builder pattern return the input argument
    c
  }
}


object PlayAxiLite4 {
  class TopLevel extends Component {
    val axiLiteConfig = AxiLite4Config(32, 32)

    val io = new Bundle {
      val input = slave(AxiLite4(axiLiteConfig))
      val output = master(AxiLite4(axiLiteConfig))
    }

    io.input >> io.output
  }

  def main(args: Array[String]) {
    SpinalVerilog(XilinxPatch(new TopLevel))
  }
}


object PlayRam {

  class TopLevel extends Component {
    val mem = Mem(Bits(32 bit),8)

    val address = in(mem.addressType())
    val writeData = in(mem.wordType())
    val writeMask = in Bits(4 bits)
    val enable,write = in Bool()
    val readData = out(mem.readWriteSync(address,writeData,enable,write,writeMask))

    mem.initBigInt((0 until 8).map(i => BigInt((i*0x40302010l)&0xFFFFFFFFl)))
  }

  def main(args: Array[String]) {
    SpinalVerilog(new TopLevel)
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}

object PlayArea {

  class TopLevel extends Component {
    val myArea = new Area{
//      val cmd = slave Stream(wrap(new Bundle{
//        val aaa = Bool
//        val xxx = new Bundle{
//          val yyy = UInt(3 bit)
//        }
//      }))
//      val tmp = cmd.m2sPipe()
//      val rsp = master(tmp.m2sPipe())
      var yolo = 54
      val subArea = new Area{
        var titi = 32
        val subsubArea = new Area {
          val toto = U(2).keep()
        }
      }.setPartialName("chocolat")
    }
  }


  def main(args: Array[String]) {
    SpinalVhdl({
      val c = new TopLevel
      println(c)
      c
    })
    println("Done")
  }
}


object PlayMux4 {
  class TopLevel extends Component {
    val inputs = in Vec(Bool,8)
    val select = in UInt(8 bit)
    val output = out(inputs(select))
  }


  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}

object PlayFunyMux {


  class TopLevel extends Component {


    val sel = in Bool
    val a = in UInt (2 bit)
    val b = in UInt (2 bit)
    val result = sel ? a | b
    out(result)
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}



object PlayBitWidth {
  class TopLevel extends Component {
    val output = out SInt(5 bit)
    output := 8
  }


  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    println("Done")
  }
}


object PlayScala {

  class Entry(val value: Int = (Math.random() * 100000).toInt);

  def main(args: Array[String]): Unit = {
    for (i <- 0 until 10) {
      var startTime = 0l
      var size = 10;
      def start(): Unit = startTime = System.nanoTime()
      def end(message: String): Unit = println((System.nanoTime() - startTime) * 1e-6 + " ms " + message);
      val set = scala.collection.mutable.HashSet[Entry]()
      val dummy = new Entry()

      for (i <- 0 until 10) {
        set += new Entry()
      }
      for (i <- 0 until 20) {
        var idx: Int = 0
        start()
        idx = 0;
        while (idx != 100000) {
          idx += 1
          set.contains(dummy)
        }
        end(" with " + size)
        for (i <- size until size * 2) {
          set += new Entry()
        }
        size *= 2

      }
    }
  }
}



//  abstract class Component protected () extends Registered {
//    registerComponent(this) // Call this for each child that is registered
//    def someCommonMethod() // A method that every child should override
//  }
//
//  object Component {
//    import scala.collection.mutable.MutableList
//
//    // Store all the existing components that are automatically registered
//    var componentLists: MutableList[Component] = MutableList[Component]()
//  }
//
//  trait Registered {
//    protected def registerComponent(c: Component) = {
//      import Component._
//      componentLists += c
//      println("ADD")
//    }
//  }
//
//  class FIFO(val p: Int) extends Component {
//    println("CONST")
//    override def someCommonMethod() = println("I am a FIFO")
//  }
//
//  class RAM(val p: Int) extends Component {
//    println("CONST")
//    override def someCommonMethod() = println("I am a RAM")
//  }
//
//  def main(args: Array[String]) {
//    val f = new FIFO(3)
//    val p = new RAM(3)
//    println(Component.componentLists.mkString(","))
//    Component.componentLists.foreach { x ⇒ x.someCommonMethod() }
//  }
/*
  abstract class CC protected () extends Registered {
    println("Component constructor")
    registerComponent(this) // Call this for each child that is registered

    def someCommonMethod() // A method that every child should override
  }

  object CC {
    import scala.collection.mutable.MutableList

    // Store all the existing components that are automatically registered
    var componentLists: MutableList[CC] = MutableList[CC]()
  }

  trait Registered {
    protected def registerComponent(c: CC) = {
      import CC._
      componentLists += c
      println(s"Something was done after object $c construction" )
    }
  }

  class FIFO(val p: Int) extends CC {
    println("FIFO constructor")
    override def someCommonMethod() = println("I am a FIFO")
  }

  class RAM(val p: Int) extends CC {
    println("RAM constructor")
    override def someCommonMethod() = println("I am a RAM")
  }

  object AutoRegister extends App {
    println("1")
    val f = new FIFO(3)
    println("2")
    val p = new RAM(3)
    println("3")

    // Objects are registered
    println(CC.componentLists.mkString(","))
    // And are from correct type
    CC.componentLists.foreach { x ⇒ x.someCommonMethod() }
  }
*/


