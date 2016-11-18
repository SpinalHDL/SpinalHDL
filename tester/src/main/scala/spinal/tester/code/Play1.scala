package spinal.tester.code


import java.io.InputStream
import java.util.concurrent.CyclicBarrier

import _root_.com.sun.xml.internal.messaging.saaj.util.{ByteOutputStream, ByteInputStream}
import spinal.core._
import spinal.demo.mandelbrot.{MandelbrotSblDemo, MandelbrotCoreParameters}
import spinal.lib._
import spinal.lib.bus.amba3.apb.{ Apb3Config, Apb3}
import spinal.lib.bus.amba4.axilite.AxiLite4.prot
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.avalon.AvalonMM
import spinal.lib.experimental.bus.sbl.{SblConfig, SblReadRet, SblReadCmd, SblWriteCmd}
import spinal.lib.com.uart._
import spinal.lib.cpu.riscv.impl.build.RiscvAvalon
import spinal.lib.cpu.riscv.impl._
import spinal.lib.cpu.riscv.impl.extension.{DebugExtension, BarrelShifterFullExtension, DivExtension, MulExtension}
import spinal.lib.experimental.MacrosClass
import spinal.lib.graphic.{RgbConfig, Rgb}
import spinal.lib.graphic.vga.{VgaCtrl, Vga}

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

class Play1 extends Component {
  val io = new Bundle {
    val input = slave Stream (new Stage0)
    val output = master Stream (new Stage1)
  }
  //  import scala.language.dynamics;
  //  class Dyna extends Dynamic{
  //    def applyDynamic(name: String)(args: Any*) ={
  //     println(name)
  //    }
  //    def selectDynamic(name: String) = println(name)
  //  }
  //
  //  val dyna = new Dyna
  //  dyna.a__fafafs_asdda__fafaf

  S(2).asUInt
  val b = new Bundle {
    val a = Bool
    val b = Bool
    val c = new Bundle {
      val d = Vec((0 until 3).map(c => Bool))
      val e = new Bundle {
        val f = Vec(Bool, 4)
        val g = Bool
      }
    }
    val h = Bool
  }

  val fName = b.flattenLocalName
  println(fName)

  for ((e, name) <- (b.flatten, b.flattenLocalName).zipped) {
    println(name)
  }

  io.input.translateInto(io.output)((to, from) => {
    to.assignSomeByName(from)
    to.d := False
  })
  io.output.a := False
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

//  override def clone() : this.type = new ComplexBundle
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

object PlayFifo {
  class TopLevel extends Component{
    val fifo = new StreamFifo(Bool,8)
    fifo.io.clone.clone
  }
  def main(args: Array[String]): Unit = {

    SpinalVhdl(new TopLevel())
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
//    }.elsewhen(cond){
//      when(cond){
//        result := a
//      }otherwise{
//        result := e
//      }
//    }.otherwise{
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
    val uintBack = U(ufix)

    val sfix = SFix(7 exp, 12 bit)
    val sint = SInt(3 bit)
    sfix := toSFix(sint)
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


object PlayMux {

  class TopLevel extends Component {
    val sel = in UInt (3 bit)
    val input = in Vec(UInt(8 bit), 7)
    val output = out UInt (8 bit)

    output := input(sel)
//    output := input(0)
//    for (i <- output.range) {
//      if (i != 0) {
//        when(sel === i) {
//          output := input(i)
//        }
//      }
//    }
    //CountOne(Seq(True,True))
    //CountOne(True,True)

  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
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


object PlayBug {

  class TopLevel extends Component {
    val a, b = in UInt (4 bit)
    val result = out UInt (4 bit)
    result := RegNext(a) + b
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}


object PlayOpt {

  class TopLevel extends Component {
    val src0, src1 = in Bits (32 bit)
    val eq  = in Bool
    val signed  = in Bool
    val result = out Bool



  //  br_result := Mux(br_eq,(src0 === src1),Mux(br_signed,(src0.asSInt < src1.asSInt),(src0.asUInt < src1.asUInt)))


    val br_src0 = (src0.msb && signed) ## src0
    val br_src1 = (src1.msb && signed) ## src1
    result :=  Mux(eq,(src0 === src1),(br_src0.asUInt-br_src1.asUInt).msb)
//    val src0, src1 = in Bits (32 bit)
//    //val br_eq  = out Bool
//    val br_signed  = in Bool
//    val br_result  = out Bool
//
//
//


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
    }.elsewhen(a > 3) {
      result(0) := True
      when(a > 50) {
        result := 5
      }
      result(2) := False
    }.otherwise {
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
    result := 1
    switch(a) {
      is(1) {

      }
      is(2) {

      }
      is(3) {

      }
      is(4) {
        result := 2
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


    val tmp = Reg(MyEnum(binarySequancial))

    tmp := MyEnum.s3
    when(input === MyEnum.s4) {
      tmp := MyEnum.s7
    }
    when(input === MyEnum.s5) {
      tmp := Mux(cond, MyEnum.s6(binarySequancial), MyEnum.s8(binarySequancial))
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

    }.otherwise{
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
    }.elsewhen(input === M"00001111----------------0001----"){
      output := 1
    }
      .elsewhen(input === M"00001111----------------0010----"){
      output := 2
    }
      .elsewhen(input === M"00001111----------------0011----"){
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


        val source = slave Stream (wrap(new Bundle{
          val a = Bool
        }))
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


object OverloadPlay {

  class OverloadPlay(frameAddressOffset: Int, p: MandelbrotCoreParameters, coreClk: ClockDomain, vgaMemoryClk: ClockDomain, vgaClk: ClockDomain) extends Component {
    for (i <- 0 until 10) {
      val memoryBusConfig = SblConfig(30, 32)
      val rgbType = RgbConfig(8, 8, 8)

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
    //    Console.in.read

    for (i <- 0 until 1) {
      val report = SpinalVhdl({
        val vgaClock = ClockDomain.external("vga")
        val vgaMemoryClock = ClockDomain.external("vgaMemory")
        val coreClock = ClockDomain.external("core",frequency = FixedFrequency(100e6))
        new OverloadPlay(0, new MandelbrotCoreParameters(256, 64, 640, 480, 7, 17 * 3), coreClock, vgaMemoryClock, vgaClock)
      })
      // Console.in.read
      println(report.toplevel)
      var entries = 0

      val c = ArrayBuffer().getClass()
      val f = c.getDeclaredField("array")
      f.setAccessible(true)
      Node.walk(report.toplevel.getAllIo.toSeq, node => {
        entries += node.getInputs.size
      })

      println(entries)
            while(true){
              Thread.sleep(1000)
              println(report.toplevel )
            }
    }
  }
}

object OverloadPlay2 {

  class OverloadPlay2 extends Component {
    for (i <- 0 until 100) {
      //replace wit null to disable instruction cache
      val iCacheConfig = InstructionCacheConfig(
        cacheSize =4096,
        bytePerLine =32,
        wayCount = 1,
        wrappedMemAccess = true,
        addressWidth = 32,
        cpuDataWidth = 32,
        memDataWidth = 32
      )

      //replace wit null to disable data cache
      val dCacheConfig = DataCacheConfig(
        cacheSize = 4096,
        bytePerLine =32,
        wayCount = 1,
        addressWidth = 32,
        cpuDataWidth = 32,
        memDataWidth = 32
      )

      val coreConfig = CoreConfig(
        pcWidth = 32,
        addrWidth = 32,
        startAddress = 0x200,
        regFileReadyKind = sync,
        branchPrediction = dynamic,
        bypassExecute0 = true,
        bypassExecute1 = true,
        bypassWriteBack = true,
        bypassWriteBackBuffer = true,
        collapseBubble = false,
        fastFetchCmdPcCalculation = true,
        dynamicBranchPredictorCacheSizeLog2 = 7
      )

      coreConfig.add(new MulExtension)
      coreConfig.add(new DivExtension)
      coreConfig.add(new BarrelShifterFullExtension)
      //  p.add(new BarrelShifterLightExtension)


      val core = new RiscvAvalon(coreConfig,iCacheConfig,dCacheConfig,true,4)
      val io = core.io.clone
      io <> core.io
      for((a,b) <- (io.flatten,core.io.flatten).zipped)if(b.isInput)
        a.asInput
      else
        a.asOutput
    }
  }

  def main(args: Array[String]): Unit = {
    //    Console.in.read

    for (i <- 0 until 1) {
      val report = SpinalVhdl(new OverloadPlay2)
      // Console.in.read
      println(report.toplevel)
      var entries = 0
      var spinalTagSet = 0

      val c = ArrayBuffer().getClass()
      val f = c.getDeclaredField("array")
      f.setAccessible(true)
      Node.walk(report.toplevel.getAllIo.toSeq, node => {
        entries += node.getInputs.size
        if(node._spinalTags != null) spinalTagSet += 1
      })

      println(entries)
      println(spinalTagSet)
      while(true){
        Thread.sleep(1000)
        println(report.toplevel )
      }
    }
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

object RIntPlay {

  class TopLevel extends Component {
    val a, b = in(RInt(max = 15, min = (-2)))
    val c = out(a + b)
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


object PlayMacro {
  import spinal.core.MacroTest._

  class TopLevel extends Component {
    val e = enum('s1, 's2, 's3)

    import e._

    val e2 = enum('s1, 's2, 's3)

    import e2._


    println("ASD3")
    out(True)

    val s = e()
    s := e.s1
    out(s)
    val s2 = e2()
    s2 := e2.s1
    out(s2)
  }

  def main(args: Array[String]) {
    //createEnum("asd")
    val a = bar("toto")
    println(a.asd)


    SpinalVhdl(new TopLevel)
    println("Done")
  }
}

object PlayMacroLib {
  import spinal.core.MacroTest._

  class TopLevel extends Component {
   
  }

  def main(args: Array[String]) {
    var titi = 2
    val a = new MacrosClass
    val x = a.doit("asd")
    print(x(2))
    
    val y = a.doit2((x : Int) => x + 1) 
    print(y(2))
   
    val z = a.doit3((x : Int) => x + 1) 
    print(z(2))
   
  }
}

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

object PlayCombLoop {

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



object PlayCombLoop2 {

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

//TODO switch test comb loop and uncomplet assignement, maybe switchnode should extend multiple assignement node ?
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
    output(3, 2) := input(1, 0)
    output(1, 0) := input(3, 2)
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

    val output = out(Sel(U"0000",
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

object PlayAxiLite4 {
  class TopLevel extends Component {
    val axiLiteConfig = AxiLite4Config(32, 32)
    val peon   = slave(AxiLite4(axiLiteConfig))
    val maitre = master(AxiLite4(axiLiteConfig))
    peon >> maitre
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}


object PlayRam {

  class TopLevel extends Component {
    val mem = Mem(Bits(32 bit),128)

    val wr = in(mem.writePort)

    val addr = RegNextWhen(in(mem.addressType),in Bool)
    val data = out (mem(addr))

  }

  def main(args: Array[String]) {
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


object PlayVec8 {
  class TopLevel extends Component {
    val inputs = List.fill(4)(List.fill(4)(wrap(new Bundle{
      val a = in Bool
      val b = wrap(new Bundle{
        val c = in(Vec(wrap(new Bundle{
          val d = in Bool
        }),4))
      })
    })))
    val select0 = in UInt(2 bit)
    val select1 = in UInt(2 bit)
    val select2 = in UInt(2 bit)
    val vec = Vec(inputs.map(a => Vec(a.map(b => b.b))))

    val output = out Bool()
    output := vec(select0)(select1).c(select2).d
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