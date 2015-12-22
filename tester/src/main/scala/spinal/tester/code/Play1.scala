package spinal.tester.code

/**
 * Created by PIC32F_USER on 20/09/2015.
 */

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3SlaveController, Apb3Config, Apb3Slave}

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
  controller.readSignal(myReadSignal, 0x10)
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

  val v = Vec(True,True,True)
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

  class PassedImplicits{
    val a = "a"
    val b = "b"
  }

  def execute[T](blockOfCode: PassedImplicits => T): Unit = {
    blockOfCode(new PassedImplicits())
  }




  def f1(str1 : String)(implicit str2 : String): Unit = {
    println(str1 + str2)
  }


  def main(args: Array[String]) {
    implicit val str2 = "asd"
    f1("a ")

    execute{ impl =>
      import impl._
      println(a+b)
    }

    val list = Seq(1,2,3,4,5,6,7,8,9)
    def sum(list : Seq[Int]) : Int = {
      list.size match{
        case 0 => return 0
        case 1 => return list.head
        case _ => {
          val (a,b) = list.splitAt(list.size/2)
          println(a.mkString(",") + " + " + b.mkString(","))
          return sum(a) + sum(b)
        }
      }
    }
    println(sum(list))
  }
}

class BundleBase{

}

class Play5(p : Int) extends Component {
  var cnt = 0
  val stream = new Bundle{ outer =>
    val a = UInt(p bit)
    //def b = print(Play5.this)
    val c = new BundleBase with Cloneable{
      val e = UInt()
      val f = cnt
      cnt += 1
     // def b = print(Play5.this)
      override def clone(): AnyRef = super.clone()
      def clone2() : this.type = clone().asInstanceOf[this.type]
    }
  }



  //val stream = new Bundle{ val a = UInt()}

  println(stream.c.clone2().f)
  println(stream.c.clone2().f)
  println(stream.c.clone2().f)
  println(stream.c.clone2().f)
  println(cnt)


  def toto: Bundle ={
    val x = "yolo"
    val y = new Bundle{
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
      val input = in UInt(4 bit)
      val output = out Bool
    }

    var carry = Bool(false)
    for(bit <- io.input.toBools){
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


  def grayCounter(n : Int, enable : Bool) : UInt = {
    val gray    = RegInit(U(0,n bit))
    var even    = RegInit(True)
    val word    = Cat(True,gray(n-3,0),even)
    when(enable){
      var found = False
      for(i <- 0 until n){
        when(word(i) && !found){
          gray(i) := ! gray(i)
          found \= True
        }
      }
      even := !even
    }
    return gray
  }


  class GrayCounter(n : Int) extends Component{
    val enable = in Bool
    val gray  = out UInt(n bit)

    gray := grayCounter(n,enable)
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

