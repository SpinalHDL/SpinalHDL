package spinal.dev

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import spinal.core._
import spinal.idslplugin.{PostInitCallback, ValCallback}
//
////import scala.annotation.StaticAnnotation
////
////class postInitCallback extends StaticAnnotation
////class valCallback extends StaticAnnotation
////@valCallback @postInitCallback


//class Bundle(val v : String) extends PostInitCallback{
//  println("Bundle constructor start")
//  def postInitCallback(): this.type = {
//    println("miaou")
//    this
//  }
//  def this(v : Int) {
//    this((v*v).toString)
//    println(v)
//  }
//  println("Bundle constructor end")
//}
//
//object Play {
//  def main(args: Array[String]): Unit = {
//    println("START")
//    new Bundle(2)
//    println("DONE")
//  }
//}

//class RGB2 extends Bundle {
//  println("A")
//  def this(v : Int) {
//    this()
//    println("B")
//  }
//  new ArrayBuffer[Int]()
//  class Yolo(){
//    println(RGB2.this)
//  }
//
//  new Yolo()
//}
//
class Bundle extends ValCallback with PostInitCallback{
  println("Bundle constructor start")
//  def valCallback(ref: Any, name: String): Unit = {
//    println(ref.getClass + " : " + name + " = " + ref)
//  }
  def postInitCallback(): this.type = {
    println("miaou")
    this
  }

  override def valCallback[T](ref: T, name: String): T = {
    println(ref.getClass + " :: " + name + " = " + ref)
    ref
  }

  val miaou = "wuff"
  println("Bundle constructor enda")
}

class RGB(val v : String) extends Bundle {

//  new RGB2()
  def this(v : Int) {
    this((v*v).toString)
    println(v)
  }

  println("RGB constructor start3     ")
  val r = 3
  @dontName val g = 4
  val b = 5
  println("hello")
  val x, y, z = 44
  var t = 3
  t = 4
  println("RGB constructor endaaaaa")
}

object Play {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Component{
      val a = Bool()
      val b = UInt(2 bits)
      val c = Bits(5 bits)

      val raw = B"00001011"

//      Vec(a,b,c).assignFromBits(raw)
    })
  }
}

object Play2 extends App{
  import spinal.core.sim._
  SpinalVerilog(new Component{

    val a,b = Bool
    val c = Bits(4 bits)
    (a,b,c) := B"111100"
    val x = (a,b,c).asBits

    val e,f,g = Bool()
    (e,f,g) := B(a,b,c).resized

  })
}

object Play43 extends App{
  import spinal.core.sim._
  SimConfig.compile(new Component{
    val counter = Reg(UInt(8 bits)) init(0)
    when(counter =/= 100){
      counter := counter + 1
    }
    when(counter === 80){
      report(L"miaou 0x$counter")
    }
  }).doSim{dut =>
    println("hello")
    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitSampling(10000000)
    println("goodbye")
  }
}

object playBB extends App{
  class MyBlackBox extends BlackBox {
    val io = new Bundle {
      val din = in Bits (8 bit)
      val dout = out Bits (8 bit)
    }

    io.dout := io.din
  }

  class Bypass extends Component {
    val io = new Bundle {
      val din = in Bits (8 bit)
      val dout = out Bits (8 bit)
    }

    io.dout := io.din
  }

  class Top extends Component{
    val io = new Bundle{
      val din = in Bits(8 bit)
      val dout = out Bits(8 bit)
    }

    val uut1 = new MyBlackBox
    val uut2 = new Bypass

    uut1.io.din := io.din
    uut2.io.din := io.din

    io.dout := uut1.io.dout | uut2.io.dout
  }
  SpinalConfig(globalPrefix = "jj_").generateVerilog(new Top)
}

//object Play2 extends App{
//  class Toplevel extends Component{
//    val io = new Bundle {
//      val a,b = in Bool
//    }
//  }
//}




//class CompilerPluginTest extends Bundle{
////  val x = new Area {
////    var sample: Bool = null
////    val s2 = new Area {
////      sample = True
////
////    }
////  }
//
//  val publicElement = Bool()
//  private val privateElement = valCallback2(Bool(), "asd")
//
//  def setPrivateElementByAnAbstractWay(trigger : UInt) : Unit = {
//    when(trigger > 10) {
//      privateElement := True
//
//    }
//  }
//}




//
//class SubAA extends Component{
//  val a,b = in Bool()
//  val result = out Bool()
//  result := a && b
//}
//
//case class SubA[T <: Any](v : T) extends Component{
//  val a,b = in Bool()
//  val result = out Bool()
//  val c2 = new SubAA()
//  c2.a <> a
//  c2.b <> b
//  c2.result <> result
//  println(v + v.toString)
//}
//
//class Toplevel extends Component{
//  val io = new Bundle {
//    val a, b = in Bool()
//    val result = out Bool()
//  }
//  val c1 = new SubA("miaou")
//  c1.a <> io.a
//  c1.b <> io.b
//  c1.result <> io.result
//}
//object PlayComponent2 extends App{
//  SpinalVerilog(new Toplevel)
//}

//object PlayBits{
//
//  class TopLevel extends Component {
////    val a, b, c = in Bits(8 bits)
////    val d, e, f = out Bool()
////    val g, h, i, j = Bits(8 bits)
////    val x,y,z = out( Reg(Bits(8 bits)))
////
////    val bits4 = Bits(4 bits)
////
////    class Struct extends Bundle{
////      val a,b,c = Bits(32 bits)
////    }
////
////    val structA, structB = new Struct
////
////    structA.a := 1
////    structB := structA
////    val tmp = a & b & c
////    val xx = g & a
////    i := h
////    x := x & a
////    bits4 := a(3 downto 0)
////    bits4 := (a(5 downto 0) & b(5 downto 0))(3 downto 0)
////
////    g(3 downto 0) := a
////    g(2 downto 0) := a
////    g(7 downto 3)(3 downto 1)(1 downto 1) := a
////
////    val tmp2 = Bits()
////    tmp2(3 downto 0) := a(1 downto 0)
////    tmp2(5 downto 2) := a(5 downto 2)
////
////    val miaou = a.resize(2)
////    miaou := b.resize(4)
////    miaou := c.resize(3)
////    val sel = in UInt(6 bits)
////    a & c & b
////    True
////    False
////    val yy = bits4.resized & c.resized
////    bits4 := a.resized
////    bits4 := (a & b).resized
////    bits4 := B"111".resized
////    bits4 := (B(0) & B(0)).resized
////
////
////    True <>  x(0)
////
////    val aa = a & B(0).resized
////    val bb = B(0).resized & a
////    val cc = B"11".resized & a
////    a & b & c
////
////
////    var ano = False
////    ano := ano & True & False & ano
////    ano = null
////
////    bits4 := B"101010".resized
////    bits4(3 downto 1)(2) := in.Bool()
////    x(2) := True
////    x := a
////    x := (bits4 | bits4).resized
////    when(a === b){
////      x init(c)
////      bits4 := 0
////      bits4(sel) := Mux(c === a, a(2), False)
////    }
//
//    val a = in Bits(8 bits)
//    var v = B"00000000"
////    e := False
//    v \= a
//    when(a === 0){
//      v \= 1
//    }
//    when(a === 1){
//      v \= 2
//    }
//    when(a === 2){
//      v \= 3
//      when(a === 3){
//        v \= 4
//      }
//    }
//    when(a === 4){
//      v \= 5
//    }
//    v \= 6
//    when(a === 5){
//      v \= 7
//    }
//
////    bits4 := (2 -> True, default -> (c === c))
//
//
////    when(a === c){
////      bits4 := 0
////      x := 0
////    } otherwise {
////      bits4 := 1
////      e := True
////      x := 1
////    }
//  }
//
//  def main(args: Array[String]) {
//    val toplevel = SpinalVhdl(new TopLevel()).toplevel
//
//  }
//}
//
//
//object State extends SpinalEnum{
//  val A,B,C,D = newElement()
//}
//
//object PlayEnum{
//
//
//  class TopLevel extends Component {
//
//    val input = in(State(binaryOneHot))
//    val cond = out Bool()
//
//    cond := False
//    when(input === State.C){
//      cond := True
//    }
//
//  }
//
//  def main(args: Array[String]) {
//    SpinalVerilog(new TopLevel())
//    SpinalVhdl(new TopLevel())
//
//  }
//}
//
//
//object PlayPoison{
//  class TopLevel extends Component {
//
//    val a = Bits(8 bits)
//    a.assignDontCare()
//    val b = Bool().assignDontCare()
//    val c = State().assignDontCare()
//   // val d = State(binaryOneHot).assignDontCare()
//  }
//
//  def main(args: Array[String]) {
//    val toplevel = SpinalVhdl(new TopLevel()).toplevel
//
//  }
//}
//
//object PlayMem{
//  class TopLevel extends Component {
//    val mem = Mem(Bits(32 bits), 256)
//    val readAsyncAddress = in UInt(8 bits)
//    val readAsyncData = out Bits(32 bits)
//    readAsyncData := mem.readAsync(readAsyncAddress)
//  }
//
//  def main(args: Array[String]) {
//    val toplevel = SpinalVhdl(new TopLevel()).toplevel
//
//  }
//}
//
//
//object PlayReg{
//
//  class TopLevel extends Component {
//    val a, b, c = in Bool()
//    val d, e, f = out Bool()
//    val g, h, i, j = Bool()
//    val x,y,z = Reg(Bool())
//
//    val l,m,n = Reg(Bool)
////    n init(False)
////    l := a
//    when(a || b || c) {
//      m := a || b  || a || b
//    }
////    n := m || l
//  }
//
//  def main(args: Array[String]) {
//    val toplevel = SpinalVhdl(new TopLevel()).toplevel
//
//  }
//}
//
//
//
//object PlaySimple{
//
//  class TopLevel extends Component {
//    val a, b, c = in Bool()
//    val d, e, f = out Bool()
//    val g, h, i, j = Bool()
//    val x,y,z = Reg(Bool())
//    val sel = Bits(4 bits)
//
//
//    when(a){
//      d := b || c
//      switch(sel){
//        default{
//          d := c
//        }
//        is(B"0000"){
//          d := c || c
//          d := g || g
//        }
//        is(B"0001"){
//          d := h || h
//        }
//        is(B"0010"){
//          d := i || i
//          d := j || j
//        }
//      }
//    }
//
//    val l,m,n,o, p, q, r,s = Bits(4 bits)
//    l := ~(m | (n & (o ## (p ^ q)).resized))
//    e := (l === n) && (o =/= q)
//
////    g := False
////    when(a || b && c){
////      g := True
////      h := True
////      x := True
////    }
//
////    when(c || c && c){
////      f := True
////      f := False
////    }
//
//
////    ~a
////    d := ~a
////    d := ((a || b && c ^ (~g)) === True) =/= False
//
//
////    def useless(): Unit ={
////      a || b || c
////      c || b
////      True
////      False
////    }
////
////    useless()
//  }
//
//  def main(args: Array[String]) {
//    val toplevel = SpinalVhdl(new TopLevel()).toplevel
//
//  }
//}
//
//
//object PlayScope{
//  class SubSubA extends Component{
//    val a,b = in Bool()
//    val result = out Bool()
//    val temp = RegNext(a || b)
//    result := temp
//  }
//  class SubA extends Component{
//    val a,b = in Bool()
//    val result = out Bool()
//    val subSubA = new SubSubA()
//    subSubA.a := a
//    subSubA.b := b
//    assert(a,"MIAOU")
//    result := subSubA.result
//  }
//  class TopLevel extends Component {
//    val a, b, c = in Bool()
//    val d, e, f = out Bool()
//    val g, h, i, j = Bool()
//    val x,y,z = Reg(Bool())
//    d := e
//    d := a
//    y.init(True)
//    when(a){
//      z init(False)
//    }
//    h := True
//    e := a || c
//    x := d || y
//    y := b
//
//    when(c) {
//      e := d
//      when(d) {
//        assert(b,"MIAOU")
//        f := e
//        e := f
//      }
//      e := f
//    }elsewhen(a) {
//      val x = Bool()
//      val y = Reg(Bool())
//      x := a || b
//      y := True
//      i := g || x
//      z := False
//    } otherwise {
//      d := j
//    }
//
//    when(a){
//      i := c
//    }
//
//    when(b){
//      d := False
//    }
//
//    when(c) {
//      val subA = new SubA()
//      subA.a := x
//      subA.b := j
//      subA.b := y || z
//      i := subA.result
//
//      d := subA.a
//    }
//
//    val l,m,n = Reg(Bool)
//    l := a
//    m := a || b
//    n := m || l
//  }
//
//  def main(args: Array[String]) {
//    val toplevel = SpinalVhdl(new TopLevel()).toplevel
//
//    var statementCount, expressionCount = 0
//    toplevel.dslBody.walkStatements(s => {
//      statementCount += 1
//      s.walkExpression(e => {
//        expressionCount += 1
//      })
//    })
//    print("DONE " + toplevel.getName() + " " + statementCount + " " + expressionCount)
//  }
//}
//
//
//object PlayComponent{
//  class SubA extends Component{
//    val a = in Bool()
//    val result = out Bool()
//
//    result := a
//  }
//
//  class SubB extends Component{
//    val a,b = in Bool()
//    val result = out Bool()
//
//    val subBA = new SubBA
//    subBA.a := a || a
//    subBA.b := b
//
//    result := a || b || subBA.result
//
////    val yolo = in(Reg(Bool()))
////    val io = out Bool()
////    subBA.result := a
////    a := b
////    a := result
////    a := True
//  }
//
//  class SubBA extends BlackBox{
//    val generic = new Generic{
//      val x = 2
//      val y = "asd"
//      val z = True
//      val xx = B"1100"
//    }
//
//    val a,b = in Bool()
//    val result = out Bool()
//
//  }
//
//
//  class TopLevel extends Component {
//    val a, b = in Bool()
//    val result = out Bool()
//
//    val subA = new SubA
//    subA.a := a
//
//    val subB = new SubB
//    subB.a := subA.result
//    subB.b := b
//
//    result := subB.result
//  }
//
//  def main(args: Array[String]) {
//    val toplevel = SpinalVhdl(new TopLevel()).toplevel
//
//    var statementCount, expressionCount = 0
//    toplevel.dslBody.walkStatements(s => {
//      statementCount += 1
//      s.walkExpression(e => {
//        expressionCount += 1
//      })
//    })
//    print("DONE " + toplevel.getName() + " " + statementCount + " " + expressionCount)
//  }
//}
//
//object PlayHeavyload {
//  new mutable.MutableList[Any]
//  class Logic extends Area{
//    val a, b, c, d, e, f, g, h, i, j = Bool()
//    val x,y,z = Reg(Bool())
//    b := True
//    a := b || c
//    x := d || y
//    c := b
//
//    when(c) {
//       e := d
//       when(d) {
//         f := e
//         e := f
//       }
//       b := f
//    }elsewhen(a) {
//      val x = Bool()
//      x := a || b
//      i := g || x
//    } otherwise {
//      b := j
//    }
//    setWeakName("asd")
//  }
//
//
//  class TopLevel(size : Int) extends Component{
//    var l = ArrayBuffer[Logic]()
//    l.sizeHint(1100000)
//    SpinalProgress("TOP START")
//    var idx = 0
//    while(idx < size) {
//      idx += 1
//      l += new Logic
//      if(idx % 10000 == 0) println(idx)
//    }
//    l = null
//    SpinalProgress("TOP END")
//  }
//
//  def main(args: Array[String]) {
//    def printInfo(toplevel : Component): Unit ={
//      var statementCount, expressionCount = 0
//      toplevel.dslBody.walkStatements(s => {
//        statementCount += 1
//        s.walkExpression(e => {
//          expressionCount += 1
//        })
//      })
//      print("DONE " + toplevel.getName() + " " + statementCount + " " + expressionCount)
//    }
//    printInfo(SpinalVhdl(new TopLevel(50000)).toplevel)
////    printInfo(SpinalVhdl(new TopLevel(1)).toplevel)
////    printInfo(SpinalVhdl(new TopLevel(1)).toplevel)
//
//
//  }
//}
//
//
//
//
//object PlayDebug{
//  object Miaou{
//    def unapply(yolo : Yolo) : Option[Int] = yolo.x match {
//      case v : Int => Some(v)
//      case _ => None
//    }
//  }
//
//  case class Yolo(var a : Any){
//    val x = a
//    println(x)
//    def getX = x
//  }
//  trait A { def key : Int}
//  case class A0 (x : Int) extends A {def key = 0 }
//  case class A1 (x : Int) extends A {def key = 1 }
//  case class A2 (x : Int) extends A {def key = 2 }
//  case class A3 (x : Int) extends A {def key = 3 }
//  case class A4 (x : Int) extends A {def key = 4 }
//  case class A5 (x : Int) extends A {def key = 5 }
//  case class A6 (x : Int) extends A {def key = 6 }
//  case class A7 (x : Int) extends A {def key = 7 }
//  case class A8 (x : Int) extends A {def key = 8 }
//  case class A9 (x : Int) extends A {def key = 9 }
//  case class A10(x : Int) extends A {def key = 10}
//  case class A11(x : Int) extends A {def key = 11}
//  case class A12(x : Int) extends A {def key = 12}
//  case class A13(x : Int) extends A {def key = 13}
//  case class A14(x : Int) extends A {def key = 14}
//  case class A15(x : Int) extends A {def key = 15}
//  case class A16(x : Int) extends A {def key = 16}
//  case class A17(x : Int) extends A {def key = 17}
//  case class A18(x : Int) extends A {def key = 18}
//  case class A19(x : Int) extends A {def key = 19}
//  case class A20(x : Int) extends A {def key = 20}
//  case class A21(x : Int) extends A {def key = 21}
//  case class A22(x : Int) extends A {def key = 22}
//  case class A23(x : Int) extends A {def key = 23}
//  case class A24(x : Int) extends A {def key = 24}
//  case class A25(x : Int) extends A {def key = 25}
//  case class A26(x : Int) extends A {def key = 26}
//  case class A27(x : Int) extends A {def key = 27}
//  case class A28(x : Int) extends A {def key = 28}
//  case class A29(x : Int) extends A {def key = 29}
//  case class A30(x : Int) extends A {def key = 30}
//  case class A31(x : Int) extends A {def key = 31}
//  case class A32(x : Int) extends A {def key = 32}
//  case class A33(x : Int) extends A {def key = 33}
//  case class A34(x : Int) extends A {def key = 34}
//  case class A35(x : Int) extends A {def key = 35}
//  case class A36(x : Int) extends A {def key = 36}
//  case class A37(x : Int) extends A {def key = 37}
//  case class A38(x : Int) extends A {def key = 38}
//  case class A39(x : Int) extends A {def key = 39}
//  case class A100 (x : Int) extends A {def key = 40 }
//  case class A101 (x : Int) extends A {def key = 41 }
//  case class A102 (x : Int) extends A {def key = 42 }
//  case class A103 (x : Int) extends A {def key = 43 }
//  case class A104 (x : Int) extends A {def key = 44 }
//  case class A105 (x : Int) extends A {def key = 45 }
//  case class A106 (x : Int) extends A {def key = 46 }
//  case class A107 (x : Int) extends A {def key = 47 }
//  case class A108 (x : Int) extends A {def key = 48 }
//  case class A109 (x : Int) extends A {def key = 49 }
//  case class A110(x : Int) extends A {def key = 50}
//  case class A111(x : Int) extends A {def key = 51}
//  case class A112(x : Int) extends A {def key = 52}
//  case class A113(x : Int) extends A {def key = 53}
//  case class A114(x : Int) extends A {def key = 54}
//  case class A115(x : Int) extends A {def key = 55}
//  case class A116(x : Int) extends A {def key = 56}
//  case class A117(x : Int) extends A {def key = 57}
//  case class A118(x : Int) extends A {def key = 58}
//  case class A119(x : Int) extends A {def key = 59}
//  case class A120(x : Int) extends A {def key = 60}
//  case class A121(x : Int) extends A {def key = 61}
//  case class A122(x : Int) extends A {def key = 62}
//  case class A123(x : Int) extends A {def key = 63}
//  case class A124(x : Int) extends A {def key = 64}
//  case class A125(x : Int) extends A {def key = 65}
//  case class A126(x : Int) extends A {def key = 66}
//  case class A127(x : Int) extends A {def key = 67}
//  case class A128(x : Int) extends A {def key = 68}
//  case class A129(x : Int) extends A {def key = 69}
//  case class A130(x : Int) extends A {def key = 70}
//  case class A131(x : Int) extends A {def key = 71}
//  case class A132(x : Int) extends A {def key = 72}
//  case class A133(x : Int) extends A {def key = 73}
//  case class A134(x : Int) extends A {def key = 74}
//  case class A135(x : Int) extends A {def key = 75}
//  case class A136(x : Int) extends A {def key = 76}
//  case class A137(x : Int) extends A {def key = 77}
//  case class A138(x : Int) extends A {def key = 78}
//  case class A139(x : Int) extends A {def key = 79}
//
//
//  def main(args: Array[String]) {
//    val l : List[Any] = List(1,2,"asd", Yolo("miaou"), Yolo(1), Yolo(1), Yolo(Yolo(1)), Yolo(Yolo(1)))
//    val l2 : List[A] = scala.util.Random.shuffle(List(
//      A0 (1),
//      A1 (1),
//      A2 (1),
//      A3 (1),
//      A4 (1),
//      A5 (1),
//      A6 (1),
//      A7 (1),
//      A8 (1),
//      A9 (1),
//      A10(1),
//      A11(1),
//      A12(1),
//      A13(1),
//      A14(1),
//      A15(1),
//      A16(1),
//      A17(1),
//      A18(1),
//      A19(1),
//      A20(1),
//      A21(1),
//      A22(1),
//      A23(1),
//      A24(1),
//      A25(1),
//      A26(1),
//      A27(1),
//      A28(1),
//      A29(1),
//      A30(1),
//      A31(1),
//      A32(1),
//      A33(1),
//      A34(1),
//      A35(1),
//      A36(1),
//      A37(1),
//      A38(1),
//      A39(1),
//      A101 (1),
//      A102 (1),
//      A103 (1),
//      A104 (1),
//      A105 (1),
//      A106 (1),
//      A107 (1),
//      A108 (1),
//      A109 (1),
//      A110(1),
//      A111(1),
//      A112(1),
//      A113(1),
//      A114(1),
//      A115(1),
//      A116(1),
//      A117(1),
//      A118(1),
//      A119(1),
//      A120(1),
//      A121(1),
//      A122(1),
//      A123(1),
//      A124(1),
//      A125(1),
//      A126(1),
//      A127(1),
//      A128(1),
//      A129(1),
//      A130(1),
//      A131(1),
//      A132(1),
//      A133(1),
//      A134(1),
//      A135(1),
//      A136(1),
//      A137(1),
//      A138(1),
//      A139(1)))
//
//    val dic = mutable.HashMap[Class[_ <: A], (A) => Int]()
//    l2.foreach(e => dic(e.getClass) = e => e.key)
//    val dic2 = new Array[(A) => Int](l2.length+1)
//    l2.foreach(e => dic2(e.key) = e => e.key)
//    l2.foreach(e => println(Integer.toHexString(e.getClass.hashCode())))
//    println("START B")
//    for(i <- 0 until 5) {
//      {
//        val startAt = System.nanoTime()
//        var count = 0
//        var idx = 0
//        while (idx < 500000) {
//          idx += 1
//          l2.foreach(e => count += dic(e.getClass)(e))
//        }
//        val endAt = System.nanoTime()
//        println(s"${(endAt - startAt) / 1e6}      $count")
//      }
//    }
//
//    println("START 0")
//    for(i <- 0 until 5) {
//      {
//        val startAt = System.nanoTime()
//        var count = 0
//        var idx = 0
//        while (idx < 500000) {
//          idx += 1
//          l2.foreach(e => count += dic2(e.key)(e))
//        }
//        val endAt = System.nanoTime()
//        println(s"${(endAt - startAt) / 1e6}      $count")
//      }
//    }
//
//    println("START A")
//    for(i <- 0 until 5) {
//      {
//        val startAt = System.nanoTime()
//        var count = 0
//        var idx = 0
//        while (idx < 500000) {
//          idx += 1
//          l2.foreach(_ match {
//            case A0 (x : Int) => count += x + 0
//            case A1 (x : Int) => count += x + 1
//            case A2 (x : Int) => count += x + 2
//            case A3 (x : Int) => count += x + 3
//            case A4 (x : Int) => count += x + 4
//            case A5 (x : Int) => count += x + 5
//            case A6 (x : Int) => count += x + 6
//            case A7 (x : Int) => count += x + 7
//            case A8 (x : Int) => count += x + 8
//            case A9 (x : Int) => count += x + 9
//            case A10(x : Int) => count += x + 10
//            case A11(x : Int) => count += x + 11
//            case A12(x : Int) => count += x + 12
//            case A13(x : Int) => count += x + 13
//            case A14(x : Int) => count += x + 14
//            case A15(x : Int) => count += x + 15
//            case A16(x : Int) => count += x + 16
//            case A17(x : Int) => count += x + 17
//            case A18(x : Int) => count += x + 18
//            case A19(x : Int) => count += x + 19
//            case A20(x : Int) => count += x + 20
//            case A21(x : Int) => count += x + 21
//            case A22(x : Int) => count += x + 22
//            case A23(x : Int) => count += x + 23
//            case A24(x : Int) => count += x + 24
//            case A25(x : Int) => count += x + 25
//            case A26(x : Int) => count += x + 26
//            case A27(x : Int) => count += x + 27
//            case A28(x : Int) => count += x + 28
//            case A29(x : Int) => count += x + 29
//            case A30(x : Int) => count += x + 30
//            case A31(x : Int) => count += x + 31
//            case A32(x : Int) => count += x + 32
//            case A33(x : Int) => count += x + 33
//            case A34(x : Int) => count += x + 34
//            case A35(x : Int) => count += x + 35
//            case A36(x : Int) => count += x + 36
//            case A37(x : Int) => count += x + 37
//            case A38(x : Int) => count += x + 38
//            case A39(x : Int) => count += x + 39
//            case A100 (x : Int) => count += x + 0
//            case A101 (x : Int) => count += x + 1
//            case A102 (x : Int) => count += x + 2
//            case A103 (x : Int) => count += x + 3
//            case A104 (x : Int) => count += x + 4
//            case A105 (x : Int) => count += x + 5
//            case A106 (x : Int) => count += x + 6
//            case A107 (x : Int) => count += x + 7
//            case A108 (x : Int) => count += x + 8
//            case A109 (x : Int) => count += x + 9
//            case A110(x : Int) => count += x + 10
//            case A111(x : Int) => count += x + 11
//            case A112(x : Int) => count += x + 12
//            case A113(x : Int) => count += x + 13
//            case A114(x : Int) => count += x + 14
//            case A115(x : Int) => count += x + 15
//            case A116(x : Int) => count += x + 16
//            case A117(x : Int) => count += x + 17
//            case A118(x : Int) => count += x + 18
//            case A119(x : Int) => count += x + 19
//            case A120(x : Int) => count += x + 20
//            case A121(x : Int) => count += x + 21
//            case A122(x : Int) => count += x + 22
//            case A123(x : Int) => count += x + 23
//            case A124(x : Int) => count += x + 24
//            case A125(x : Int) => count += x + 25
//            case A126(x : Int) => count += x + 26
//            case A127(x : Int) => count += x + 27
//            case A128(x : Int) => count += x + 28
//            case A129(x : Int) => count += x + 29
//            case A130(x : Int) => count += x + 30
//            case A131(x : Int) => count += x + 31
//            case A132(x : Int) => count += x + 32
//            case A133(x : Int) => count += x + 33
//            case A134(x : Int) => count += x + 34
//            case A135(x : Int) => count += x + 35
//            case A136(x : Int) => count += x + 36
//            case A137(x : Int) => count += x + 37
//            case A138(x : Int) => count += x + 38
//            case A139(x : Int) => count += x + 39
//
//            case _ =>
//          })
//        }
//        val endAt = System.nanoTime()
//        println(s"${(endAt - startAt) / 1e6}      $count")
//      }
//    }
//
//
//
///*
//    println("START A")
//    for(i <- 0 until 10) {
//      {
//        val startAt = System.nanoTime()
//        var count = 0
//        var idx = 0
//        while (idx < 5000000) {
//          idx += 1
//          l.foreach(_ match {
//            case Yolo(x: Int) => count += x
//            case _ =>
//          })
//        }
//        val endAt = System.nanoTime()
//        println(s"${(endAt - startAt) / 1e6}      $count")
//      }
//    }
//    println("START B")
//
//    for(i <- 0 until 5) {
//      {
//        val startAt = System.nanoTime()
//        var count = 0
//        var idx = 0
//        while (idx < 5000000) {
//          idx += 1
//          l.foreach(_ match {
//            case yolo : Yolo => if(yolo.a.isInstanceOf[Int]) count += yolo.a.asInstanceOf[Int]
//            case _ =>
//          })
//        }
//        val endAt = System.nanoTime()
//        println(s"${(endAt - startAt) / 1e6}      $count")
//      }
//    }
//
//    println("START C")
//
//    for(i <- 0 until 5) {
//      {
//        val startAt = System.nanoTime()
//        var count = 0
//        var idx = 0
//        while (idx < 5000000) {
//          idx += 1
//          l.foreach(_ match {
//            case yolo : Yolo =>  yolo.a match {
//              case i : Int => count += i
//              case _ =>
//            }
//            case _ =>
//          })
//        }
//        val endAt = System.nanoTime()
//        println(s"${(endAt - startAt) / 1e6}      $count")
//      }
//    }
//    println("START D")
//    for(i <- 0 until 5) {
//      {
//        val startAt = System.nanoTime()
//        var count = 0
//        var idx = 0
//        while (idx < 5000000) {
//          idx += 1
//          l.foreach(_ match {
//            case Yolo(Yolo(x : Int)) => count += x
//            case _ =>
//          })
//        }
//        val endAt = System.nanoTime()
//        println(s"${(endAt - startAt) / 1e6}      $count")
//      }
//    }
//
//    println("START E")
//    for(i <- 0 until 5) {
//      {
//        val startAt = System.nanoTime()
//        var count = 0
//        var idx = 0
//        while (idx < 5000000) {
//          idx += 1
//          l.foreach(_ match {
//            case Miaou(x) => count += x
//            case _ =>
//          })
//        }
//        val endAt = System.nanoTime()
//        println(s"${(endAt - startAt) / 1e6}      $count")
//      }
//    }*/
//  }
//}