package spinal.tester.code.sandbox


//class Component extends spinal.idslplugin.ValCallback{
//  override def valCallback[T](ref: T, name: String) : T = {
//    println(s"Got $ref named $name")
//    ref
//  }
//}
//
//class UInt
//class Bits
//class MyComponent extends Component{
//  val two = 2
//  val wuff = "miaou"
//  val toto = new UInt
//  val rawrr = new Bits
//}
//
//object Debug3 extends App{
//  new MyComponent()
//}




object Sandbox extends App {
  import spinal.core._
  import spinal.lib._

//  class MyComponent extends Component {
//    val a,b = in UInt(8 bits)     // Will be properly named
//    val toto = out UInt(8 bits)   // same
//
//    def doStuff(): Unit = {
//      val tmp = UInt(8 bits) // This will not be named, as it isn't stored anywhere in a component val (but there is a solution explained later)
//      tmp := 42
//      toto := tmp
//    }
//    doStuff()
//  }

//  class MyComponent extends Component {
//    val logicA = new Area{
//      val toggle = Reg(Bool())
//      toggle := !toggle
//    }
//  }

//  class MyComponent extends Component {
//    def isZero(value: UInt) = new Area {
//      val comparator = value === 0
//    }
//
//    val value = in UInt (8 bits)
//    val someLogic = isZero(value)
//
//    val result = out Bool()
//    result := someLogic.comparator
//  }

//  class MyComponent extends Component {
//    def isZero(value: UInt) = new Composite(value) {
//      val comparator = value === 0
//    }.comparator
//
//    val value = in UInt (8 bits)
//    val result = out Bool()
//    result := isZero(value)
//  }

//  class MyComponent extends Component {
//    def isZero(value: UInt) = new Composite(value) {
//      val comparator = value === 0
//    }.comparator
//
//
//    def inverted(value: Bool) = new Composite(value) {
//      val inverter = !value
//    }.inverter
//
//    val value = in UInt(8 bits)
//    val result = out Bool()
//    result := inverted(isZero(value))
//  }


//  def Miaou: Unit ={
//    val toto = UInt(8 bits)
//    val miaou = Bool()
//    toto.setName("rawrr") // Force name
//    toto.setName("rawrr", weak = true) // Propose a name, will not be applied if a stronger name is already applied
//    toto.setCompositeName(miaou, postfix = "wuff") // Force toto to be named as miaou.getName() + _wuff"
//  }
//
//  class MyComponent extends Component {
//    val source = slave(Stream(UInt(8 bits)))
//    val sink = master(Stream(UInt(8 bits)))
//    sink << source.queue(size = 16).m2sPipe()
//  }

//  class MyComponent extends Component {
//    val conditions = in Vec(Bool(), 64)
//    val result = conditions.reduce(_ || _) // Do a logical or between all the conditions elements
//  }

//  class MyComponent extends Component {
//    val a,b,c,d = in UInt(8 bits)
//    val result = a + b + c + d
//  }

//  class MyComponent extends Component {
//    val value = in UInt(8 bits)
//    val isZero = out(Bool())
//    val counter = out(Reg(UInt(8 bits)))
//
//    isZero := False
//    when(value === 0){
//      isZero := True
//      counter := counter + 1
//    }
//  }


//  class MyComponent extends Component {
//    val enable = in Bool()
//    val value = out UInt(8 bits)
//
//    def count(cond : Bool): UInt ={
//      val ret = Reg(UInt(8 bits))
//      when(cond){
//        ret := ret + 1
//      }
//      return ret
//    }
//
//    value := count(enable)
//  }


  class MyComponent extends Component{
    val a, b, c, d = Bool()
    b.setName("rawrr") // Force name
    c.setName("rawrr", weak = true) // Propose a name, will not be applied if a stronger name is already applied
    d.setCompositeName(b, postfix = "wuff") // Force toto to be named as b.getName() + _wuff"
  }

  SpinalVerilog(new MyComponent)

}
