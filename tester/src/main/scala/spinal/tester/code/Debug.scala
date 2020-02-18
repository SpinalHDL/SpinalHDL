
package spinal.tester.code



import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer


object Debug {
  class halfadder () extends Component{
    val io = new Bundle{
      val a = in  Bits(1 bits)
      val b = in  Bits(1 bits)
      val s = out Bits(1 bits)
      val c = out Bits(1 bits)
    }
    io.s := io.a ^ io.b
    io.c := io.a & io.b
  }

  //---------------------------------------------------
  //1-Bit Full Adder
  //---------------------------------------------------
  class fulladder () extends Component{
    val io = new Bundle{
      val a    = in  Bits(1 bits)
      val b    = in  Bits(1 bits)
      val cin  = in  Bits(1 bits)
      val sum  = out Bits(1 bits)
      val cout = out Bits(1 bits)
    }
    val cell0 = new halfadder
    val cell1 = new halfadder

    cell0.io.a := io.a
    cell0.io.b := io.b

    cell1.io.a := io.cin
    cell1.io.b := cell0.io.s

    io.sum     := cell1.io.s
    io.cout    := cell0.io.c | cell1.io.c
  }
  //---------------------------------------------------
  // 4-Bits carry adder
  //---------------------------------------------------
  class carryadder extends Component{
    val io = new Bundle{
      val cin   = in Bits (1 bits)
      val op0   = in  Bits (4 bits)
      val op1   = in  Bits (4 bits)
      val sum   = out Bits (4 bits)
      val cout  = out Bits (1 bits)
    }
    //val value = Bits(4 bits)

    val cellArray = Array.fill(4)(new fulladder)

    cellArray(0).io.cin <> io.cin
    cellArray(0).io.a   <> io.op0(0).asBits
    cellArray(0).io.b   <> io.op1(0).asBits
    //io.sum(0).asBits    <> cellArray(0).io.sum

    for (i <- 1 until 4){
      cellArray(i).io.cin <> cellArray(i-1).io.cout
      cellArray(i).io.a   <> io.op0(i).asBits
      cellArray(i).io.b   <> io.op1(i).asBits
      //io.sum(i).asBits    <> cellArray(i).io.sum
    }

    io.cout := cellArray(3).io.cout
    for (i <- 0 until 4){
      io.sum(i).asBits    <> cellArray(i).io.sum
    }
  }
  class A {
    val a = 1
    val aa = 2
  }

  class B extends A {
    val b = 2
  }

  class C(xx: Int) extends B {

  }

  class MyBundle extends Bundle {
    val a = Bool
    val b = Bool
    val c = Vec(new MyBundle2, 2)
  }

  class MyBundle2 extends Bundle {
    val a = Bool
    val b = Bool
  }


  class MyBundleSub extends MyBundle {

  }

  object MyEnum extends SpinalEnum {
    val s0, s1, s2 = newElement()
  }

  object MyEnum2 extends SpinalEnum {
    val s0, s1, s2 = newElement()
  }

  class SubA extends Component {
    val subB = new SubB()
  }

  class SubB extends Component {
    val titi = Bool
  }

  class TopLevel(a: Int) extends Component {
    val toto = Bool
    println("toto : " + toto.getName())
    val subA = new SubA()

    println("TITI " + (subA.subB.titi.getComponents().map(_.getName()) ++ Seq(subA.subB.titi.getName())).reduce(_ + ":" + _))
    val io = new Bundle {
      //      val in1 = in (new MyBundle)
      //      val out1 = out (new MyBundle2)

      //      val outputVec = Vec(3, master Stream (new MyBundle))

      val input = slave Stream (new MyBundle)
      val output = master Stream (new MyBundle)

      //      val romCmd = slave Stream(UInt(2 bit))
      //      val romRead = master Stream(RomData())
      //      val romCmd = in(UInt(2 bit))
      //      val romRead = out(MyData())
      val sin = out SInt (16 bit)
      val fir = out SInt (16 bit)

      val boolToUnsigned = out UInt()
      val tt = out Bool
    }
    //    println(io.elements.mkString("\n"))
    SFix(4 exp, 8 bit).maxValue
    SFix(4 exp, 8 bit).minValue

    SFix(4 exp, 8 bit)
    SFix(4 exp, 8 exp)

    val newInput = in Bool
    val newOutput = out Bool

    newOutput := !newInput

    val myClockDomain = ClockDomain.external("ttDomain")
    val ttArea = new ClockingArea(myClockDomain) {
      io.tt := RegNext(!io.tt)
    }

    MyEnum.s1 === MyEnum.s2()
    //    implicit def EnumElementToCraft[T <: SpinalEnum](enumDef : T) : SpinalEnumCraft[T] = enumDef.craft().asInstanceOf[SpinalEnumCraft[T]]
    //    implicit def EnumElementToCraft2[T <: SpinalEnum](enumDef : SpinalEnumElement[T]) : SpinalEnumCraft[T] = enumDef.craft().asInstanceOf[SpinalEnumCraft[T]]
    //
    val s0Reg = RegNext(MyEnum.s0())

    io.boolToUnsigned := U(True)

    val forks = StreamFork(io.input, 3)
    io.output << StreamArbiterFactory.lowerFirst.transactionLock.on(forks)

    object MyData {
      def apply(a: Boolean, b: BigInt): MyData = {
        val ret = MyData()
        ret.a := Bool(a)
        ret.b := b
        ret
      }
    }

    //    for(i <- 0 to 63){
    //      romData += MyData(false,i)
    //    }

    case class MyData() extends Bundle {
      val a = Bool
      val b = UInt(3 bit)
    }

    //    val romData = ArrayBuffer(MyData(false,1),MyData(false,2),MyData(true,3),MyData(false,4))
    //    val rom = Mem(MyData(),68) init(romData)
    //
    //    io.romRead := rom(io.romCmd)

    //    val lockupTable = Mem(SInt(16 bit),1024)
    //    lockupTable.init((0 to 1023).map(i => S((Math.sin(i*2*Math.PI/1024.0)*32767).toInt)))
    //
    //    val counter = CounterFreeRun(1024)
    //    io.sin := (lockupTable(counter)*lockupTable(counter)) >> 16


    io.sin := Mem(SInt(16 bit), (0 to 1023).map(i => S((Math.pow(Math.sin(i * 2 * Math.PI / 1024.0), 1) * 32767).toInt))).readSync(CounterFreeRun(1024))

    val waveform = (0 to 1023).map(i => {
      S((Math.pow(Math.sin(i * 2 * Math.PI / 1024.0), 1) * 32767).toInt)
    })
    val rom = Mem(SInt(16 bit), waveform)
    val animated = rom.readSync(CounterFreeRun(1024))

    val firLength = 32
    val coefs = (0 until firLength).map(i => S(((0.54 - 0.46 * Math.cos(2 * Math.PI * i / firLength)) * 32767 / firLength).toInt, 16 bit))
    io.fir := (coefs, History(io.sin, firLength)).zipped.map((coef, delay) => (coef * delay) >> 15).reduce(_ + _)


    //MacroTest.mkObject("asd")

  }


  def main(args: Array[String]) {
    println("START")
    val a = new A
    val b = new B
    val c = new C(2)

    SpinalVhdl(new carryadder())
    println("DONE")


  }

  //  createEnum("asd")
}


//object MyEnum extends  spinal.core.MacroTest.mkObject("asd")
//
//
