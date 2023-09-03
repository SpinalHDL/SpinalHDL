package spinal.core

import spinal.lib._
import spinal.tester.SpinalTesterCocotbBase

object CommonTester {


  class BundleA extends Bundle {
    val bod = new Bundle {
      val gggg = Bool()
      val aosi = UInt(3 bit)
    }
    val ahe = Bool()
    val zwg = Bool()
  }

  class BundleAA extends BundleA {
    val vsw = Bool()
    val lwee = UInt(5 bit)
  }

  class CommonTester extends Component {
    val io = new Bundle {
      val conds = in Vec(Bool(),8)

      val inUIntA = in UInt (8 bit)
      val inUIntB = in UInt (8 bit)
      val outUIntAdder = out UInt()

      val inAA = in(new BundleAA)
      val inAABits = in Bits (new BundleAA().getBitsWidth bit)
      val outAA = out(new BundleAA)
      val outAABits = out Bits (new BundleAA().getBitsWidth bit)

      val complexLiteral = out UInt (16 bit)


      val assign = new Bundle{
        val sel = in Vec(UInt(4 bit),4)
        val bitDemux = out Bits(16 bit)


        def doit: Unit ={
          bitDemux := B(0)
          bitDemux(sel(0)) := conds(0)
          when(conds(1)){
            bitDemux(sel(1)) := conds(2)
          }elsewhen(conds(3)){
            bitDemux(sel(0)) := conds(4)
          }
          when(conds(5)){
            bitDemux(sel(1)) := conds(6)
          }
          bitDemux(5) := True
        }
      }
      def doit: Unit ={
        assign.doit
      }
    }


    io.doit

    val noData = NoData

    io.outAA.assignFromBits(io.inAABits)
    io.outAABits := io.inAA.asBits

    io.complexLiteral(15 downto 4) := 0x70
    io.complexLiteral(15 downto 12) := (U(2) + U(1)).resized
    io.complexLiteral(6) := True
    io.complexLiteral(3) := True
    io.complexLiteral(5) := True
    io.complexLiteral(3 downto 0) := 2
    io.complexLiteral(13) := False

    def combAdderFunc(x : UInt,y : UInt) = {
      val ret = UInt(Math.max(widthOf(x),widthOf(y)) bit)
      val size = io.inUIntA.getWidth
      var c = False
      for (i <- 0 until size) {
        val a = x(i)
        val b = y(i)
        ret(i) := a ^ b ^ c
        c \= (a & b) | (a & c) | (b & c)
      }
      ret
    }


//    val combAdder = new Area {
//      val size = io.inUIntA.getWidth
//      val out = UInt(size bit)
//
//      var c = False
//      for (i <- 0 until size) {
//        val a = io.inUIntA(i)
//        val b = io.inUIntB(i)
//        out(i) := a ^ b ^ c
//        c = (a & b) | (a & c) | (b & c)
//      }
//      io.outUIntAdder := out
//    }
    io.outUIntAdder := combAdderFunc(io.inUIntA,io.inUIntB)



    //Clone test
    case class MyBundle(paramBool : Bool,asd : Int) extends Bundle{
      val a = cloneOf(paramBool)
    }

    class MyBundle2 extends Bundle{
      val a = Bool()
    }

    cloneOf(new MyBundle(True,1))
    cloneOf(new MyBundle2)



  }

}

//class CommonTesterGhdlBoot extends SpinalTesterGhdlBase {
//  override def getName: String = "CommonTester"
//
//  override def createToplevel: Component = new CommonTester.CommonTester
//}

class CommonTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "CommonTester"
  override def createToplevel: Component =  new CommonTester.CommonTester
  override def pythonTestLocation: String = "tester/src/test/python/spinal/CommonTester"
}