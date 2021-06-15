
package spinal.tester.code



import spinal.core.Nameable.{DATAMODEL_WEAK, USER_WEAK}
import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState

import scala.collection.mutable.ArrayBuffer


object Debug {
  class NameVecIndexing extends Component {
    val io = new Bundle {
      val qpsize = in  UInt(32 bits)
      val addr1 = in  UInt(32 bits)
      val addr2 = in  UInt(32 bits)
      val addr3 = in  UInt(32 bits)
      val qlevel = out (UInt(32 bits))
    }
    noIoPrefix()

    val qp_f = Vec(Vec(Vec(UInt(32 bits), 5), 4), 3)

    for (i <- 0 until 3) (
      for (j <- 0 until 4) (
        for (k <- 0 until 5) {
          qp_f(i)(j)(k) := i + j + k
        }
        )
      )

    io.addr1.addTag(tagAutoResize)
    io.addr2.addTag(tagAutoResize)
    io.addr3.addTag(tagAutoResize)
    io.qlevel := qp_f(io.addr1(8 downto 4).addTag(tagAutoResize))(io.addr2)(io.addr3)
  }


  def main(args: Array[String]) {
    SpinalVerilog(new NameVecIndexing)

  }

  //  createEnum("asd")
}


object Debug2 extends App{

//  class Miaou extends Bundle{
//
//
//    def fifo2() = new Composite{
//      val x = 4
//    }.x
//
//    def fifo1(): Unit = new Area {
//      val x = UInt(8 bits)
//
//      this.setCompositeName(Miaou.this)
//      x + 1
//    }
////    def fifo(): Unit = composite(this) {
////      val x = UInt(8 bits)
////
////      x + 1
////    }
//  }
  SpinalVerilog(new Component{
//    val input = slave Stream(UInt(8 bits))
//    val output = master Stream(UInt(8 bits))
//
//    output << input.queue(8).m2sPipe()

//    val a,b,c,d = in Bool()
//
//    val result = out UInt(8 bits)
//    result.allowOverride
//
//    def miaou(value : Int) = new Composite(result){
//      val x = U(value, 8 bits)
//    }.x
//
//    def miaou2(value : Int) = new Composite(result, "rawrrr"){
//      val x = U(value, 8 bits) + self
//    }.x
//
//    def miaou3(value : Int) = new Composite(UInt(8 bits)){
//      val tmp = U(value, 8 bits)
//      self := tmp
//    }.self
//
//
//    def miaou4(value : Int) ={
//      val tmp = UInt(8 bits)
//      tmp := value
//      tmp(1) := a
//
//
//      val tmp2 = UInt(8 bits)
//      tmp2 := value+tmp
//      tmp2(1) := b
//
//      tmp2
//    }
//
//    result := miaou4(10)

    val digital = slave(TriState(Bool))
    val pin = inout(Analog(Bool))
    when(digital.writeEnable){
      pin := digital.write
    }
    digital.read := pin



//    val t1 = new Area {
//      val x = miaou(1)
//      val y = miaou(2)
//
//      miaou(3)
//    }
//
//    val t2 = new Area {
//      val x = miaou2(4)
//      val y = miaou2(5)
//      miaou2(6)
//    }
//    val t3 = new Area {
//      val x = miaou3(7)
//      val y = miaou3(8)
//      miaou3(9)
//    }

//    result := 0

//    when(a){
//      miaou(4)
//    }
//    when(b){
//      miaou(5)
//    }


    //    val miaou = UInt(8 bits).setCompositeName(input.queue(8))
//  println(miaou.getName())
//    val input =  Vec(Bool, 8)
//    val sel = in UInt(3 bits)
//    val output = (input(sel))

//    val output = out(Reg(Bool) init(False))
//    output := False

//    val sub1 = new Component{
//      val sub2 = new Component{
//        val a = False
//        val sub3 = new Component{
//
//        }
//      }
//    }
//
//    val rawrrr = (CombInit(sub1.sub2.a.pull()))


//    val a,b,c = Bool()
//
//    a := False
//    when(b || c){
//      a := True
//    }

//    val a = Bool()
//
//    a := False

//    val wuff = in Bool()

//    val bb = new Component{
//      val miaou = in UInt(8 bits)
//        setDefinitionName("aaaa")
//      setName("bbb")
//    }
//    bb.miaou := 4
//    val a,b,c,d = in UInt(8 bits)
//    val result = out (UInt(8 bits))
//    result := a + b + c.resize(9 bits)(7 downto 0) + d
//
//    when(a+b+c === 0){
//      result := 42
//    }

//    when(a+b+c === 0){
//    }
    setDefinitionName("miaou")
  })

}

//object MyEnum extends  spinal.core.MacroTest.mkObject("asd")
//
//


