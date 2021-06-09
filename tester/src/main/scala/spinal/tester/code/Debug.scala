
package spinal.tester.code



import spinal.core._
import spinal.lib._

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

  class Miaou extends Bundle{

    def fifo1(): Unit = new Area {
      val x = UInt(8 bits)

      this.setCompositeName(Miaou.this)
      x + 1
    }
//    def fifo(): Unit = composite(this) {
//      val x = UInt(8 bits)
//
//      x + 1
//    }
  }
  SpinalVerilog(new Component{



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
    val a,b,c,d = in UInt(8 bits)
    val result = out (UInt(8 bits))
//    result := a + b + c.resize(9 bits)(7 downto 0) + d

    when(a+b+c === 0){
      result := 42
    }

//    when(a+b+c === 0){
//    }
  })

}

//object MyEnum extends  spinal.core.MacroTest.mkObject("asd")
//
//
