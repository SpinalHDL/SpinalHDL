package spinal.tester

import spinal.core._
import spinal.lib._


object PlayDebug{


//  class TopLevel extends Component {
//    val sel = in UInt(4 bits)
//    val x = Reg(Bits(8 bits))
//    val y = (Bits(8 bits))
//
//    switch(sel){
//      is(0){
//        x := 1
//        y := 3
//      }
//      is(1){
//        x := 2
//      }
//    }
//
//    when(sel === 3){
//      x := 5
//      y := 6
//    }
//
//    when(BufferCC(sel(0))){
//      x := 87
//    }
//  }


//  class TopLevel extends Component {
//    val buffer = new BufferCC(Bits(8 bits),false, 2)
//  }

//  class TopLevel extends Component {
//    val a,b = in Bits(32 bits)
//    assert(a === b, "miaou")
//    val x = RegNext(a)
//  }

//  class TopLevel extends Component {
//    val a = in Bits(32 bits)
//    val enable = in Bool()
//    val x = ClockDomain(ClockDomain.current.clock,ClockDomain.current.reset,clockEnable = enable)(BufferCC(a))
//  }

//  class TopLevel extends Component {
//    val a,b = in Bits(32 bits)
//    val x = BufferCC(a)
//    val y = RegNext(a)
////    val z = RegNext(a)
//  }

//  class TopLevel extends Component {
//    val a,b = in Bool()
//    val result = out Bool()
//    result := a
//  }

//  class TopLevel extends Component {
//    val a,b = in Bits(32 bits)
//    val x,y = out Bits(32 bits)
//    x := 3
//    val xx = {
//      val tmp = Bits(8 bits)
//      tmp := 5
//      y := tmp
//      null
//    }
//  }

//  class TopLevel extends Component {
//    val a,b = in Bool()
//    val x = out Bool()
//    x := ConditionalContext.isTrue
//  }
//
//  case class Struct() extends Bundle {
//    val y = Bool
//    val x = Bool
//  }
//  class TopLevel extends Component {
//    val a,b = in (Struct())
//    val x = out (Struct())
//    val tmp = RegNext(a)
//    val tmp2 = Struct()
//    val tmp3 = Struct()
//    tmp2 := tmp
//    tmp3 := tmp2
//    tmp3.x init(True)
//  }


//  class TopLevel extends Component {
//     assert(True === True)
//  }

//  class TopLevel extends Component {
//    val a,b = in Bool()
//    val sel = in UInt(2 bits)
//    val result = UInt(8 bits)
////    result := 0
//    when(a){
//      result := 1
//    } otherwise {
//      result(2 downto 0) := 2
////      when(b){
////        result := 2
////      }otherwise {
////        result(7 downto 3) := 2
////      }
//    }
//
//    result(3) := b
//    when(b){
//      switch(sel){
//        is(0){
//          result := 0
////          result := 0
//        }
//        is(1){
//          result(7 downto 4) := 2
//        }
//        is(2){
//          result := 2
//          result := 0
//        }
//        is(3){
////        default{
//          result := 3
//          result.msb := True
//          result := 3
//        }
//
//      }
//    }otherwise {
//      result(7 downto 4) := 2
//    }
//  }

  class TopLevel extends Component {
    val a = in UInt(8 bits)
    val b = BufferCC(a, B(0))
  }

  def main(args: Array[String]) {
    val toplevel = SpinalConfig().generateVhdl(new TopLevel()).toplevel

  }
}
