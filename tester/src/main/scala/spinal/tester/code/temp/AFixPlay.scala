package spinal.tester.code.temp

import spinal.core._
import spinal.lib._


object AFixPlay extends App{
  SpinalVerilog(new Component{
    val l0 = new Area {
      val a, b = new AFix(15, -16, -2)
      val c = a + b

      val d = in Bool()

      def miaou(that : AFix) = println(that.raw)

      miaou(AFix.U(12 bits)) //Q12.0
      miaou(AFix.U(8 exp, 12 bits)) //Q8.4
      miaou(AFix.U(8 exp, -4 exp)) //Q8.4
//      miaou(AFix.U(255, -4 exp)) //Q8.4
      miaou(AFix.S(12 bits)) //Q11.0 + sign bit
      miaou(AFix.S(8 exp, 12 bits)) //Q8.3  + sign bit
      miaou(AFix.S(8 exp, -4 exp)) //Q8.4  + sign bit
//      miaou(AFix.S(255, -256, -4 exp)) //Q8.4
    }

//    val l1 = new Area{
//      val a = in(AFix.S(8 exp, 4 bits))
//      val b = out(AFix.U(12 bits))
//      b := a.truncated  //.truncated
//    }
  })

}
