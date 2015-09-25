package spinal.tester.code

/**
 * Created by PIC32F_USER on 20/09/2015.
 */

import spinal.core._
import spinal.lib._

trait BundleA extends Bundle{
  val a = Bool
}
trait BundleB extends Bundle{
  val b = Bool
}
trait BundleC extends Bundle{
  val c = Bool
}
trait BundleD extends Bundle{
  val d = Bool
}
class Stage0 extends Bundle with BundleA with BundleB with BundleC
class Stage1 extends Bundle with BundleA with BundleB with BundleD

class Play1 extends Component{
  val io = new Bundle{
    val input = slave Stream(new Stage0)
    val output = master Stream(new Stage1)
  }
//  io.output.translateFrom(io.input)((to,from) => {
//    to.assignSomeByName(from)
//    to.d := False
//  })

  io.input.translateInto(io.output)((to,from) => {
    to.assignSomeByName(from)
    to.d := False
  })


  val mySignal = UInt(3 bit)
  val myCounter = UInt(4 bit)

  myCounter :> mySignal


}

object Play1 {
  def main(args: Array[String]): Unit = {
    SpinalVhdl(new Play1)
  }
}
