package spinal.tester.code

/**
 * Created by PIC32F_USER on 20/09/2015.
 */

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3SlaveController, Apb3Config, Apb3Slave}

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


  val mySignal : Data = in UInt(5 bit)
  val myCounter : Data = out UInt(4 bit)

  myCounter := mySignal.autoResize()
}

object Play1 {
  def main(args: Array[String]): Unit = {
    SpinalVhdl(new Play1)
  }
}


class ComplexBundle extends Bundle{
  val a = Bits(12 bit)
  val b = UInt(50 bit)
  val c = Bool
  val d = Vec(Bits(8 bit),3)
}


class Play2 extends Component{
  val busConfig = new Apb3Config(16,32)
  val bus = slave(new Apb3Slave(busConfig))
  val controller = new Apb3SlaveController(bus)

  val myReadSignal = in(new ComplexBundle); controller.readSignal(myReadSignal,0x10)
  val myWriteOnlyReg = out(controller.writeOnlyReg(new ComplexBundle,0x20))
  val myWriteReadReg = out(controller.writeReadReg(new ComplexBundle,0x30))
  val myPushStreamBits = master(controller.pushStream(0x40))
  val myPushStreamComplex = master(controller.pushStream(new ComplexBundle,0x50))
  val myPopStreamComplex = slave(Stream(new ComplexBundle)); controller.popStream(myPopStreamComplex,0x60)

}

object Play2 {
  def main(args: Array[String]): Unit = {
    SpinalVhdl(new Play2)
  }
}
