package spinal.tester.code

/**
 * Created by PIC32F_USER on 20/09/2015.
 */

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3SlaveController, Apb3Config, Apb3Slave}

trait BundleA extends Bundle {
  val a = Bool
}

trait BundleB extends Bundle {
  val b = Bool
}

trait BundleC extends Bundle {
  val c = Bool
}

trait BundleD extends Bundle {
  val d = Bool
}

class Stage0 extends Bundle with BundleA with BundleB with BundleC

class Stage1 extends Bundle with BundleA with BundleB with BundleD

class Play1 extends Component {
  val io = new Bundle {
    val input = slave Stream (new Stage0)
    val output = master Stream (new Stage1)
  }


  io.input.translateInto(io.output)((to, from) => {
    to.assignSomeByName(from)
    to.d := False
  })
}

object Play1 {
  def main(args: Array[String]): Unit = {
    SpinalVhdl(new Play1)
  }
}


class ComplexBundle extends Bundle {
  val a = Bits(12 bit)
  val b = UInt(50 bit)
  val c = Bool
  val d = Vec(Bits(8 bit), 3)
}


class Play2 extends Component {
  val busConfig = new Apb3Config(16, 32)
  val bus = slave(new Apb3Slave(busConfig))
  val controller = new Apb3SlaveController(bus)

  val myReadSignal = in(new ComplexBundle);
  controller.readSignal(myReadSignal, 0x10)
  //  val myWriteOnlyReg = out(controller.writeOnlyReg(new ComplexBundle,0x20))
  //  val myWriteReadReg = out(controller.writeReadReg(new ComplexBundle,0x30))
  val myPushStreamBits = master(controller.writeStream(0x40))
  val myPushStreamComplex = master(controller.writeStreamOf(new ComplexBundle, 0x50))
  val myPopStreamComplex = slave(Stream(new ComplexBundle));
  controller.readStream(myPopStreamComplex, 0x60)

}

object Play2 {
  def main(args: Array[String]): Unit = {
    SpinalVhdl(new Play2)
  }
}


class Play3 extends Component {


  //  val c = out(a & b)
  //  lazy val a = in Bool
  //  lazy val b = in Bool
//  val areaC = new Area {
//    lazy val c  : Bool = areaAB.a & areaAB.b
//    lazy val d : Bool = True
//  }
//  val areaAB = new Area {
//    lazy val a : Bool = in Bool
//    lazy val b : Bool = in Bool
//    lazy val c : Bool  =  areaC.d
//  }
//
//  out(areaC.c)


  //  val arrayType = Vec(Bool,7)
  //  //val arrayIn = in Vec(Vec(Bool,5),10)
  //  //val arrayIn = in Vec(arrayType,10)
  //  val arrayIn = in Vec(UInt(3 bit), UInt(5 bit),UInt(7 bit))
  //  val arrayOut = out cloneOf(arrayIn)
  //  arrayOut := arrayIn
  //
  //  val uList = List(U(4),U(5))
  //  val count = out (SetCount(B"b1100"))
  //  val count2 = out(uList.sContains(U(0)))
  //
  //
  //  val normalVec = Vec(UInt(4 bit),10)
  //
  //  val containZero = normalVec.sContains(0)
  //  val existOne = normalVec.sExists(_ === 1)
  //  val (firstTwoValid,firstTwoIndex) = normalVec.sFindFirst(_ === 2)
  //
  //
  //  in(normalVec)
  //  out(firstTwoValid)
  //  out(firstTwoIndex)
}

object Play3 {
  def main(args: Array[String]): Unit = {
    SpinalVhdl(new Play3)
  }
}