package spinal.tester.scalatest

import spinal.core._
import spinal.lib.com.uart._
import spinal.tester.scalatest.FixedPointTester.FixedPointTester

object FixedPointTester{

  case class BundleA(width : Int) extends Bundle{
    val a = BundleAA(width)
  }
  case class BundleAA(width : Int) extends Bundle{
    val sfix = SFix(4,width bit)
  }

  class FixedPointTester extends Component {
    val io = new Bundle {
      val inSFix = in(Vec(Seq(SFix(4,16 bit),SFix(2,12 bit))))
      val outSFix = out(Vec(Seq(SFix(4,16 bit),SFix(8,24 bit))))

      val inBundleA = in(BundleA(8))
      val outBundleA = out(BundleA(6))
    }
    io.outSFix(0) := RegNext(io.inSFix(0) + io.inSFix(1)) init(io.inSFix(1))
    //28 bit  point at 28-6=22   target is at 24-8=16
    io.outSFix(1) := io.inSFix(0) * io.inSFix(1)

    io.outBundleA <> io.inBundleA

    //  val xx = UInt(3 bit)
    //  xx := True
    //  val fix = SFix(3,4)
    //  fix := xx
    //
    //  io.outSFix.apply(0) := Trues
  }
}


class FixedPointTesterBoot extends SpinalTesterBase {
  override def getName: String = "FixedPointTester"
  override def createToplevel: Component = new FixedPointTester
}