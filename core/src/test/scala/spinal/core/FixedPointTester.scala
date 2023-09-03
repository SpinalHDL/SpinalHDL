package spinal.core

import spinal.tester.SpinalTesterCocotbBase

object FixedPointTester{

  case class BundleA(width : Int) extends Bundle{
    val a = BundleAA(width)
  }
  case class BundleAA(width : Int) extends Bundle{
    val sfix = SFix(4 exp,width bit)
  }

  class FixedPointTester extends Component {
    val io = new Bundle {
      val inSFix = in(Vec(Seq(SFix(4 exp,16 bit),SFix(2 exp,12 bit))))
      val outSFix = out(Vec(Seq(SFix(4 exp,16 bit),SFix(8 exp,24 bit))))

      val inSFix2 = in SFix(0 exp, -8 exp)
      val outSFix2 = out(SFix(2 exp, -8 exp))

      val inBundleA = in(BundleA(8))
      val outBundleA = out(BundleA(6))
    }
    io.outSFix(0) := (io.inSFix(0) + io.inSFix(1))
    //28 bit  point at 28-6=22   target is at 24-8=16
    io.outSFix(1) := (io.inSFix(0) * io.inSFix(1)).truncated

    io.outSFix2 := ((io.inSFix2 <<|1) + io.inSFix2 ) <<|1
    //io.outSFix(0).raw := S(0,16 bit)
    //io.outSFix(1).raw := S(0,24 bit)
    //io.outBundleA.a.sfix.raw := S(0,6 bit)
    io.inBundleA.a.sfix.addTag(tagTruncated)
    io.outBundleA <> io.inBundleA



  }
}

//
//class FixedPointTesterGhdlBoot extends SpinalTesterGhdlBase {
//  override def getName: String = "FixedPointTester"
//  override def createToplevel: Component = new FixedPointTester
//}

class FixedPointTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "FixedPointTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/FixedPointTester"
  override def createToplevel: Component = new FixedPointTester.FixedPointTester
}