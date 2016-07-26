package spinal.tester.scalatest

import spinal.core._
import spinal.lib._

object BlackboxTester {

  case class BBGenerics(aWidth: Int, bWidth: Int) extends Generic

  class BlackBoxToTest(val generic : BBGenerics) extends BlackBox {
    import generic._
    val io = new Bundle {
      val clockPin,resetPin = in Bool

      val inA = in UInt(aWidth bits)
      val inB = in UInt(bWidth bits)
      val outA = out UInt(aWidth bits)
      val outB = out UInt(bWidth bits)
    }

    mapClockDomain(clock=io.clockPin,reset = io.resetPin)
  }

  class BlackBoxTester extends Component {
    val generic = BBGenerics(8,16)
    import generic._
    val io = new Bundle{
      val inA = in UInt(aWidth bits)
      val inB = in UInt(bWidth bits)
      val outA = out UInt(aWidth bits)
      val outB = out UInt(bWidth bits)
    }
    val blackBoxtoTest  = new BlackBoxToTest(generic)
    io.inA <> blackBoxtoTest.io.inA
    io.inB <> blackBoxtoTest.io.inB
    io.outA <> blackBoxtoTest.io.outA
    io.outB <> blackBoxtoTest.io.outB
  }

}

class BlackboxTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "BlackBoxTester"
  override def createToplevel: Component =  new BlackboxTester.BlackBoxTester
  override def pythonTestLocation: String = "tester/src/test/python/spinal/BlackBoxTester"
}