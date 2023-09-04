package spinal.core

import spinal.tester.{SpinalAnyFunSuite, SpinalSimFunSuite, SpinalTesterCocotbBase}

object BlackboxTester {

  case class BBGenerics(aWidth: Int, bWidth: Int) extends Generic

  class BlackBoxToTest(val generic : BBGenerics) extends BlackBox {
    import generic._
    val io = new Bundle {
      val clockPin,resetPin = in Bool()

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

import spinal.core.sim._
class SpinalSimBlackboxTester extends SpinalSimFunSuite {
  test("test"){
    SimConfig.addRtl(s"tester/src/test/python/spinal/BlackBoxTester/BlackBoxToTest.${if(tester.language == VHDL) "vhd" else "v"}").doSim(new BlackboxTester.BlackBoxTester) {dut =>
      dut.clockDomain.forkStimulus(10)
      var outA_ref = 0
      var outB_ref = 0
      for(i <- 0 to 1000) {
        dut.io.inA.randomize()
        dut.io.inB.randomize()
        dut.clockDomain.waitSampling()
        assert(outA_ref == dut.io.outA.toInt)
        assert(outB_ref == dut.io.outB.toInt)
        outA_ref = ((outA_ref + dut.io.inA.toInt) & dut.io.outA.maxValue).toInt
        outB_ref = ((outB_ref + dut.io.inB.toInt) & dut.io.outB.maxValue).toInt
      }
    }
  }
}

class BlackBoxTester2 extends SpinalAnyFunSuite{
  case class MyBlackBox() extends BlackBox {
    val io = new Bundle {
      val aclk = in Bool()
      val data_in = in UInt(32 bits)
      val data_out = out UInt(32 bits)
    }

    mapClockDomain(clock=io.aclk)
    noIoPrefix()
    addTag(noNumericType)
  }

  case class BlackBoxMulti() extends Component {
    val channels = 2
    val io = new Bundle {
      val data_in = in UInt(32 bits)
      val data_out = out Vec(UInt(32 bits), channels)
    }
    for(i <- 0 until channels){
      val bb = MyBlackBox()
      bb.io.data_in := io.data_in
      io.data_out(i) := bb.io.data_out
    }

  }
  test("vhdl"){
    SimConfig.withGhdl.doSim(new BlackBoxMulti){dut => }
  }
}