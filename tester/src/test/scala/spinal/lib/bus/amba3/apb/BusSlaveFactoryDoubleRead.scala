package spinal.lib.bus.amba3.apb

import org.scalatest.{Assertions}
import spinal.core._
import spinal.lib._
import spinal.tester.SpinalAnyFunSuite


object TestTopLevel {

  class DoubleReadDut extends Component {

    val io = new Bundle {
      val bus = slave(Apb3(Apb3Config(32, 32, 1, false)))
      val dummy_r_0 = in Bits (16 bits)
      val dummy_r_1 = in Bits (16 bits)
    }

    val factory = new Apb3SlaveFactory(io.bus, selId = 0)

    factory.read(io.dummy_r_0, 0x00, 0)
    factory.read(io.dummy_r_1, 0x00, 8)

  }

  class DoubleWriteDut extends Component {

    val io = new Bundle {
      val bus = slave(Apb3(Apb3Config(32, 32, 1, false)))
      val dummy_w_0 = out Bits (16 bits)
      val dummy_w_1 = out Bits (16 bits)
    }

    val factory = new Apb3SlaveFactory(io.bus, selId = 0)

    factory.drive(io.dummy_w_0, 0x00, 0)
    factory.drive(io.dummy_w_1, 0x00, 8)

  }

}

class BusSlaveFactoryDoubleReadTester extends SpinalAnyFunSuite {

  test("BusSlaveFactoryDoubleRead") {

    val caught = Assertions.intercept[java.lang.AssertionError](
      SpinalConfig().generateVerilog {
        new TestTopLevel.DoubleReadDut()
      }
    )

    assert(caught.toString.contains("DOUBLE-READ-ERROR"),
      "SpinalHDL compilation did not throw a DOUBLE-READ-ERROR where there should have been one!")
  }

  test("BusSlaveFactoryDoubleWrite") {
    // test that no error is thrown on double write access
    SpinalConfig().generateVhdl {
      new TestTopLevel.DoubleWriteDut()
    }
  }
}

