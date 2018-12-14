package spinal.tester.scalatest

import org.scalatest.{FunSuite, Assertions}
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._


object TestTopLevel {

  class TestTopLevelDut extends Component {

    val io = new Bundle {
      val bus = slave(Apb3(Apb3Config(32, 32, 1, false)))
      val dummy_r_0 = in Bits (32 bits)
      val dummy_r_1 = in Bits (16 bits)
    }

    val factory = new Apb3SlaveFactory(io.bus, selId = 0)

    factory.read(io.dummy_r_0, 0x00, 0)
    factory.read(io.dummy_r_1, 0x00, 8)

  }

}

class BusSlaveFactoryDoubleReadTester extends FunSuite {

  test("BusSlaveFactoryDoubleRead") {

    val caught = Assertions.intercept[spinal.core.SpinalExit](
      SpinalConfig(
        targetDirectory = "/dev",
        netlistFileName = "null"
      ).generateVhdl(
        new TestTopLevel.TestTopLevelDut()
      )
    )

    assert(caught.toString.contains("HIERARCHY VIOLATION"),
      "SpinalHDL compilation did not throw a HIERARCHY VIOLATION where there should have been one!")
  }
}

