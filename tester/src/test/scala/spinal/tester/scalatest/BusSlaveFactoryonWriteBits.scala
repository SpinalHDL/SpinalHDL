package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{Assertions}
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._

object BusSlaveFactoryonWriteBitsTester {
  class BusSlaveFactoryonWriteBitsTester extends Component {
    val io = new Bundle {
      val bus = slave(Apb3(Apb3Config(32, 32)))
      val ok1 = out(Bool)
      val ok2 = out(Bool)
    }
    val (ok1, ok2) = (RegNext(False), RegNext(False))
    io.ok1 := ok1
    io.ok2 := ok2
    val factory = Apb3SlaveFactory(io.bus)
    factory.onWriteBits(0x0) { bits =>
      when(bits(0) === True) {
        ok1 := True
      }
    }
    factory.onWriteBits(0x0040) { bits =>
      when(bits(1) === True) {
        ok2 := True
      }
    }
  }
}

class BusSlaveFactoryonWriteBitsTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "BusSlaveFactoryonWriteBitsTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/BusSlaveFactoryonWriteBitsTester"
  override def createToplevel: Component = new BusSlaveFactoryonWriteBitsTester.BusSlaveFactoryonWriteBitsTester

  withWaveform = true
  override def noVhdl = true
}
