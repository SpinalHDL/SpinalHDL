package spinal.tester.scalatest

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4SlaveFactory}

object Axi4SlaveFactoryTester {
  def axi4Config = Axi4Config(
    addressWidth = 4,
    dataWidth = 32,
    idWidth = 0
  )

  class Axi4SlaveFactoryTester extends Component {
    val io = new Bundle {
      val bus = slave(Axi4(axi4Config))
      val nonStopWrited = out Bits (16 bits)
    }

    val ctrl = new Axi4SlaveFactory(io.bus)
    val regA, regB = Reg(UInt(20 bits)) init (44)
    ctrl.readAndWrite(regA, address = 9, bitOffset = 10)
    ctrl.readAndWrite(regB, address = 7, bitOffset = 10)
    ctrl.onWrite(15) {
      regA := 11
    }
    ctrl.onRead(2) {
      regB := 33
    }
    ctrl.nonStopWrite(io.nonStopWrited, bitOffset = 4)
  }
}

class Axi4SlaveFactoryTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "Axi4SlaveFactoryTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/Axi4SlaveFactoryTester"
  override def createToplevel: Component = new Axi4SlaveFactoryTester.Axi4SlaveFactoryTester

  withWaveform = true
}