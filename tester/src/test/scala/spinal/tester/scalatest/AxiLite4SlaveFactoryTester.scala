package spinal.tester.scalatest

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{AxiLite4SlaveFactory, AxiLite4, AxiLite4Config}

object AxiLite4SlaveFactoryTester{
  def axiLite4Config = AxiLite4Config(
    addressWidth = 4,
    dataWidth = 32
  )

  class AxiLite4SlaveFactoryTester extends Component {
    val io = new Bundle {
      val bus = slave (AxiLite4(axiLite4Config))
      val nonStopWrited = out Bits(16 bits)
    }

    val ctrl = new AxiLite4SlaveFactory(io.bus)
    val regA,regB = Reg(UInt(20 bits)) init(44)
    ctrl.readAndWrite(regA,address = 9,bitOffset = 10)
    ctrl.readAndWrite(regB,address = 7,bitOffset = 10)
    ctrl.onWrite(15){
      regA := 11
    }
    ctrl.onRead(2){
      regB := 33
    }
    ctrl.nonStopWrite(io.nonStopWrited,bitOffset = 4)
  }
}

class AxiLite4SlaveFactoryTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "AxiLite4SlaveFactoryTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/AxiLite4SlaveFactoryTester"
  override def createToplevel: Component = new AxiLite4SlaveFactoryTester.AxiLite4SlaveFactoryTester
  withWaveform = true
}