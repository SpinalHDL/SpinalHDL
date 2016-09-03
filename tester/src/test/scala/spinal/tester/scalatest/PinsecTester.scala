package spinal.tester.scalatest

import org.scalatest.{Stepwise, Sequential, Suites}
import spinal.core._
import spinal.lib.com.jtag.Jtag
import spinal.lib.com.uart.Uart
import spinal.lib.io.TriStateArray
import spinal.lib.memory.sdram.{IS42x320D, SdramInterface}
import spinal.lib.{master, slave}
import spinal.lib.soc.pinsec.{PinsecTimerCtrlExternal, Pinsec}

/**
 * Created by PIC32F_USER on 22/08/2016.
*/



//object PinsecTester{
//  case class SdramModel() extends BlackBox{
//    val Dq = in Bool
//    val Addr = in Bool
//    val Ba = in Bool
//    val Clk = in Bool
//    val Cke = in Bool
//    val Cs_n = in Bool
//    val Ras_n = in Bool
//    val Cas_n = in Bool
//    val We_n = in Bool
//    val Dqm = in Bool
//  }
//
//  case class PinsecTester() extends Component{
//    val io = new Bundle{
//      val asyncReset = in Bool
//      val axiClk = in Bool
//      val jtag_tck = in Bool
//      val jtag = slave(Jtag())
//      val gpioA = master(TriStateArray(32 bits))
//      val gpioB = master(TriStateArray(32 bits))
//      val timerExternal = in(PinsecTimerCtrlExternal())
//      val uart  = master(Uart())
//    }
//
//    val pinsec = new Pinsec()
//    io.asyncReset <> pinsec.io.asyncReset
//    io.axiClk <> pinsec.io.axiClk
//    io.jtag_tck <> pinsec.io.jtag_tck
//    io.jtag <> pinsec.io.jtag
//    io.gpioA <> pinsec.io.gpioA
//    io.gpioB <> pinsec.io.gpioB
//    io.timerExternal <> pinsec.io.timerExternal
//    io.uart <> pinsec.io.uart
//  }
//}

class PinsecTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "PinsecTester"
  override def pythonTests: Seq[(String,String)] = List(
    "jtag" -> "tester/src/test/python/spinal/Pinsec/jtag",
    "uart" -> "tester/src/test/python/spinal/Pinsec/uart"
  )
  override def createToplevel: Component = {
    val pinsec = new Pinsec(133 MHz)
//    pinsec.axi.rom.ram.randBoot()
    pinsec.axi.ram.ram.randBoot()
    val sdramPowerupCounter = pinsec.axi.sdramCtrl.ctrl.powerup.counter
    sdramPowerupCounter.component.rework(
      sdramPowerupCounter init(sdramPowerupCounter.maxValue - 100)
    )
    pinsec
  }

  override def backendConfig(config: SpinalConfig): SpinalConfig = config.copy().copy(dumpWave = null)
}


//
//class OrderedSuite extends Stepwise(
//  new BundleTesterCocotbBoot,
//
//    new FixedPointTesterCocotbBoot,
//    new WhenTesterCocotbBoot,
//    new StreamTesterCocotbBoot,
//    new BlackboxTesterCocotbBoot
//  ,
//  new ZeroWidthTesterCocotbBoot
//)