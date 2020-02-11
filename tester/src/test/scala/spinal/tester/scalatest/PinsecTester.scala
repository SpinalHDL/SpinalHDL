package spinal.tester.scalatest

import org.scalatest.{Stepwise, Sequential, Suites}
import spinal.core._
import spinal.lib._
import spinal.lib.com.jtag.Jtag
import spinal.lib.com.uart.Uart
import spinal.lib.io.TriStateArray
import spinal.lib.memory.sdram.sdr.IS42x320D
import spinal.lib.{master, slave}
import spinal.lib.soc.pinsec.{PinsecConfig, PinsecTimerCtrlExternal, Pinsec}

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
    val pinsec = new Pinsec(PinsecConfig.default.copy(axiFrequency = 133 MHz))
//    pinsec.axi.rom.ram.randBoot()
    pinsec.axi.ram.ram.randBoot()
    val sdramPowerupCounter = pinsec.axi.sdramCtrl.ctrl.powerup.counter
    sdramPowerupCounter.component.rework(
      sdramPowerupCounter init(sdramPowerupCounter.maxValue - 100)
    )
    pinsec.axi.ram.rework{
      import pinsec.axi.ram._

      val port = ram.writePort
      port.valid.setName("ram_port0_write") := False
      port.address.setName("ram_port0_address") := 0
      port.data.setName("ram_port0_writeData") := 0

      Bool().setName("ram_port0_enable") := False
      Bits(4 bits).setName("ram_port0_mask") := 0
    }
    pinsec
  }
  override def backendConfig(config: SpinalConfig) = config.mode match {
    case `Verilog` => config.copy(mergeAsyncProcess = false) // avoid iverilog bug
    case _ => config
  }
  override def noVhdl = true

//  withWaveform = true
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