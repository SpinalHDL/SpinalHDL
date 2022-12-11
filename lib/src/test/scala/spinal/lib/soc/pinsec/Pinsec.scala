package spinal.lib.soc.pinsec

import org.scalatest.{Stepwise, Sequential, Suites}
import spinal.tester.SpinalTesterCocotbBase

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriStateArray
import spinal.lib.com.jtag.Jtag
import spinal.lib.com.uart.Uart

class PinsecTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "PinsecTester"

  override def pythonTests: Seq[(String, String)] = List(
    "jtag" -> "tester/src/test/python/spinal/Pinsec/jtag",
    "uart" -> "tester/src/test/python/spinal/Pinsec/uart"
  )

  override def createToplevel: Component = {
    val pinsec = new Pinsec(PinsecConfig.default.copy(axiFrequency = 133 MHz))
    pinsec.axi.ram.ram.randBoot()

    val sdramPowerupCounter = pinsec.axi.sdramCtrl.ctrl.powerup.counter
    sdramPowerupCounter.component.rework(
      sdramPowerupCounter init sdramPowerupCounter.maxValue - 100
    )

    val miaou = pinsec.axi.ram.rework(new Area {
      import pinsec.axi.ram._

      val port = ram.writePort
      port.valid.setName("ram_port0_write")     := False
      port.address.setName("ram_port0_address") := 0
      port.data.setName("ram_port0_writeData")  := 0

      val a = Bool().setName("ram_port0_enable")
      val b = Bits(4 bits).setName("ram_port0_mask")
      a := False
      b := 0
    })

    miaou.a.pull()
    miaou.b.pull()
    pinsec
  }

  override def backendConfig(config: SpinalConfig) = config.mode match {
    case `Verilog` =>
      config.copy(mergeAsyncProcess = false) // avoid iverilog bug
    case _ => config
  }

  override def noVhdl = true
}
