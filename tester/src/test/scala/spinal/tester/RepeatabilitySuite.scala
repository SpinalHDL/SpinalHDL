package spinal.tester

import spinal.core._
import spinal.core.sim._
import spinal.core.internals.GraphUtils

import spinal.lib.com.i2c._
import spinal.lib.com.uart.{UartCtrl, UartCtrlGenerics}
import spinal.lib.soc.pinsec.{Pinsec, PinsecConfig}

class RepeatabilityTester extends RepeatabilitySuite {
  def configI2C = I2cSlaveMemoryMappedGenerics(
    ctrlGenerics = I2cSlaveGenerics(),
    addressFilterCount = 0,
    masterGenerics = I2cMasterMemoryMappedGenerics(timerWidth = 32)
  )

  test("Apb3I2cCtrlGraph") {
    val dut = SpinalConfig(defaultClockDomainFrequency = FixedFrequency(50 MHz))
      .generateVerilog(new Apb3I2cCtrl(configI2C))
      .toplevel
    assert(GraphUtils.countNames(dut) == 221)
  }

  test("UartGraph") {
    val dut = SpinalVerilog(new UartCtrl(UartCtrlGenerics())).toplevel
    assert(GraphUtils.countNames(dut) == 94)
  }

  test("Apb3I2cCtrlVerilog") {
    checkOutputHash(new Apb3I2cCtrl(configI2C))
  }

  test("UartVerilog") {
    checkOutputHash(new UartCtrl(UartCtrlGenerics()))
  }

  test("PinsecVerilog") {
    checkOutputHash(new Pinsec(PinsecConfig.default))
  }
}
