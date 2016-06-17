# Simple tests for an adder module
import cocotb
from cocotb.triggers import Timer, RisingEdge, FallingEdge, Event
#from cocotb.result import TestFailure

from spinal.common.misc import ClockDomainInAsynResetn

#from I2CSlaveModel import I2CSlaveModel



@cocotb.test()
def master_test(dut):

    dut.log.info("Cocotb I2C Master HAL ")


    cocotb.fork(ClockDomainInAsynResetn(dut.clk, dut.resetn))

    yield Timer(5000000)


    dut.log.info("I2C Master HAL is done")

