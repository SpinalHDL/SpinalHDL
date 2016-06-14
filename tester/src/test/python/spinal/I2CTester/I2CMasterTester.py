# Simple tests for an adder module
import cocotb
from cocotb.triggers import Timer, RisingEdge, FallingEdge, Event
from cocotb.result import TestFailure

from spinal.common.misc import ClockDomainInAsynResetn
from I2CSlaveModel import I2CSlaveModel


@cocotb.coroutine
def send_randomData(dut, model):
    dut.io_start      <= 0
    dut.io_addrDevice <= 0
    yield RisingEdge(dut.clk)
    dut.io_start      <= 1
    dut.io_addrDevice <= 2
    yield RisingEdge(dut.clk)
    if int(dut.io_write_ready) == 1 :
        dut.io_write_payload <= 3
        dut.io_write_valid   <= 1

    yield model.dataRxEvent.wait()




@cocotb.test()
def master_test(dut):

    dut.log.info("Cocotb I2C Master controller")

    memory =  {0: 11, 1: 22, 2: 33}
    slaveModel = I2CSlaveModel(dut.io_i2c_sda_write, dut.io_i2c_sda_read, dut.io_i2c_scl, dut.clk, dut.resetn, 4, memory)
 #   slaveModel.startSlave ()



    cocotb.fork(ClockDomainInAsynResetn(dut.clk, dut.resetn))
    cocotb.fork(send_randomData(dut, slaveModel))



    yield Timer(10000000)

    dut.log.info("Cocotb I2C Master controller simulation done")



 # I2C Master signal 
 #   .io_i2c_sda_write(io_i2c_sda_write),
 #   .io_i2c_sda_read(io_i2c_sda_read),
 #   .io_i2c_scl(io_i2c_scl),
 #   .io_read_valid(io_read_valid),
 #   .io_read_payload(io_read_payload),
 #   .io_write_valid(io_write_valid),
 #   .io_write_ready(io_write_ready),
 #   .io_write_payload(io_write_payload),
 #   .io_start(io_start),
 #   .io_read_cmd_valid(io_read_cmd_valid),
 #   .io_read_cmd_ready(io_read_cmd_ready),
 #   .io_addrDevice(io_addrDevice),
 #   .io_errorAck(io_errorAck),
 #   .io_busy(io_busy),
 #   .clk(clk),
 #   .resetn(resetn) 