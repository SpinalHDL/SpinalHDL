# Simple tests for an adder module
import cocotb
from cocotb.triggers import Timer, RisingEdge, FallingEdge, Event
from cocotb.result import TestFailure

from spinal.common.misc import ClockDomainInAsynResetn
from I2CSlaveModel import I2CSlaveModel

@cocotb.coroutine
def readData(dut, model, addrDevice, nbrRead):

    dut.io_start      <= 0
    dut.io_addrDevice <= 0
    dut.io_read_cmd_valid <= 0

    yield RisingEdge(dut.clk)

    dut.io_start      <= 1
    dut.io_addrDevice <= addrDevice

    yield RisingEdge(dut.clk)

    dut.io_start         <= 0

    yield RisingEdge(dut.clk)


    for x in range(0, nbrRead):
        dut.io_read_cmd_valid <= 1

        yield FallingEdge(dut.io_read_cmd_ready)

        if x < nbrRead-1 :
            dut.io_read_cmd_valid <= 1
        else:
            dut.io_read_cmd_valid <= 0

        yield model.dataRxEvent.wait()

        res = model.dataRxEvent.data

        print("data received..", res['data'], res['ack'])


    dut.io_read_cmd_valid <= 0



@cocotb.coroutine
def writeRandomData(dut, model, addrDevice,  data2send):

    dut.io_start      <= 0
    dut.io_addrDevice <= 0
    dut.io_read_valid <= 0
    
    yield RisingEdge(dut.clk)

    dut.io_start      <= 1
    dut.io_addrDevice <= addrDevice

    yield RisingEdge(dut.clk)

    dut.io_start         <= 0


    dut.io_write_payload <= data2send[0]
    dut.io_write_valid   <= 1

    yield model.dataTXEvent.wait()

    # check address
    dataRX = model.dataTXEvent.data[:-1]
    assert addrDevice == int("".join([str(x) for x in dataRX]), 2)

    for index in range(1, len(data2send)):
        dut.io_write_payload <= data2send[index]
        dut.io_write_valid   <= 1

        yield model.dataTXEvent.wait()

        assert data2send[index-1] == int("".join([str(x) for x in model.dataTXEvent.data]), 2)

    dut.io_write_valid <= 0

    yield model.dataTXEvent.wait()

   ## assert data2send[len(data2send)-1] == int("".join([str(x) for x in model.dataTXEvent.data]), 2)

    dut.io_write_valid <= 0

    yield RisingEdge(dut.clk)



@cocotb.test()
def master_test(dut):

    dut.log.info("Cocotb I2C Master controller")

    memory =  {0: 11, 1: 22, 2: 33}
    slaveModel = I2CSlaveModel(dut.io_i2c_sda_write, dut.io_i2c_sda_read, dut.io_i2c_scl, dut.clk, dut.resetn, 7, memory)
    slaveModel.startSlave ()

    cocotb.fork(ClockDomainInAsynResetn(dut.clk, dut.resetn))
    cocotb.fork(writeRandomData(dut, slaveModel, 3,  [2,0,255,123]))
    #cocotb.fork(readData(dut, slaveModel, 4,  3))



    yield Timer(30000000)

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
 #   .io_read_cmd_ready(io_read_cmd_ready),
 #   .io_addrDevice(io_addrDevice),
 #   .io_errorAck(io_errorAck),
 #   .io_busy(io_busy),
 #   .clk(clk),
 #   .resetn(resetn) 