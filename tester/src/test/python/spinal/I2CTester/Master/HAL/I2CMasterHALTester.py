# Simple tests for an adder module
import cocotb
from cocotb.triggers import Timer, RisingEdge, FallingEdge, Event
#from cocotb.result import TestFailure

from spinal.common.misc import ClockDomainInAsynResetn

from I2CSlaveModelHAL import I2CSlaveModelHAL, I2CSlaveModelHALState


###############################################################################
# Define the different command mode that the master accept
class I2CMasterHALMode:
    WRITE      = 0
    READ       = 1
    WRITECLOSE = 2
    RESTART    = 3
    READCLOSE  = 4
    IDLE       = 5


###############################################################################
# Generate an Event when the cmd.ready signal is high
@cocotb.coroutine
def check_cmd_ready(clk, ready, event):
    while True:
        yield RisingEdge(clk)
        if int(ready) == 1:
            event.set()


###############################################################################
# Execute several read
@cocotb.coroutine
def readTest(dut, readyCmdEvent, model, data2Read):

    if nbrRead == 0:
        return

    dut.io_cmd_valid        <= 0
    dut.io_cmd_payload_mode <= 0
    dut.io_cmd_payload_data <= 0x0

    # Wait the end of the reset
    while True:
        yield RisingEdge(dut.clk)
        if int(dut.resetn) == 1:
            break;

    dut.io_cmd_valid        <= 1
    dut.io_cmd_payload_data <= 0
    dut.io_cmd_payload_mode <= I2CMasterHALMode.READCLOSE

    yield RisingEdge(dut.clk)


###############################################################################
# Write a list a byte and check that the slave as read correctly
# >> writeTest(dut, event, model, [0x33, 0x11,0x22]
@cocotb.coroutine
def writeTest(dut, readyCmdEvent, model, data2Send):

    if len(data2Send) == 0:
        return

    dut.io_cmd_valid        <= 0
    dut.io_cmd_payload_mode <= 0
    dut.io_cmd_payload_data <= 0x0

    # Wait the end of the reset
    while True:
        yield RisingEdge(dut.clk)
        if int(dut.resetn) == 1:
            break;

    for x in range(0, len(data2Send)):

        dut.io_cmd_valid        <= 1
        dut.io_cmd_payload_data <= data2Send[x]

        if x == len(data2Send)-1:
            dut.io_cmd_payload_mode <= I2CMasterHALMode.WRITECLOSE
        else:
            dut.io_cmd_payload_mode <= I2CMasterHALMode.WRITE


        yield readyCmdEvent.wait()
        dut.io_cmd_valid        <= 0

        if x == 0:
            yield model.startEvent.wait()

        cocotb.fork(model._readData())
        yield model.dataTXEvent.wait()
        assert ( model.dataTXEvent.data == data2Send[x], "Data send is not equal to data received " )

    yield model.stopEvent.wait()


    yield RisingEdge(dut.clk)

###############################################################################
# Read the written data by the master
#
@cocotb.coroutine
def readWriteTest(dut,data2Send):

    if len(data2Send) == 0:
        return

    cnt = 0

    while True:
        yield RisingEdge(dut.clk)
        if int(dut.io_rsp_valid) == 1:
            dataRead = int(dut.io_rsp_payload_data)
            ack      = int(dut.io_rsp_payload_ack)
            assert(dataRead != data2Send[cnt], "Data write not equal to data read")
            cnt += 1


###############################################################################
# Test a sequence of write
@cocotb.test()
def master_hal_test_write(dut):

    dut.log.info("Cocotb I2C Master HAL - write Test ")

    cmdReadyEvent = Event()
    modelSlave = I2CSlaveModelHAL(dut.clk, dut.resetn,  dut.io_i2c_sda_write, dut.io_i2c_sda_read, dut.io_i2c_scl_write )
    data2Send = [0xf0, 0x55, 0xAA]

    cocotb.fork(ClockDomainInAsynResetn(dut.clk, dut.resetn))
    cocotb.fork(check_cmd_ready(dut.clk, dut.io_cmd_ready, cmdReadyEvent))
    cocotb.fork(writeTest(dut, cmdReadyEvent, modelSlave, data2Send))
    cocotb.fork(readWriteTest(dut,data2Send))


    # Wait to avoid that the model detect a stop condition a the beginnning
    yield RisingEdge(dut.resetn)
    modelSlave.startSlave()

    yield Timer(3000000)

    dut.log.info("I2C Master HAL - wirte Test done")


###############################################################################
# Test a sequence of read
#@cocotb.test()
def master_hal_test_read(dut):

    dut.log.info("Cocotb I2C Master HAL - read Test ")

    cmdReadyEvent = Event()
    modelSlave = I2CSlaveModelHAL(dut.clk, dut.resetn,  dut.io_i2c_sda_write, dut.io_i2c_sda_read, dut.io_i2c_scl_write )
    data2Read = [0xf0, 0x55, 0xAA]

    cocotb.fork(ClockDomainInAsynResetn(dut.clk, dut.resetn))
    cocotb.fork(ClockDomainInAsynResetn(dut.clk, dut.resetn))
    cocotb.fork(check_cmd_ready(dut.clk, dut.io_cmd_ready, cmdReadyEvent))
    cocotb.fork(readTest(dut, cmdReadyEvent, modelSlave, 1))

    yield Timer(3000000)

    dut.log.info("I2C Master HAL - read Test done")



#.io_i2c_sda_write(io_i2c_sda_write),
#.io_i2c_scl_write(io_i2c_scl_write),
#.io_cmd_valid(io_cmd_valid),
#.io_cmd_ready(io_cmd_ready),
#.io_cmd_payload_mode(io_cmd_payload_mode),
#.io_cmd_payload_data(io_cmd_payload_data),
#.io_rsp_valid(io_rsp_valid),
#.io_rsp_payload_ack(io_rsp_payload_ack),
#.io_rsp_payload_data(io_rsp_payload_data),
#.clk(clk),
#.resetn(resetn)