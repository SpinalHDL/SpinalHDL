###############################################################################
# Test for the I2C Master HAL
#
###############################################################################

import cocotb
from cocotb.triggers import Timer, RisingEdge, FallingEdge, Event

from spinal.common.misc import assertEquals
from spinal.common.ClockDomain import ClockDomain, RESET_ACTIVE_LEVEL
from I2CSlaveModelHAL import I2CSlaveModelHAL


###############################################################################
# Define the different command mode that the master accept
class I2CMasterHALMode:
    START = 0
    WRITE = 1
    READ  = 2
    STOP  = 3


###############################################################################
# Generate an Event when the cmd.ready signal is high
@cocotb.coroutine
def check_cmd_ready(clk, ready, event):
    while True:
        yield RisingEdge(clk)
        if int(ready) == 1:
            event.set()


###############################################################################
# Execute write restart read sequence
@cocotb.coroutine
def writeRestartReadTest(dut, readyCmdEvent, model ):

    dut.io_cmd_valid        <= 0
    dut.io_cmd_payload_mode <= 0
    dut.io_cmd_payload_data <= 0x0

    # Wait the end of the reset
    while True:
        yield RisingEdge(dut.clk)
        if int(dut.resetn) == 1:
            break;

    # Start
    dut.io_cmd_valid        <= 1
    dut.io_cmd_payload_mode <= I2CMasterHALMode.START
    dut.io_cmd_payload_data <= 0x0

    yield readyCmdEvent.wait()
    dut.io_cmd_valid  <= 0

    yield model.startEvent.wait()

    # Write
    dut.io_cmd_valid        <= 1
    dut.io_cmd_payload_mode <= I2CMasterHALMode.WRITE
    dut.io_cmd_payload_data <= 0x55

    yield readyCmdEvent.wait()
    dut.io_cmd_valid <= 0

    cocotb.fork(model.readData())
    yield model.dataTXEvent.wait()

    dut.io_cmd_valid        <= 1
    dut.io_cmd_payload_mode <= I2CMasterHALMode.START
    dut.io_cmd_payload_data <= 0x55

    yield readyCmdEvent.wait()
    dut.io_cmd_valid  <= 0

    yield model.startEvent.wait()

    # Read
    dut.io_cmd_valid        <= 1
    dut.io_cmd_payload_mode <= I2CMasterHALMode.READ
    dut.io_cmd_payload_data <= 0x0

    yield readyCmdEvent.wait()
    dut.io_cmd_valid  <= 0

    yield RisingEdge(dut.io_i2c_scl_write)

    cocotb.fork(model.writeData(0x25))
    yield model.dataRxEvent.wait()


    # Send stop sequence
    dut.io_cmd_valid        <= 1
    dut.io_cmd_payload_mode <= I2CMasterHALMode.STOP
    dut.io_cmd_payload_data <= 0

    yield readyCmdEvent.wait()
    dut.io_cmd_valid <= 0

    yield RisingEdge(dut.clk)





###############################################################################
# Execute several read
@cocotb.coroutine
def readTest(dut, readyCmdEvent, model, data2Read):

    if len(data2Read) == 0:
        return

    dut.io_cmd_valid        <= 0
    dut.io_cmd_payload_mode <= 0
    dut.io_cmd_payload_data <= 0x0

    # Wait the end of the reset
    while True:
        yield RisingEdge(dut.clk)
        if int(dut.resetn) == 1:
            break;

    # Start
    dut.io_cmd_valid        <= 1
    dut.io_cmd_payload_mode <= I2CMasterHALMode.START
    dut.io_cmd_payload_data <= 0x0

    yield readyCmdEvent.wait()
    dut.io_cmd_valid  <= 0

    yield model.startEvent.wait()

    for index in range(0, len(data2Read)):

        dut.io_cmd_valid        <= 1
        dut.io_cmd_payload_mode <= I2CMasterHALMode.READ
        dut.io_cmd_payload_data <= 0x0

        yield readyCmdEvent.wait()
        dut.io_cmd_valid  <= 0

        if index != 0:
            yield RisingEdge(dut.io_i2c_scl_write)

        cocotb.fork(model.writeData(data2Read[index]))
        yield model.dataRxEvent.wait()


    # Send stop sequence
    dut.io_cmd_valid        <= 1
    dut.io_cmd_payload_mode <= I2CMasterHALMode.STOP
    dut.io_cmd_payload_data <= 0

    yield readyCmdEvent.wait()
    dut.io_cmd_valid <= 0

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


    # Send the START sequence
    dut.io_cmd_valid        <= 1
    dut.io_cmd_payload_mode <= I2CMasterHALMode.START
    dut.io_cmd_payload_data <= 0

    yield readyCmdEvent.wait()
    dut.io_cmd_valid  <= 0

    yield model.startEvent.wait()

    # send all data
    for index in range(0, len(data2Send)):

        dut.io_cmd_valid        <= 1
        dut.io_cmd_payload_mode <= I2CMasterHALMode.WRITE
        dut.io_cmd_payload_data <= data2Send[index]

        yield readyCmdEvent.wait()
        dut.io_cmd_valid <= 0

        cocotb.fork(model.readData())
        yield model.dataTXEvent.wait()

        assertEquals(model.dataTXEvent.data, data2Send[index], "Data Send by Master is not equal to Data read by the Slave")

    # Send stop sequence
    dut.io_cmd_valid        <= 1
    dut.io_cmd_payload_mode <= I2CMasterHALMode.STOP
    dut.io_cmd_payload_data <= 0

    yield readyCmdEvent.wait()
    dut.io_cmd_valid <= 0

    yield RisingEdge(dut.clk)


###############################################################################
# Read the written data by the master and compare with the data send
#
@cocotb.coroutine
def checkWriteTest(dut,data2Send):

    if len(data2Send) == 0:
        return

    cnt = 0
    while True:
        yield RisingEdge(dut.clk)
        if int(dut.io_rsp_valid):
            dataRead = int(dut.io_rsp_payload_data)
            assertEquals(dataRead,data2Send[cnt], "Data write not equal to data read")
            cnt += 1



###############################################################################
# Check the data received by the master compare with the data send by the slave
#
@cocotb.coroutine
def checkReadTest(dut, data2Read):

    if len(data2Read) == 0 :
        return

    cnt = 0
    while True:
        yield RisingEdge(dut.io_rsp_valid)
        yield RisingEdge(dut.clk)
        data  = int(dut.io_rsp_payload_data)
        assertEquals(data, data2Read[cnt], "Data Read by the master is not equal to the data write by the slave")
        cnt += 1



###############################################################################
# Test a sequence of write
@cocotb.test()
def master_hal_test_write(dut):

    dut.log.info("Cocotb I2C Master HAL - write Test ")

    cmdReadyEvent = Event()
    modelSlave    = I2CSlaveModelHAL(dut.clk, dut.resetn,  dut.io_i2c_sda_write, dut.io_i2c_sda_read, dut.io_i2c_scl_write )
    data2Send     = [0xf0, 0x55]

    clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

    cocotb.fork(clockDomain.start())
    cocotb.fork(check_cmd_ready(dut.clk, dut.io_cmd_ready, cmdReadyEvent))
    cocotb.fork(writeTest(dut, cmdReadyEvent, modelSlave, data2Send))
    cocotb.fork(checkWriteTest(dut,data2Send))


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
    modelSlave    = I2CSlaveModelHAL(dut.clk, dut.resetn,  dut.io_i2c_sda_write, dut.io_i2c_sda_read, dut.io_i2c_scl_write )
    data2Read     = [0x04, 0x85, 0xFF]

    clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

    cocotb.fork(clockDomain.start())
    cocotb.fork(check_cmd_ready(dut.clk, dut.io_cmd_ready, cmdReadyEvent))
    cocotb.fork(readTest(dut, cmdReadyEvent, modelSlave, data2Read))
    cocotb.fork(checkReadTest(dut, data2Read))

    # Wait to avoid that the model detect a stop condition after the reset
    yield RisingEdge(dut.resetn)
    modelSlave.startSlave()

    yield Timer(3000000)

    dut.log.info("I2C Master HAL - read Test done")


###############################################################################
# Test a sequence of write read (try the restart sequence)
#@cocotb.test()
def master_hal_test_writeRestartRead(dut):

    dut.log.info("Cocotb I2C Master HAL - write restart read Test ")

    cmdReadyEvent = Event()
    modelSlave    = I2CSlaveModelHAL(dut.clk, dut.resetn,  dut.io_i2c_sda_write, dut.io_i2c_sda_read, dut.io_i2c_scl_write )
    #data2Read     = [0x05, 0x45, 0x00]

    clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

    cocotb.fork(clockDomain.start())
    cocotb.fork(check_cmd_ready(dut.clk, dut.io_cmd_ready, cmdReadyEvent))
    cocotb.fork(writeRestartReadTest(dut, cmdReadyEvent, modelSlave))


    # Wait to avoid that the model detect a stop condition after the reset
    yield RisingEdge(dut.resetn)
    modelSlave.startSlave()

    yield Timer(4000000)

    dut.log.info("I2C Master HAL - write restart read Test done")

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