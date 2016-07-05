###############################################################################
# Test for the I2C Slave HAL
#
###############################################################################

import cocotb
from cocotb.triggers import Timer, RisingEdge, FallingEdge, Event

from spinal.common.misc import assertEquals
from spinal.common.ClockDomain import ClockDomain, RESET_ACTIVE_LEVEL
from I2CMasterModelHAL import I2CMasterModelHAL


###############################################################################
# Rsp mode
class RspMode :
    DATA = 0
    NONE = 1
    ACK  = 2

###############################################################################
# Cmd mode
class CmdMode :
    START = 0
    NACK  = 1
    ACK   = 2
    STOP  = 3
    DATA  = 4


###############################################################################
# Test the read
@cocotb.coroutine
def read_test(dut, model, eventCmdValid, rspReadyEvent, data):

    dut.io_rsp_payload_mode <= 0
    dut.io_rsp_valid        <= 0
    dut.io_rsp_payload_data <= 0

    # wait end of the rest
    yield RisingEdge(dut.resetn)

    # Start -------------------------------------------------------------------
    yield eventCmdValid.wait()

    # Read --------------------------------------------------------------------
    for i in range(0,len(data)):

        dut.io_rsp_valid        <= 1
        dut.io_rsp_payload_mode <= RspMode.DATA
        dut.io_rsp_payload_data <= data[i]

        yield rspReadyEvent.wait()

        dut.io_rsp_valid <= 0

        yield RisingEdge(dut.clk)

        # ACK
        dut.io_rsp_valid        <= 1
        dut.io_rsp_payload_mode <= RspMode.NONE

        yield rspReadyEvent.wait()

        dut.io_rsp_valid <= 0

        yield RisingEdge(dut.clk)

    # STOP --------------------------------------------------------------------

    dut.io_rsp_valid        <= 1
    dut.io_rsp_payload_mode <= RspMode.NONE

    yield rspReadyEvent.wait()

    dut.io_rsp_valid <= 0

    yield RisingEdge(dut.clk)



###############################################################################
# Check command received during the write operation
@cocotb.coroutine
def check_write_cmd( eventCmdValid, data2Write):

    yield eventCmdValid.wait()

    assertEquals(eventCmdValid.data["mode"], CmdMode.START, "START : Cmd mode received wrong")

    for data in data2Write:

        yield eventCmdValid.wait()

        assertEquals(eventCmdValid.data["mode"], CmdMode.DATA, "DATA : Cmd mode received wrong")
        assertEquals(eventCmdValid.data["data"], data, "Data send by the Master is not equal to the data read by the slave")

        yield eventCmdValid.wait()

        assertEquals(eventCmdValid.data["mode"], CmdMode.ACK, "ACK : Cmd mode received wrong")


    yield eventCmdValid.wait()

    assertEquals(eventCmdValid.data["mode"], CmdMode.STOP, "STOP : Cmd mode received wrong")


###############################################################################
# Test the write
@cocotb.coroutine
def write_test(dut, model, eventCmdValid, rspReadyEvent, data):

    dut.io_rsp_payload_mode <= 0
    dut.io_rsp_valid        <= 0
    dut.io_rsp_payload_data <= 0

    # wait end of the rest
    yield RisingEdge(dut.resetn)

    # Start -------------------------------------------------------------------
    yield eventCmdValid.wait()

    # Write -------------------------------------------------------------------
    for _ in data:

         dut.io_rsp_valid        <= 1
         dut.io_rsp_payload_mode <= RspMode.NONE

         yield rspReadyEvent.wait()
         dut.io_rsp_valid <= 0

         # ACK ---------------------------------------------------------------------
         dut.io_rsp_valid <= 1
         dut.io_rsp_payload_mode <= RspMode.ACK

         yield rspReadyEvent.wait()
         dut.io_rsp_valid <= 0

         yield RisingEdge(dut.clk)

    # STOP --------------------------------------------------------------------

    dut.io_rsp_valid        <= 1
    dut.io_rsp_payload_mode <= RspMode.NONE

    yield rspReadyEvent.wait()

    dut.io_rsp_valid <= 0




###############################################################################
# Detect the cmd valid signal
@cocotb.coroutine
def monitor_cmd_valid(dut, event):
    while True:
        yield RisingEdge(dut.clk)
        if int(dut.io_cmd_valid) == 1:

            result = dict()
            result["data"]  = int(dut.io_cmd_payload_data)
            result["mode"]  = int(dut.io_cmd_payload_mode)

            event.set(result)


###############################################################################
# Detect the RSP ready signal
@cocotb.coroutine
def monitor_rsp_ready(dut, event):
    while True:
        yield RisingEdge(dut.clk)
        if int(dut.io_rsp_ready) == 1:
            event.set()



###############################################################################
# Test a sequence of write
@cocotb.test()
def slave_hal_test_write(dut):

    dut.log.info("Cocotb I2C Slave HAL - write Test ")

    cmdValidEvent = Event()
    rspReadyEvent = Event()

    data2Write = [0x65, 0x33]

    modelMaster = I2CMasterModelHAL(dut.clk, dut.io_i2c_scl_read, dut.io_i2c_sda_read, dut.io_i2c_scl_write, dut.io_i2c_sda_write, 50)

    clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

    cocotb.fork(clockDomain.start())
    cocotb.fork(monitor_cmd_valid(dut, cmdValidEvent))
    cocotb.fork(monitor_rsp_ready(dut, rspReadyEvent))
    cocotb.fork(write_test(dut, modelMaster, cmdValidEvent,rspReadyEvent, data2Write))
    cocotb.fork(check_write_cmd(cmdValidEvent, data2Write))

    modelMaster.startMaster()
    modelMaster.writeTest(data2Write)

    yield Timer(3000000)

    dut.log.info("I2C Slave HAL - write Test done")



###############################################################################
# Test a sequence of read
@cocotb.test()
def slave_hal_test_read(dut):

    dut.log.info("Cocotb I2C Slave HAL - read Test ")

    cmdValidEvent = Event()
    rspReadyEvent = Event()

    data2Read = [0x34,0xAA]

    modelMaster = I2CMasterModelHAL(dut.clk, dut.io_i2c_scl_read, dut.io_i2c_sda_read, dut.io_i2c_scl_write, dut.io_i2c_sda_write, 50)

    clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

    cocotb.fork(clockDomain.start())
    cocotb.fork(monitor_cmd_valid(dut, cmdValidEvent))
    cocotb.fork(monitor_rsp_ready(dut, rspReadyEvent))
    cocotb.fork(read_test(dut, modelMaster, cmdValidEvent,rspReadyEvent, data2Read))
    cocotb.fork(check_write_cmd(cmdValidEvent, data2Read))


    modelMaster.startMaster()
    modelMaster.readTest(data2Read)

    yield Timer(3000000)

    dut.log.info("I2C Slave HAL - read Test done")