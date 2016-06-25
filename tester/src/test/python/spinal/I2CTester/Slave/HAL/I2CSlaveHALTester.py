###############################################################################
# Test for the I2C Slave HAL
#
###############################################################################

import cocotb
from cocotb.triggers import Timer, RisingEdge, FallingEdge, Event

from spinal.common.misc import assertEquals
from spinal.common.ClockDomain import ClockDomain, RESET_ACTIVE_LEVEL
from I2CMasterModelHAL import I2CMasterModelHAL



class RspMode :
    DATA = 0
    NONE = 1
    ACK  = 2


@cocotb.coroutine
def write_test(dut, model, eventCmdValid):

    dut.io_cmd_ready        <= 0
    dut.io_rsp_payload_mode <= 0
    dut.io_rsp_valid        <= 0
    dut.io_rsp_payload_data <= 0

    # wait end of the rest
    yield RisingEdge(dut.resetn)

    # gen start
    cocotb.fork( model.genStart() )
    yield eventCmdValid.wait()

    dut.io_cmd_ready <= 1
    yield RisingEdge(dut.clk)
    dut.io_cmd_ready <= 0

    yield RisingEdge(dut.clk)

    dut.io_rsp_valid <= 1
    dut.io_rsp_payload_mode <= RspMode.NONE

    yield RisingEdge(dut.clk)

    dut.io_rsp_valid <= 0

    cocotb.fork(  model.writeData(0x33) )

    yield eventCmdValid.wait()

    dut.io_cmd_ready <= 1
    yield RisingEdge(dut.clk)
    dut.io_cmd_ready <= 0
    yield RisingEdge(dut.clk)

    dut.io_rsp_valid <= 1
    dut.io_rsp_payload_mode <= RspMode.ACK


    yield RisingEdge(dut.clk)

    dut.io_rsp_valid <= 0

    yield model.sclFalling.wait()

    cocotb.fork(model.genStop())

    yield eventCmdValid.wait()







@cocotb.coroutine
def monitor_cmd_valid(dut, event):
    while True:
        yield RisingEdge(dut.clk)
        if int(dut.io_cmd_valid) == 1:
            event.set()



###############################################################################
# Test a sequence of write
@cocotb.test()
def slave_hal_test_write(dut):

    dut.log.info("Cocotb I2C Slave HAL - write Test ")


    cmdValidEvent = Event()

    modelMaster = I2CMasterModelHAL(dut.clk, dut.io_i2c_scl_read, dut.io_i2c_sda_read, dut.io_i2c_scl_write, dut.io_i2c_sda_write, 50)

    clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

    cocotb.fork(clockDomain.start())
    cocotb.fork(monitor_cmd_valid(dut, cmdValidEvent))
    cocotb.fork(write_test(dut, modelMaster, cmdValidEvent))
    modelMaster.startMaster()


    # Wait to avoid that the model detect a stop condition a the beginnning
    # yield RisingEdge(dut.resetn)


    yield Timer(3000000)

    dut.log.info("I2C Slave HAL - write Test done")


#    .io_i2c_sda_write(io_i2c_sda_write),
#    .io_i2c_sda_read(io_i2c_sda_read),
#    .io_i2c_scl_write(io_i2c_scl_write),
#    .io_i2c_scl_read(io_i2c_scl_read),
#    .io_cmd_valid(io_cmd_valid),
#    .io_cmd_payload_mode(io_cmd_payload_mode),
#    .io_cmd_payload_data(io_cmd_payload_data),
#    .io_rsp_valid(io_rsp_valid),
#    .io_rsp_ready(io_rsp_ready),
#    .io_rsp_payload_mode(io_rsp_payload_mode),
#    .clk(clk),
#    .resetn(resetn)
