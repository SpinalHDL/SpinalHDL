###############################################################################
# Test for the I2C Master HAL
#
###############################################################################

import cocotb
from cocotb.triggers import Timer, RisingEdge, FallingEdge, Event


from spinal.common.ClockDomain import ClockDomain, RESET_ACTIVE_LEVEL
from spinal.I2CTester.I2CMasterHAL import *
from spinal.I2CTester.I2CHAL import *
from spinal.I2CTester.Master.I2CSlaveModelHAL import *



###############################################################################
# Test a sequence of write
@cocotb.test()
def master_hal_basic_tests(dut):

    dut.log.info("Cocotb I2C Master HAL - write Test ")

    listOperation = [START(), WRITE(0x44), ACK(), STOP()]
    #listOperation = [START(), READ(0x11),  ACK(), STOP()]
    #listOperation = [START(), WRITE(0x44), NACK(), WRITE(0x44), NACK(), STOP()]
    #listOperation = [START(), READ(0x11),  ACK(),  READ(0x11),  NACK(), STOP()]
    #listOperation = [START(), WRITE(0x44), NACK(), START(), READ(0x44),  NACK(), STOP()]
    #listOperation = [START(), READ(0x44),  ACK(),  START(), WRITE(0x44), ACK(),  STOP()]

    helperMaster = I2CMasterHAL(dut)
    analyser     = I2CHALAnalyser()
    modelSlave   = I2CSlaveModelHAL(dut.clk, dut.resetn,  dut.io_i2c_sda_write, dut.io_i2c_sda_read, dut.io_i2c_scl_write )


    clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

    cocotb.fork(clockDomain.start())

    # Init IO and wait the end of the reset
    io.init()
    yield clockDomain.event_endReset.wait()


    cocotb.fork(helperMaster.execOperations(listOperation))
    cocotb.fork(helperMaster.checkResponse(listOperation))
    cocotb.fork(analyser.start())


    # Wait to avoid that the model detect a stop condition a the beginning
    modelSlave.startSlave()

    yield Timer(3000000)

    dut.log.info("I2C Master HAL - wirte Test done")


