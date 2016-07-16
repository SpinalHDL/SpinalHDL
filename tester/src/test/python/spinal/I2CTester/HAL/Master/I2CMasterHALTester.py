###############################################################################
# Test for the I2C Master HAL
#
###############################################################################

import cocotb
from cocotb.triggers import Timer, RisingEdge, FallingEdge, Event

from spinal.common.misc import assertEquals
from spinal.common.ClockDomain import ClockDomain, RESET_ACTIVE_LEVEL


from spinal.I2CTester.HAL.I2CMasterHAL import I2CMasterHAL
from spinal.I2CTester.HAL.I2CHAL import *

from I2CSlaveModelHAL import I2CSlaveModelHAL



###############################################################################
# Test a sequence of write
@cocotb.test()
def master_hal_basic_tests(dut):

    dut.log.info("Cocotb I2C Master HAL - write Test ")

    listOperation = [START(), WRITE(0x45), NACK(), STOP()]
    listOperation = [START(), READ(0x11),  NACK(), STOP()]
    listOperation = [START(), WRITE(0x44), NACK(), WRITE(0x11), NACK(), STOP()]
    listOperation = [START(), READ(0x11),  ACK(),  READ(0x45),  NACK(), STOP()]
    listOperation = [START(), WRITE(0x44), NACK(), START(), READ(0x88),  NACK(), STOP()]
    listOperation = [START(), READ(0x33),  ACK(),  START(), WRITE(0x22), ACK(),  STOP()]

    helperMaster = I2CMasterHAL(dut)
 #   analyser     = I2CHALAnalyser(helperMaster, listOperation)
    modelSlave   = I2CSlaveModelHAL(helperMaster)

    clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

    cocotb.fork(clockDomain.start())

    # Init IO and wait the end of the reset
    helperMaster.io.init()
    yield clockDomain.event_endReset.wait()


    cocotb.fork(modelSlave.startSlave(listOperation))
    cocotb.fork(helperMaster.execOperations(listOperation))
    cocotb.fork(helperMaster.checkResponse(listOperation))
    #cocotb.fork(analyser.start())



    yield Timer(3000000)

    dut.log.info("I2C Master HAL - wirte Test done")


