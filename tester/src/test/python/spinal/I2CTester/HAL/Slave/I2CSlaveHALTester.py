###############################################################################
# Test for the I2C Slave HAL
#
###############################################################################

import cocotb
from cocotb.triggers import Timer, RisingEdge, FallingEdge, Event

from spinal.common.misc import assertEquals
from spinal.common.ClockDomain import ClockDomain, RESET_ACTIVE_LEVEL


from spinal.I2CTester.HAL.I2CSlaveHAL import I2CSlaveHAL
from spinal.I2CTester.HAL.I2CHAL import *

from I2CMasterModelHAL import I2CMasterModelHAL



###############################################################################
# Test a sequence of write
@cocotb.test()
def slave_hal_basic_test(dut):

    dut.log.info("Cocotb I2C Slave HAL - write Test ")

    listOperation = list()

    listOperation.append( [START(), WRITE(0x44), ACK(), STOP()] )
    listOperation.append( [START(), READ(0x11), NACK(), STOP()] )
    listOperation.append( [START(), READ(0x11),  ACK(), READ(0xAA), NACK(), STOP()] )
    listOperation.append( [START(), WRITE(0x8A), ACK(), WRITE(0x55), NACK(), STOP()])
    listOperation.append( [START(), READ(0xAA),  ACK(), WRITE(0x55), NACK(), STOP()])
    listOperation.append( [START(), WRITE(0xAA), ACK(), READ(0x55), NACK(), STOP()] )
    listOperation.append( [START(), WRITE(0xAA), ACK(), START(), READ(0x55), NACK(), STOP()]  )
    listOperation.append( [START(), READ(0x01),  ACK(), START(), WRITE(0x55), NACK(), STOP()] )


    operationSeq = listOperation[7]

    helperSlave  = I2CSlaveHAL(dut)
    modelMaster  = I2CMasterModelHAL(helperSlave, 50)
    analyser     = I2CHALAnalyser(helperSlave, operationSeq)
    clockDomain  = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

    # Start clock
    cocotb.fork(clockDomain.start())

    # Init IO and wait the end of the reset
    helperSlave.io.init()
    yield clockDomain.event_endReset.wait()

    #
    cocotb.fork(helperSlave.execOperations(operationSeq))
    cocotb.fork(modelMaster.startMaster(operationSeq))
    cocotb.fork(analyser.start())
    yield helperSlave.checkResponse(operationSeq)


    yield Timer(1000000)

    dut.log.info("I2C Slave HAL - write Test done")


