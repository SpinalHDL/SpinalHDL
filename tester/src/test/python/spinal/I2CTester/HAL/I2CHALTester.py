###############################################################################
# Test for the I2C  HAL
#
###############################################################################

import cocotb
from cocotb.triggers import Timer, RisingEdge, FallingEdge, Event

from spinal.common.ClockDomain import ClockDomain, RESET_ACTIVE_LEVEL

from spinal.I2CTester.HAL.I2CSlaveHAL import I2CSlaveHAL
from spinal.I2CTester.HAL.I2CMasterHAL import I2CMasterHAL
from spinal.I2CTester.HAL.I2CHAL import *


###############################################################################
# Test a sequence of read
@cocotb.test()
def test_scenario_1(dut):

    dut.log.info("Cocotb I2C HAL - Basic test")


    delay = 300000

    listOperation = list()

    listOperation.append( [START(), WRITE(0x88, 0, delay), ACK(), STOP()] )
    listOperation.append( [START(), READ(), NACK(), STOP()] )
    listOperation.append( [START(), READ(),  ACK(), READ(),  NACK(), STOP()] )
    listOperation.append( [START(), WRITE(), ACK(), WRITE(), NACK(), STOP()])
    listOperation.append( [START(), READ(),  ACK(), WRITE(), NACK(), STOP()])
    listOperation.append( [START(), WRITE(), ACK(), READ(),  NACK(), STOP()] )
    listOperation.append( [START(), WRITE(), ACK(), START(), READ(), NACK(), STOP()]  )
    listOperation.append( [START(), READ(),  ACK(), START(), WRITE(), NACK(), STOP()] )

    for operationSeq in listOperation:

        helperMaster = I2CMasterHAL(dut, True)
        helperSlave  = I2CSlaveHAL(dut, True)
        #analyser     = I2CHALAnalyser(helperMaster, operationSeq)

        clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)
        cocotb.fork(clockDomain.start())

        # Init IO and wait the end of the reset
        sclClockDivider = 50
        helperMaster.io.init(sclClockDivider)
        helperSlave.io.init()
        yield clockDomain.event_endReset.wait()


      #  cocotb.fork(analyser.start())
        cocotb.fork(helperMaster.execOperations(operationSeq))
        cocotb.fork(helperMaster.checkResponse(operationSeq))
        cocotb.fork(helperSlave.execOperations(operationSeq))
        yield helperSlave.checkResponse(operationSeq)

        yield Timer(500000)

        # Stop all processes
        clockDomain.stop()
        helperSlave.stop()
        helperMaster.stop()
       # analyser.stop()

        yield Timer(500000)


    dut.log.info("Cocotb I2C HAL - Basic test")
