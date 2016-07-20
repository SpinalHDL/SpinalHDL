###############################################################################
# Test for the I2C Master HAL
#
###############################################################################

import cocotb
from cocotb.triggers import Timer, RisingEdge, FallingEdge, Event


from spinal.common.ClockDomain         import ClockDomain, RESET_ACTIVE_LEVEL
from spinal.I2CTester.HAL.I2CMasterHAL import I2CMasterHAL
from spinal.I2CTester.HAL.I2CHAL       import *

from I2CSlaveModelHAL import I2CSlaveModelHAL

###############################################################################
# Basic test
@cocotb.test()
def master_hal_basic_tests(dut):

    dut.log.info("Cocotb I2C Master HAL - Basic Test ")

    delayBetweenCMD = 300000

    listOperation = list()
    listOperation.append( [START(delayBetweenCMD), WRITE(), ACK(), STOP()]  )
    listOperation.append( [START(), READ(),  ACK(delayBetweenCMD), STOP()] )
    listOperation.append( [START(), WRITE(-1,delayBetweenCMD), NACK(), WRITE(), NACK(), STOP()] )
    listOperation.append( [START(), READ(),  ACK(),  READ(),  NACK(delayBetweenCMD), STOP()] )
    listOperation.append( [START(), WRITE(), ACK(), START(delayBetweenCMD), READ(),  NACK(), STOP(delayBetweenCMD)] )
    listOperation.append( [START(), READ(-1,delayBetweenCMD),  NACK(), START(), WRITE(), NACK(),  STOP()] )

    for operationSeq in listOperation:

        helperMaster = I2CMasterHAL(dut)
        analyser     = I2CHALAnalyser(helperMaster, operationSeq)
        modelSlave   = I2CSlaveModelHAL(helperMaster)

        clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

        cocotb.fork(clockDomain.start())

        # Init IO and wait the end of the reset
        sclClockDivider = 50
        helperMaster.io.init(sclClockDivider)
        yield clockDomain.event_endReset.wait()

        # run
        cocotb.fork(modelSlave.startSlave(operationSeq))
        cocotb.fork(helperMaster.execOperations(operationSeq))
        cocotb.fork(analyser.start())
        yield helperMaster.checkResponse(operationSeq)


        yield Timer(500000)

        # kill all processes
        clockDomain.stop()
        helperMaster.stop()
        modelSlave.stop()
        analyser.stop()

        yield Timer(500000)


    dut.log.info("I2C Master HAL - Basic Test done")


