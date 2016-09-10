###############################################################################
# Test for the I2C Slave HAL
#
###############################################################################
from cocotb.triggers import Timer

from I2CMasterModelHAL import I2CMasterModelHAL
from cocotblib.ClockDomain import ClockDomain, RESET_ACTIVE_LEVEL
from spinal.I2CTester.HAL.I2CHAL      import *
from spinal.I2CTester.HAL.I2CSlaveHAL import I2CSlaveHAL


###############################################################################
# Basic tests
@cocotb.test()
def slave_hal_basic_test(dut):

    dut.log.info("Cocotb I2C Slave HAL - Basic tests")

    # list all scenarios
    listOperation = list()
    listOperation.append( [START(), WRITE(), ACK(), STOP()] )
    listOperation.append( [START(), READ(), NACK(), STOP()] )
    listOperation.append( [START(), READ(),  ACK(), READ(),  NACK(), STOP()] )
    listOperation.append( [START(), WRITE(), ACK(), WRITE(), NACK(), STOP()])
    listOperation.append( [START(), READ(),  ACK(), WRITE(), NACK(), STOP()])
    listOperation.append( [START(), WRITE(), ACK(), READ(),  NACK(), STOP()] )
    listOperation.append( [START(), WRITE(), ACK(), START(), READ(), NACK(), STOP()]  )
    listOperation.append( [START(), READ(),  ACK(), START(), WRITE(), NACK(), STOP()] )


    # Run all scenarios
    for operationSeq in listOperation :

        helperSlave  = I2CSlaveHAL(dut)
        modelMaster  = I2CMasterModelHAL(helperSlave, 50)
        analyser     = I2CHALAnalyser(helperSlave, operationSeq)
        clockDomain  = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

        # Start clock
        cocotb.fork(clockDomain.start())

        # Init IO and wait the end of the reset
        helperSlave.io.init()
        yield clockDomain.event_endReset.wait()

        # run
        cocotb.fork(helperSlave.execOperations(operationSeq))
        cocotb.fork(modelMaster.startMaster(operationSeq))
        cocotb.fork(analyser.start())
        yield helperSlave.checkResponse(operationSeq)


        yield Timer(500000)

        # stop all processes
        clockDomain.stop()
        helperSlave.stop()
        modelMaster.stop()
        analyser.stop()

        yield Timer(500000)


    dut.log.info("I2C Slave HAL - basic tests done")


