###############################################################################
# Test for the I2C  Io layer
#
###############################################################################
from cocotb.triggers import Timer

from cocotblib.ClockDomain import ClockDomain, RESET_ACTIVE_LEVEL
from spinal.I2CTester.iolayer.I2CIoLayer       import *
from spinal.I2CTester.iolayer.I2CMasterIoLayer import I2CMasterIoLayer
from spinal.I2CTester.iolayer.I2CSlaveIoLayer  import I2CSlaveIoLayer


###############################################################################
# Test (Scenario 1)
@cocotb.test()
def test_scenario(dut):

    dut.log.info("Cocotb I2C IO Layer - Basic test")
    from cocotblib.misc import cocotbXHack
    cocotbXHack()

    delay = 300000

    listOperation = list()

    listOperation.append( [START(), WRITE_BIT(0), WRITE_BIT(1), WRITE_BIT(0), STOP()] )
    listOperation.append( [START(), READ_BIT(0),  READ_BIT(1),  STOP()] )
    listOperation.append( [START(), WRITE_BIT(), READ_BIT(),  STOP()] )
    listOperation.append( [START(), READ_BIT(0), START(), WRITE_BIT(0), STOP()] )



    for operationSeq in listOperation:

        helperMaster = I2CMasterIoLayer(dut)
        helperSlave  = I2CSlaveIoLayer(dut)
        #analyser     = I2CHALAnalyser(helperMaster, operationSeq)

        clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)
        cocotb.fork(clockDomain.start())

        # Init IO and wait the end of the reset
        helperMaster.io.init()
        helperSlave.io.init()
        yield clockDomain.event_endReset.wait()


        #cocotb.fork(analyser.start())
        cocotb.fork(helperMaster.execOperations(operationSeq))
        #cocotb.fork(helperMaster.checkResponse(operationSeq))
        cocotb.fork(helperSlave.execOperations(operationSeq))
        #yield helperSlave.checkResponse(operationSeq)

        yield Timer(500000)

        # Stop all processes
        clockDomain.stop()
        helperSlave.stop()
        helperMaster.stop()
       # analyser.stop()

        yield Timer(250000)


    dut.log.info("Cocotb I2C Io Layer - Basic test")
