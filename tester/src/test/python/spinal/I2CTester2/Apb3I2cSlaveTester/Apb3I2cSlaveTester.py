import random

import cocotb
from cocotb import fork, log
from cocotb.decorators import coroutine
from cocotb.triggers import RisingEdge, FallingEdge, Event, Timer

from cocotblib.Apb3 import Apb3
from cocotblib.Flow import Flow
from cocotblib.Stream import Stream
from cocotblib.misc import assertEquals, randInt, ClockDomainAsyncReset, simulationSpeedPrinter, clockedWaitTrue, Bundle, SimulationTimeout
from spinal.I2CTester2.lib.misc import OpenDrainInterconnect, I2cSoftMaster





@cocotb.test()
def test1(dut):
    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset,100000))
    cocotb.fork(simulationSpeedPrinter(dut.clk))

    sclInterconnect = OpenDrainInterconnect()
    sclInterconnect.addHardDriver(dut.io_i2c_scl_write)
    sclInterconnect.addHardReader(dut.io_i2c_scl_read)

    sdaInterconnect = OpenDrainInterconnect()
    sdaInterconnect.addHardDriver(dut.io_i2c_sda_write)
    sdaInterconnect.addHardReader(dut.io_i2c_sda_read)

    softMaster = I2cSoftMaster(sclInterconnect.newSoftConnection(), sdaInterconnect.newSoftConnection(), 2500000,dut.clk)




    apb = Apb3(dut, "io_apb", dut.clk)
    apb.idle()

    @coroutine
    def txData(valid = False, enable = False, value = 0xFF, repeat = False, startDrop = False, disableOnConflict = False):
        yield apb.write(0, (valid << 8) | (enable << 9) | (value << 0) | (repeat << 10) | (startDrop << 11) | (disableOnConflict << 12))

    @coroutine
    def txData(valid=False, enable=False, value=0x1, repeat=False, startDrop=False, disableOnConflict=False):
        yield apb.write(0, (valid << 8) | (enable << 9) | (value << 0) | (repeat << 10) | (startDrop << 11) | (disableOnConflict << 12))



    yield apb.write(16, 3)        #samplingClockDivider
    yield apb.write(20, 25*10-1)  #timeout
    yield apb.write(24, 4)        #tsuDat


    yield softMaster.wait(2)
    yield softMaster.sendStart()
    yield softMaster.sendByte(0xAA)
    yield softMaster.sendBit(True)
    yield softMaster.sendStop()
    yield softMaster.wait(5)

    yield txData(valid = True, enable = True, value = 0x0F, disableOnConflict = True)
    yield softMaster.wait(2)
    yield softMaster.sendStart()
    yield softMaster.sendByte(0xFF)
    yield softMaster.sendBit(True)
    yield softMaster.sendStop()
    yield softMaster.wait(5)

    yield txData(valid=True, enable=True, value=0x0F, disableOnConflict=True)
    yield softMaster.wait(2)
    yield softMaster.sendStart()
    yield softMaster.sendByte(0xF9)
    yield softMaster.sendBit(True)
    yield softMaster.sendStop()
    yield softMaster.wait(5)

