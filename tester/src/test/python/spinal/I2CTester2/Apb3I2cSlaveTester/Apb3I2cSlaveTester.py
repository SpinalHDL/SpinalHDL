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
    cocotb.fork(SimulationTimeout(2000 * 2.5e6))

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
    def txAck(valid=False, enable=False, value=0x1, repeat=False, startDrop=False, disableOnConflict=False):
        yield apb.write(4, (valid << 8) | (enable << 9) | (value << 0) | (repeat << 10) | (startDrop << 11) | (disableOnConflict << 12))

    @coroutine
    def rxDataConfig(listen = False):
        yield apb.write(8, listen << 15)

    @coroutine
    def rxAckConfig(listen = False):
        yield apb.write(12, listen << 15)

    @coroutine
    def rxDataValue(expected):
        yield apb.readAssertMasked(8, 0x100 | expected, 0x1FF)

    @coroutine
    def rxAckValue(expected):
        yield apb.readAssertMasked(12, 0x100 | expected, 0x101)

    @coroutine
    def rxDataNotValid():
        yield apb.readAssertMasked(8, 0, 0x100)

    @coroutine
    def rxAckNotValid():
        yield apb.readAssertMasked(12, 0, 0x100)


    @coroutine
    def idle():
        yield txData(valid = True, repeat = True)
        yield txAck(valid=True, repeat=True)
        yield rxDataConfig(listen=False)
        yield rxAckConfig(listen=False)

    buffer = [0]

    yield apb.write(40, 3)        #samplingClockDivider
    yield apb.write(44, 25*20-1)  #timeout
    yield apb.write(48, 4)        #tsuDat

    #Check idle controller
    yield softMaster.wait(2)
    yield softMaster.sendStart()
    yield softMaster.sendByteCheck(0xAA, 0xAA)
    yield softMaster.sendBitCheck(False, False)
    yield softMaster.sendByteCheck(0x55, 0x55)
    yield softMaster.sendBitCheck(True, True)
    yield softMaster.sendStop()
    yield softMaster.wait(5)


    # Check simple txData
    yield idle()
    yield txData(valid = True, enable = True, value = 0x0F)
    yield rxDataConfig(listen=True)
    yield softMaster.wait(2)
    yield softMaster.sendStart()
    yield softMaster.sendByteCheck(0xAA, 0x0A)
    yield softMaster.sendBitCheck(True, True)
    yield softMaster.sendStop()
    yield rxDataValue(0x0A)
    yield rxDataNotValid();
    yield softMaster.wait(5)


    # Check simple txAck
    yield idle()
    yield txAck(valid=True, enable=True, value=False)
    yield rxAckConfig(listen=True)
    yield softMaster.wait(2)
    yield softMaster.sendStart()
    yield softMaster.sendByteCheck(0xFF, 0xFF)
    yield softMaster.sendBitCheck(True, False)
    yield rxAckValue(False)
    yield txAck(valid=True, enable=True, value=True)
    yield softMaster.sendByteCheck(0x00, 0x00)
    yield softMaster.sendBitCheck(True, True)
    yield softMaster.sendStop()
    yield rxAckValue(True)
    yield softMaster.wait(5)


    # Check explicit idle controller
    yield idle()
    yield softMaster.wait(2)
    yield softMaster.sendStart()
    yield softMaster.sendByteCheck(0xAA, 0xAA)
    yield softMaster.sendBitCheck(False, False)
    yield softMaster.sendByteCheck(0x55, 0x55)
    yield softMaster.sendBitCheck(True, True)
    yield softMaster.sendStop()
    yield softMaster.wait(5)


    # Check tx clock stretching
    yield idle()
    yield txData(valid=False)
    yield txAck(valid=False)
    yield softMaster.wait(2)
    yield softMaster.sendStart()

    txThread = fork(softMaster.sendByteCheck(0xAA, 0x0A))
    yield softMaster.wait(10)
    yield txData(valid = True, enable = True, value = 0x0F)
    yield txThread.join()
    txThread = fork(softMaster.sendBitCheck(True, False))
    yield softMaster.wait(10)
    yield txAck(valid=True, enable=True, value=False)
    yield txThread.join()

    txThread = fork(softMaster.sendByteCheck(0x55, 0x50))
    yield softMaster.wait(10)
    yield txData(valid = True, enable = True, value = 0xF0)
    yield txThread.join()
    txThread = fork(softMaster.sendBitCheck(True, False))
    yield softMaster.wait(10)
    yield txAck(valid=True, enable=True, value=False)
    yield txThread.join()

    yield txData(valid = True, enable = True, value = 0x8F)
    yield txAck(valid=True, enable=True, value=True)
    yield softMaster.sendByteCheck(0xF3, 0x83)
    yield softMaster.sendBitCheck(True, True)
    yield softMaster.sendStop()
    yield softMaster.wait(5)


    # Check rxData clock streching
    yield idle()
    yield rxDataConfig(listen=True)
    yield softMaster.wait(2)
    yield softMaster.sendStart()
    yield softMaster.sendByteCheck(0x11, 0x11)
    yield softMaster.sendBitCheck(False, False)
    txThread = fork(softMaster.sendByteCheck(0x22, 0x22))
    yield softMaster.wait(16)
    yield rxDataValue(0x11)
    yield txThread.join()
    yield rxDataValue(0x22)
    yield softMaster.sendBitCheck(True, True)
    yield softMaster.sendStop()
    yield softMaster.wait(5)


    # Check rxAck clock streching
    yield idle()
    yield rxAckConfig(listen=True)
    yield softMaster.wait(2)
    yield softMaster.sendStart()
    yield softMaster.sendByteCheck(0x11, 0x11)
    yield softMaster.sendBitCheck(False, False)
    yield softMaster.sendByteCheck(0x22, 0x22)
    txThread = fork(softMaster.sendBitCheck(True, True))
    yield softMaster.wait(16)
    yield rxAckValue(False)
    yield txThread.join()
    yield rxAckValue(True)
    yield softMaster.sendStop()
    yield softMaster.wait(5)


    #check txData repeat
    yield idle()
    yield txData(valid = True, enable = True, value = 0x0F,repeat = True)
    yield softMaster.wait(2)
    yield softMaster.sendStart()
    yield softMaster.sendByteCheck(0x33, 0x03)
    yield softMaster.sendBitCheck(False, False)
    yield softMaster.sendByteCheck(0x44, 0x04)
    yield softMaster.sendBitCheck(False, False)
    yield softMaster.sendByteCheck(0x55, 0x05)
    yield softMaster.sendBitCheck(True, True)
    yield softMaster.sendStop()
    yield softMaster.wait(5)


    # check txAck repeat
    yield idle()
    yield txAck(valid=True, enable=True, value=0x0, repeat=True)
    yield softMaster.wait(2)
    yield softMaster.sendStart()
    yield softMaster.sendByteCheck(0x33, 0x33)
    yield softMaster.sendBitCheck(True, False)
    yield softMaster.sendByteCheck(0x44, 0x44)
    yield softMaster.sendBitCheck(True, False)
    yield softMaster.sendByteCheck(0x55, 0x55)
    yield softMaster.sendBitCheck(True, False)
    yield softMaster.sendStop()
    yield softMaster.wait(5)
