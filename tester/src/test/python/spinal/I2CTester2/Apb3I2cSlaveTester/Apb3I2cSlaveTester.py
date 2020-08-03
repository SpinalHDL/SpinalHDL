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
    def txData(valid = False, enable = False, value = 0xFF, repeat = False,  disableOnConflict = False):
        yield apb.write(0, (valid << 8) | (enable << 9) | (value << 0) | (repeat << 10) | (disableOnConflict << 11))

    @coroutine
    def txAck(valid=False, enable=False, value=0x1, repeat=False, disableOnConflict=False):
        yield apb.write(4, (valid << 8) | (enable << 9) | (value << 0) | (repeat << 10) | (disableOnConflict << 11))

    @coroutine
    def rxDataConfig(listen = False):
        yield apb.write(8, listen << 9)

    @coroutine
    def rxAckConfig(listen = False):
        yield apb.write(12, listen << 9)

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
    def addressFilter(index, enable = False, is10Bits = False, value = 0):
        yield apb.write(136+index*4, (enable << 15) | (is10Bits << 14) | (value << 0))

    @coroutine
    def addressFilterHits(value):
        yield apb.readAssertMasked(128, value, 0xFFFFFFFF)

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


    #Check address filter
    yield idle()
    yield addressFilter(index = 2, enable = True, is10Bits = False, value = 0x63)
    yield addressFilter(index = 1, enable = True, is10Bits = True, value = 0x123)
    for i in range(2):
        #7bits
        yield softMaster.wait(2)
        yield softMaster.sendStart()
        yield softMaster.sendByteCheck(0x63*2, 0x63*2)
        t = fork(softMaster.sendBitCheck(True, False))
        yield softMaster.wait(5)
        yield addressFilterHits(1 << 2)
        yield txData(valid=True, enable=True, value=0xF0)
        yield txAck(valid=True, enable=True, value=False)
        yield t.join()
        yield softMaster.sendByteCheck(0x44, 0x40)
        yield txAck(valid=True, repeat=True)
        yield softMaster.sendBitCheck(True, True)
        yield softMaster.sendStop()
        yield softMaster.wait(5)

        #10bits
        yield softMaster.wait(2)
        yield softMaster.sendStart()
        yield softMaster.sendByteCheck(0xF3,0xF3)
        yield softMaster.sendBitCheck(True, False)
        yield softMaster.sendByteCheck(0x23,0x23)
        t = fork(softMaster.sendBitCheck(True, False))
        yield softMaster.wait(5)
        yield addressFilterHits(1 << 1)
        yield txData(valid=True, enable=True, value=0xF0)
        yield txAck(valid=True, enable=True, value=False)
        yield t.join()
        yield softMaster.sendByteCheck(0x44, 0x40)
        yield txAck(valid=True, repeat=True)
        yield softMaster.sendBitCheck(True, True)
        yield softMaster.sendStop()
        yield softMaster.wait(5)

        #10bits no trigger
        yield softMaster.wait(2)
        yield softMaster.sendStart()
        yield softMaster.sendByteCheck(0xF3,0xF3)
        yield softMaster.sendBitCheck(True, False)
        yield softMaster.sendByteCheck(0x54,0x54)
        yield softMaster.sendBitCheck(True, True)
        yield addressFilterHits(0 << 1)
        yield softMaster.sendStop()
        yield softMaster.wait(5)

        #7bits no trigger
        yield softMaster.wait(2)
        yield softMaster.sendStart()
        yield softMaster.sendByteCheck(0x13,0x13)
        yield softMaster.sendBitCheck(True, True)
        yield addressFilterHits(0 << 1)
        yield softMaster.sendStop()
        yield softMaster.wait(5)