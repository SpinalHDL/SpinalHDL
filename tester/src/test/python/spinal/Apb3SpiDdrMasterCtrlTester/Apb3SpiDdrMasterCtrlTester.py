import random
from queue import Queue

import cocotb
from cocotb.triggers import Edge, RisingEdge, FallingEdge, Timer

from cocotblib.Apb3 import Apb3
from cocotblib.misc import assertEquals, ClockDomainAsyncReset, simulationSpeedPrinter, waitClockedCond


@cocotb.coroutine
def checkTx(dut):
    yield 1



@cocotb.coroutine
def genCLock(dut):
    period = 100000
    dut.reset <= 1
    dut.clk <= 0
    dut.clkEarly <= 0
    yield Timer(period)
    dut.reset <= 0
    while True:
        dut.clk <= 0
        dut.clkEarly <= 0
        yield Timer(period/2)
        dut.clk <= 1
        dut.clkEarly <= 1
        yield Timer(period/2)


@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")

    from cocotblib.misc import cocotbXHack
    cocotbXHack()



    cocotb.fork(genCLock(dut))
    cocotb.fork(simulationSpeedPrinter(dut.clk))

    apb = Apb3(dut, "io_apb", dut.clk)
    apb.idle()

    dut.io_xip_cmd_valid <= False
    # bus.nonStopWrite(streamUnbuffered.data, bitOffset=0)
    # bus.nonStopWrite(streamUnbuffered.write, bitOffset=8)
    # bus.nonStopWrite(streamUnbuffered.read, bitOffset=9)
    # bus.nonStopWrite(streamUnbuffered.kind, bitOffset=11)
    # bus.read(fifoAvailability, address=baseAddress + 4, 16)
    @cocotb.coroutine
    def waitNotFull():
        while True:
            readThread = apb.read(4)
            yield readThread
            if (int(readThread.retval)  >> 16) != 0:
                break

    @cocotb.coroutine
    def waitEmpty():
        while True:
            readThread = apb.read(4)
            yield readThread
            if (int(readThread.retval)  >> 16) == 32:
                break


    @cocotb.coroutine
    def start():
        yield waitNotFull()
        yield apb.write(0, 0x880)

    @cocotb.coroutine
    def stop():
        yield waitNotFull()
        yield apb.write(0, 0x800)

    @cocotb.coroutine
    def write(data):
        yield waitNotFull()
        yield apb.write(0, 0x100 | data)

    @cocotb.coroutine
    def readPush(length):
        for i in range(length):
            yield waitNotFull()
            yield apb.write(0, 0x200)

    @cocotb.coroutine
    def read(result):
        readSent = 0
        for i in range(len(result)):
            while True:
                if readSent != len(result):
                    readThread = apb.read(4)
                    yield readThread
                    if int(readThread.retval) >> 1 != 0:
                        yield apb.write(0, 0x200)
                        readSent += 1
                readThread = apb.read(0)
                yield readThread
                ret = int(readThread.retval)
                if ret >> 31 != 0:
                    result[i] = ret & 0xFF
                    break


    @cocotb.coroutine
    def writeEnable():
        yield start()
        yield write(0x06)
        yield stop()

    @cocotb.coroutine
    def readStatus(result):
        yield start()
        yield write(0x05)
        yield read(result)
        yield stop()

    @cocotb.coroutine
    def whileFlashBusy():
        while True:
            pull = [0]
            yield readStatus(pull)
            if (pull[0] & 0x01) == 0:
                break

    @cocotb.coroutine
    def clearStatusRegister():
        yield start()
        yield write(0x50)
        yield stop()

    @cocotb.coroutine
    def fastRead(addr, tab):
        yield start()
        yield write(0x0B)
        yield write((addr >> 16) & 0xFF)
        yield write((addr >>  8) & 0xFF)
        yield write((addr >>  0) & 0xFF)
        yield write(0xFF)
        yield read(tab)
        yield stop()

    @cocotb.coroutine
    def programPage(addr, data):
        yield writeEnable()
        yield start()
        yield write(0x02)
        yield write((addr >> 16) & 0xFF)
        yield write((addr >>  8) & 0xFF)
        yield write((addr >>  0) & 0xFF)
        for b in data:
            yield write(b)
        yield stop()

        yield whileFlashBusy()
        yield clearStatusRegister()

    @cocotb.coroutine
    def writeRegister(id, data):
        yield start()
        yield write(id)
        yield write(data)
        yield stop()

    @cocotb.coroutine
    def sectorErase(addr):
        yield writeEnable()
        yield start()
        yield write(0xD8)
        yield write((addr >> 16) & 0xFF)
        yield write((addr >>  8) & 0xFF)
        yield write((addr >>  0) & 0xFF)
        yield stop()
        yield whileFlashBusy()

    @cocotb.coroutine
    def readVolatileConfig(tab):
        yield start()
        yield write(0x85)
        yield read(tab)
        yield stop()

    @cocotb.coroutine
    def writeVolatileConfig(tab):
        yield writeEnable()
        yield start()
        yield write(0x81)
        yield write(tab)
        yield stop()

    dut.vcc <= 0
    for i in range(300):
        yield Timer(10000)
        dut.vcc <= i*10
    # bus.drive(config.kind, baseAddress + 8, bitOffset=0)
    # bus.drive(config.mod, baseAddress + 8, bitOffset=4)
    yield apb.write(8, 0)
    yield apb.write(8, 0)
    # bus.drive(config.sclkToggle, baseAddress + 0x20)
    # bus.drive(config.fullRate, baseAddress + 0x20, bitOffset=31)
    yield apb.write(0x20, 2)
    # bus.drive(config.ss.setup, baseAddress + 0x24)
    yield apb.write(0x24, 14)
    # bus.drive(config.ss.hold, baseAddress + 0x28)
    yield apb.write(0x28, 18)
    # bus.drive(config.ss.disable, baseAddress + 0x2C)
    yield apb.write(0x2C, 22)



    # while True:
    yield start()
    yield write(0x9F)
    tab = [0]*3
    yield read(tab)
    print(tab)
    yield stop()

    # XIP = 0 dummy = 8
    yield start()
    yield write(0x81)
    yield write(0x83)
    yield stop()

    # Write lock register
    # yield writeRegister(0xE5, )



    yield programPage(0x1100, list(range(256)))
    yield programPage(0x1200, list(range(0x10,0x20)))


    tab = [0]*5
    yield fastRead(0x1105, tab)
    print(tab)
    assert(tab == [5,6,7,8,9])

    tab = [0]*5
    yield fastRead(0x1205, tab)
    print(tab)
    assert(tab == [0x15,0x16,0x17,0x18,0x19])

    tab = [0]*10
    yield fastRead(0x11FA, tab)
    print(tab)
    assert(tab == [0xFA,0xFB,0xFC,0xFD,0xFE,0xFF, 0x10,0x11,0x12,0x13])


    # bus.drive(enable, baseAddress + 0x40)
    # bus.drive(instructionData, baseAddress + 0x44, bitOffset=0)
    # bus.drive(instructionEnable, baseAddress + 0x44, bitOffset=8)
    # bus.drive(dummyData, baseAddress + 0x44, bitOffset=16)
    # bus.drive(dummyCount, baseAddress + 0x44, bitOffset=24)
    yield waitEmpty()
    yield apb.write(0x44, 0x00FF010B)
    yield apb.write(0x40, 0x1)



    @cocotb.coroutine
    def xipCmd(addr):
        dut.io_xip_cmd_valid <= True
        dut.io_xip_cmd_payload <= addr
        yield waitClockedCond(dut.clk, lambda: dut.io_xip_cmd_ready == True)
        dut.io_xip_cmd_valid <= False


    xipRspRef = Queue()
    @cocotb.coroutine
    def xipRspScoreboard():
        while True:
            yield waitClockedCond(dut.clk, lambda: dut.io_xip_rsp_valid == True)
            assert(not xipRspRef.empty())
            assert(xipRspRef.get_nowait() == int(dut.io_xip_rsp_payload))

    cocotb.fork(xipRspScoreboard())

    @cocotb.coroutine
    def xip(addr,value):
        xipRspRef.put(value)
        yield xipCmd(addr)


    yield xip(0x1100, 0x03020100)
    yield xip(0x1104, 0x07060504)
    yield xip(0x1108, 0x0B0A0908)
    yield xip(0x1100, 0x03020100)
    yield xip(0x1104, 0x07060504)
    yield xip(0x1108, 0x0B0A0908)
    yield xip(0x1100, 0x03020100)
    yield xip(0x1104, 0x07060504)
    yield xip(0x1108, 0x0B0A0908)

    yield waitClockedCond(dut.clk, lambda: xipRspRef.empty())

    yield apb.write(0x40, 0x0)
    yield stop()

    yield sectorErase(0x1205)

    tab = [0]*10
    yield fastRead(0x11FA, tab)
    print(tab)
    assert(tab == [0xFF]*10)

    yield waitEmpty()


    tab = [0,0]
    yield readVolatileConfig(tab)
    print(tab)

    yield Timer(1000000)



    dut.log.info("Cocotb test done")
