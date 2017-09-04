import random
from Queue import Queue

import cocotb
from cocotb import fork, log
from cocotb.decorators import coroutine
from cocotb.triggers import RisingEdge, FallingEdge, Event, Timer


from cocotblib.Apb3 import Apb3
from cocotblib.Flow import Flow
from cocotblib.Spi import SpiMaster
from cocotblib.Stream import Stream, StreamDriverMaster, Transaction
from cocotblib.misc import assertEquals, randInt, ClockDomainAsyncReset, simulationSpeedPrinter, clockedWaitTrue, Bundle, randBits, randBool, SimulationTimeout, TimerClk, testBit


class SlaveCmdData:
    def __init__(self, masterData, slaveData):
        self.masterData = masterData
        self.slaveData = slaveData


class SlaveCmdSs:
    def __init__(self, index, enable):
        self.index = index
        self.enable = enable




@coroutine
def apbAgent(apb, slaveQueue):

    @coroutine
    def cmdData(masterData, slaveData):
        slaveQueue.put(SlaveCmdData(masterData, slaveData))
        # for i in xrange(8):
        #     if slaveData == None:
        #         slave
        #     slaveDataQueue.put(slaveData)
        yield apb.write(0, masterData | (0x01000000 if slaveData != None else 0))

    @coroutine
    def cmdSs(index, enable):
        slaveQueue.put(SlaveCmdSs(index, enable))
        yield apb.write(0, 0x10000000 | (0x01000000 if enable else 0) | index)


    yield apb.write(8, 0)
    yield apb.write(12, 9)
    yield apb.write(16, 23)
    yield apb.write(20, 27)
    yield apb.write(24, 31)

    # yield apb.readAssert(8, 0)
    # yield apb.readAssert(12, 20)
    # yield apb.readAssert(16, 24)
    # yield apb.readAssert(20, 28)
    # yield apb.readAssert(24, 32)


    yield cmdData(0x00, None)
    yield cmdData(0xFF, 0x00)
    yield cmdData(0x02, 0x42)
    yield cmdData(0xAA, None)
    yield cmdData(0x55, 0xFF)

    yield cmdSs(2, True)
    yield cmdData(0xAA, None)
    yield cmdData(0xAA, 0xAA)
    yield cmdSs(2, False)

    yield cmdSs(1, True)
    yield cmdData(0xAA, None)
    yield cmdData(0xAA, 0xEE)
    yield cmdSs(1, False)

    yield TimerClk(apb.clk, 5000)


sclkStable = 0
mosiStable = 0
ssStable = 0

@coroutine
def spiSlaveAgent(spi, queue, clk):
    global sclkStable
    global mosiStable
    global ssStable

    @coroutine
    def wait(cycles):
        global sclkStable
        global mosiStable
        global ssStable
        sclkLast = bool(spi.sclk)
        mosiLast = bool(spi.mosi)
        ssLast = bool(spi.ss)
        for i in xrange(cycles):
            yield RisingEdge(clk)
            sclkNew = bool(spi.sclk)
            mosiNew = bool(spi.mosi)
            ssNew = bool(spi.ss)

            if sclkNew != sclkLast:
                sclkStable = 0
            if mosiNew != mosiLast:
                mosiStable = 0
            if ssNew != ssLast:
                ssStable = 0

            sclkStable += 1
            mosiStable += 1
            ssStable += 1

            sclkLast = sclkNew
            mosiLast = mosiNew
            ssLast = ssNew

    while True:
        if queue.empty():
            yield wait(1)
            # assert(sclkStable > 1)
            # assert(mosiStable > 1)
            # assert(ssStable > 1)
        else:
            head = queue.get()
            if isinstance(head, SlaveCmdData):
                for i in xrange(8):
                    spi.miso <= testBit(head.slaveData, 7-i) if head.slaveData != None else randBool()
                    while True:
                        yield wait(1)
                        if spi.sclk == True:
                            break

                    while True:
                        yield wait(1)
                        if spi.sclk == False:
                            break


            elif isinstance(head, SlaveCmdSs):
                pass



@cocotb.test()
def test1(dut):
    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset,1000))
    cocotb.fork(simulationSpeedPrinter(dut.clk))
    cocotb.fork(SimulationTimeout(1000*20e3))

    apb = Apb3(dut, "io_apb", dut.clk)
    apb.idle()

    spi = SpiMaster(dut, "io_spi")

    slaveQueue = Queue()

    yield Timer(5000)
    yield RisingEdge(dut.clk)

    apbThread = fork(apbAgent(apb,slaveQueue))
    spiThread = fork(spiSlaveAgent(spi,slaveQueue, dut.clk))

    yield apbThread.join()

