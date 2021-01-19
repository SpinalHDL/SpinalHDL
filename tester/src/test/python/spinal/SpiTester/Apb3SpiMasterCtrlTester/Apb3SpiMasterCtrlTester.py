import random
from queue import Queue

import cocotb
from cocotb import fork, log
from cocotb.decorators import coroutine
from cocotb.triggers import RisingEdge, FallingEdge, Event, Timer


from cocotblib.Apb3 import Apb3
from cocotblib.Flow import Flow
from cocotblib.Spi import SpiMaster
from cocotblib.Stream import Stream, StreamDriverMaster, Transaction
from cocotblib.misc import assertEquals, randInt, ClockDomainAsyncReset, simulationSpeedPrinter, clockedWaitTrue, Bundle, randBits, randBool, SimulationTimeout, TimerClk, testBit, \
    setBit


class SlaveCmdData:
    def __init__(self, masterData, slaveData):
        self.masterData = masterData
        self.slaveData = slaveData


class SlaveCmdSs:
    def __init__(self, index, enable):
        self.index = index
        self.enable = enable



class SpiConfig:
    def __init__(self):
        self.sclkToggle = None
        self.ssSetup = None
        self.ssHold = None
        self.ssDisable = None
        self.cpha = None
        self.cpol = None


spiConfig = SpiConfig()

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

    @coroutine
    def rspData(expected):
        yield apb.pull(4, 32 << 16, 0xFF << 16)
        yield apb.readAssertMasked(0,0x80000000 | expected, 0x8000FFFF)


    @coroutine
    def setConfig(cpol, cpha, sclkToggle, ssSetup, ssHold, ssDisable):
        yield apb.write(8, cpol + cpha*2)
        yield apb.write(12, sclkToggle)
        yield apb.write(16, ssSetup)
        yield apb.write(20, ssHold)
        yield apb.write(24, ssDisable)
        global spiConfig

        spiConfig.sclkToggle = sclkToggle+1
        spiConfig.ssSetup = ssSetup + 1
        spiConfig.ssHold = ssHold + 1
        spiConfig.ssDisable = ssDisable + 1
        spiConfig.cpol = cpol
        spiConfig.cpha = cpha

    @coroutine
    def testIt():
        yield TimerClk(apb.clk, 50)

        yield cmdData(0x00, None)
        yield cmdData(0xFF, 0x00)
        yield rspData(0x00)
        yield cmdData(0x02, 0x42)
        yield cmdData(0xAA, None)
        yield cmdData(0x55, 0xFF)
        yield rspData(0x42)

        yield cmdSs(2, True)
        yield cmdData(0xAA, None)
        yield cmdData(0xAA, 0xAA)
        yield cmdSs(2, False)

        yield cmdSs(1, True)
        yield cmdData(0xAA, None)
        yield cmdData(0xAA, 0xEE)
        yield cmdSs(1, False)

        yield rspData(0xFF)
        yield rspData(0xAA)
        yield rspData(0xEE)

        yield TimerClk(apb.clk, 50)


    yield setConfig(cpol=0, cpha=0, sclkToggle=9, ssSetup=23, ssHold=27, ssDisable=31)
    yield testIt()
    yield setConfig(cpol=0, cpha=1, sclkToggle=9, ssSetup=23, ssHold=27, ssDisable=31)
    yield testIt()
    yield setConfig(cpol=1, cpha=0, sclkToggle=9, ssSetup=23, ssHold=27, ssDisable=31)
    yield testIt()
    yield setConfig(cpol=1, cpha=1, sclkToggle=9, ssSetup=23, ssHold=27, ssDisable=31)
    yield testIt()

sclkStable = 0
mosiStable = 0
ssStable = 0
sclkStableLast = 0
mosiStableLast = 0
ssStableLast = 0

@coroutine
def spiSlaveAgent(spi, queue, clk):
    global sclkStable
    global mosiStable
    global ssStable
    global sclkStableLast
    global mosiStableLast
    global ssStableLast

    @coroutine
    def wait(cycles):
        global sclkStable
        global mosiStable
        global ssStable
        global sclkStableLast
        global mosiStableLast
        global ssStableLast

        sclkLast = str(spi.sclk)
        mosiLast = str(spi.mosi)
        ssLast = str(spi.ss)
        for i in range(cycles):
            yield RisingEdge(clk)
            sclkNew = str(spi.sclk)
            mosiNew = str(spi.mosi)
            ssNew = str(spi.ss)

            sclkStable += 1
            mosiStable += 1
            ssStable += 1

            if sclkNew != sclkLast:
                sclkStableLast = sclkStable
                sclkStable = 0
            if mosiNew != mosiLast:
                mosiStableLast = mosiStable
                mosiStable = 0
            if ssNew != ssLast:
                ssStableLast = ssStable
                ssStable = 0


            sclkLast = sclkNew
            mosiLast = mosiNew
            ssLast = ssNew

    ssValue = 0xF
    while True:
        if queue.empty():
            yield wait(1)
            # assert(sclkStable > 1)
            # assert(mosiStable > 1)
            # assert(ssStable > 1)
        else:
            head = queue.get()
            if isinstance(head, SlaveCmdData):
                for i in range(8):
                    if spiConfig.cpha == False:
                        spi.miso <= testBit(head.slaveData, 7-i) if head.slaveData != None else randBool()
                        while True:
                            yield wait(1)
                            if spi.sclk == (not spiConfig.cpol):
                                break
                        assert sclkStableLast >= spiConfig.sclkToggle
                        assert mosiStable >= spiConfig.sclkToggle
                        assertEquals(spi.mosi, testBit(head.masterData, 7-i),"MOSI mismatch")
                        while True:
                            yield wait(1)
                            if spi.sclk == (spiConfig.cpol):
                                break
                        assert sclkStableLast >= spiConfig.sclkToggle
                    else:
                        while True:
                            yield wait(1)
                            if spi.sclk == (not spiConfig.cpol):
                                break
                        spi.miso <= testBit(head.slaveData, 7 - i) if head.slaveData != None else randBool()
                        assert sclkStableLast >= spiConfig.sclkToggle
                        while True:
                            yield wait(1)
                            if spi.sclk == (spiConfig.cpol):
                                break
                        assert mosiStable >= spiConfig.sclkToggle
                        assert sclkStableLast >= spiConfig.sclkToggle
                        assertEquals(spi.mosi, testBit(head.masterData, 7 - i), "MOSI mismatch")


            elif isinstance(head, SlaveCmdSs):
                while True:
                    yield wait(1)
                    assert sclkStable > 0
                    if spi.ss != ssValue:
                        break
                if head.enable:
                    yield wait(spiConfig.ssSetup-1)
                    print(str(ssStable) + " " + str(sclkStable))
                    assert ssStable >= spiConfig.ssSetup-1
                    assert sclkStable >= spiConfig.ssSetup-1
                else:
                    print(str(ssStableLast) + " " + str(sclkStable))
                    assert ssStableLast >= spiConfig.ssHold
                    assert sclkStable >= spiConfig.ssHold
                    yield wait(spiConfig.ssDisable-1)
                    print(str(ssStable) + " " + str(sclkStable))
                    assert ssStable >= spiConfig.ssDisable-1
                    assert sclkStable >= spiConfig.ssDisable-1


                assertEquals(spi.ss, setBit(ssValue, head.index, not head.enable), "SS mismatch")
                ssValue = int(spi.ss)



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
