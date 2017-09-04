import random
from Queue import Queue

import cocotb
from cocotb import fork, log
from cocotb.decorators import coroutine
from cocotb.triggers import RisingEdge, FallingEdge, Event, Timer


from cocotblib.Apb3 import Apb3
from cocotblib.Flow import Flow
from cocotblib.Spi import SpiMaster, SpiSlave, SpiSlaveMaster
from cocotblib.Stream import Stream, StreamDriverMaster, Transaction
from cocotblib.misc import assertEquals, randInt, ClockDomainAsyncReset, simulationSpeedPrinter, clockedWaitTrue, Bundle, randBits, randBool, SimulationTimeout, TimerClk, testBit





@coroutine
def apbAgent(apb, slaveQueue):

    yield apb.write(8,0)
    yield TimerClk(apb.clk, 5000)
#
#
@coroutine
def spiSlaveAgent(spiCtrl):
    spiCtrl.init(0,0,10000,8)
    yield Timer(50e3)
    yield spiCtrl.exchange(0xAA)
    yield Timer(50e3)
    yield spiCtrl.enable()
    yield spiCtrl.exchange(0x55)
    yield spiCtrl.disable()
    yield spiCtrl.enable()
    yield spiCtrl.exchange(0x2)
    yield spiCtrl.exchange(0x3)
    yield spiCtrl.disable()
    yield Timer(50e3)
#     global sclkStable
#     global mosiStable
#     global ssStable
#
#     @coroutine
#     def wait(cycles):
#         global sclkStable
#         global mosiStable
#         global ssStable
#         sclkLast = bool(spi.sclk)
#         mosiLast = bool(spi.mosi)
#         ssLast = bool(spi.ss)
#         for i in xrange(cycles):
#             yield RisingEdge(clk)
#             sclkNew = bool(spi.sclk)
#             mosiNew = bool(spi.mosi)
#             ssNew = bool(spi.ss)
#
#             if sclkNew != sclkLast:
#                 sclkStable = 0
#             if mosiNew != mosiLast:
#                 mosiStable = 0
#             if ssNew != ssLast:
#                 ssStable = 0
#
#             sclkStable += 1
#             mosiStable += 1
#             ssStable += 1
#
#             sclkLast = sclkNew
#             mosiLast = mosiNew
#             ssLast = ssNew
#
#     while True:
#         if queue.empty():
#             yield wait(1)
#             # assert(sclkStable > 1)
#             # assert(mosiStable > 1)
#             # assert(ssStable > 1)
#         else:
#             head = queue.get()
#             if isinstance(head, SlaveCmdData):
#                 for i in xrange(8):
#                     spi.miso <= testBit(head.slaveData, 7-i) if head.slaveData != None else randBool()
#                     while True:
#                         yield wait(1)
#                         if spi.sclk == True:
#                             break
#
#                     while True:
#                         yield wait(1)
#                         if spi.sclk == False:
#                             break
#
#
#             elif isinstance(head, SlaveCmdSs):
#                 pass
#


@cocotb.test()
def test1(dut):
    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset,1000))
    cocotb.fork(simulationSpeedPrinter(dut.clk))
    cocotb.fork(SimulationTimeout(1000*20e3))

    apb = Apb3(dut, "io_apb", dut.clk)
    apb.idle()

    spi = SpiSlave(dut, "io_spi")
    spiCtrl = SpiSlaveMaster(spi)

    slaveQueue = Queue()

    yield Timer(5000)
    yield RisingEdge(dut.clk)

    apbThread = fork(apbAgent(apb,slaveQueue))
    spiThread = fork(spiSlaveAgent(spiCtrl))

    yield apbThread.join()
    yield spiThread.join()

