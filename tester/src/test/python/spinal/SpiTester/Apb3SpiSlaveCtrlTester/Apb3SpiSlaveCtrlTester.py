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
def testIt(apb,spiCtrl, cpol, cpha):
    @coroutine
    def txFifo(data):
        yield apb.write(0,data)

    @coroutine
    def rxFifo(expected):
        yield apb.readAssertMasked(0,0x80000000 | expected, 0x8000FFFF)

        # while True:
        #     readThread = apb.read(0)
        #     yield readThread
        #     data = readThread.retval
        #     if testBit(data,31):
        #         assertEquals(data & 0xFF, expected, " APB readAssert failure")


    spiCtrl.init(cpol,cpha,10000,8)
    yield apb.write(8,cpol + cpha*2)
    yield TimerClk(apb.clk, 50)
    yield spiCtrl.exchange(0x23)
    yield spiCtrl.enable()
    yield spiCtrl.exchange(0x64)
    yield apb.writeMasked(4, 0x8000, 0x8000)
    yield spiCtrl.disable()
    yield spiCtrl.enable()
    yield spiCtrl.exchange(0xAA)
    yield txFifo(0x32)
    yield spiCtrl.exchangeCheck(0x55,0x32)
    yield spiCtrl.exchange(0xFF)
    yield rxFifo(0xAA)
    yield spiCtrl.disable()
    yield rxFifo(0x55)
    yield rxFifo(0xFF)
    yield spiCtrl.exchange(0x54)
    yield spiCtrl.enable()
    yield spiCtrl.exchange(0x00)
    yield rxFifo(0x00)
    yield spiCtrl.disable()
    yield spiCtrl.enable()
    yield txFifo(0x00)
    yield txFifo(0xFF)
    yield txFifo(0x42)
    yield spiCtrl.exchangeCheck(0x11,0x00)
    yield spiCtrl.exchangeCheck(0x22,0xFF)
    yield txFifo(0xAA)
    yield txFifo(0x55)
    yield spiCtrl.exchangeCheck(0x33,0x42)
    yield spiCtrl.exchangeCheck(0x44,0xAA)
    yield spiCtrl.exchangeCheck(0x55,0x55)
    yield spiCtrl.disable()

    yield rxFifo(0x11)
    yield rxFifo(0x22)
    yield rxFifo(0x33)
    yield rxFifo(0x44)
    yield rxFifo(0x55)

    yield TimerClk(apb.clk, 50)


@coroutine
def restart(dut):
    dut.reset <= 1
    yield Timer(10e3)
    dut.reset <= 0
    yield Timer(10e3)

@cocotb.test()
def test1(dut):
    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset,1000))
    cocotb.fork(simulationSpeedPrinter(dut.clk))
    cocotb.fork(SimulationTimeout(1000*20e3))

    apb = Apb3(dut, "io_apb", dut.clk)
    apb.idle()

    spi = SpiSlave(dut, "io_spi")
    spiCtrl = SpiSlaveMaster(spi)

    yield Timer(5000)
    yield RisingEdge(dut.clk)


    yield testIt(apb, spiCtrl, 0, 0)

    yield restart(dut)
    yield testIt(apb, spiCtrl, 0, 1)

    yield restart(dut)
    yield testIt(apb, spiCtrl, 1, 0)

    yield restart(dut)
    yield testIt(apb, spiCtrl, 1, 1)

