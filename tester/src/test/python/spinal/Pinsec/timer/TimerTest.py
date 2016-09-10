import random

import cocotb
from cocotb.triggers import Timer

from cocotblib.misc import ClockDomainAsyncReset, simulationSpeedPrinter
from spinal.Pinsec.common.HexLoader import loadIHex


@cocotb.coroutine
def assertions(dut):
    # yield readCoreValueAssert(dut,16, "A")
    # yield readCoreValueAssert(dut,16, "A2")
    # yield readCoreValueAssert(dut,0xAA, "B")
    # yield readCoreValueAssert(dut,3, "C")
    # yield readCoreValueAssert(dut,0x01, "D")
    # yield readCoreValueAssert(dut,0x02, "E")
    # yield readCoreValueAssert(dut,0x03, "F")
    # yield readCoreValueAssert(dut,0, "C")
    #
    # yield readCoreValueAssert(dut,0x10, "L")
    # yield readCoreValueAssert(dut,0x99, "M")
    # yield readCoreValueAssert(dut,0x12345678, "N")
    #
    # yield readCoreValueAssert(dut,0x9A, "O")
    #
    # yield readCoreValueAssert(dut, 0x10, "P")
    # yield readCoreValueAssert(dut, 0x9B, "Q")
    # yield readCoreValueAssert(dut, 0x1234567B, "R")

    yield Timer(1000*3000)

@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    random.seed(0)

    cocotb.fork(simulationSpeedPrinter(dut.io_axiClk))
    yield loadIHex(dut,"../hex/timer.hex",dut.io_axiClk,dut.io_asyncReset)
    cocotb.fork(ClockDomainAsyncReset(dut.io_axiClk, dut.io_asyncReset))

    yield assertions(dut)
    yield Timer(1000*10)

    dut.log.info("Cocotb test done")
