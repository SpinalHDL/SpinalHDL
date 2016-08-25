import random
from Queue import Queue

import cocotb
from cocotb.result import TestFailure, TestSuccess
from cocotb.triggers import Timer, Edge, RisingEdge, Join, FallingEdge

from spinal.Pinsec.common.CoreCom import readCoreValue, readCoreValueAssert
from spinal.Pinsec.common.HexLoader import loadIHex
from spinal.common.AhbLite3 import AhbLite3MasterDriver, AhbLite3SlaveMemory, AhbLite3MasterIdle, AhbLite3TraficGenerator, AhbLite3MasterReadChecker, AhbLite3Terminaison
from spinal.common.misc import setBit, randSignal, assertEquals, truncUInt, sint, ClockDomainAsyncReset, randBoolSignal, \
    BoolRandomizer, StreamRandomizer,StreamReader, FlowRandomizer, Bundle, simulationSpeedPrinter, readIHex, log2Up


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
