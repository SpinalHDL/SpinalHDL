import random
from Queue import Queue

import cocotb
from cocotb.result import TestFailure, TestSuccess
from cocotb.triggers import Timer, Edge, RisingEdge, Join, FallingEdge

from spinal.Pinsec.common.CoreCom import readCoreValue, readCoreValueAssert
from spinal.Pinsec.common.HexLoader import loadIHex
from spinal.Pinsec.common.Misc import pinsecClockGen
from spinal.common.AhbLite3 import AhbLite3MasterDriver, AhbLite3SlaveMemory, AhbLite3MasterIdle, AhbLite3TraficGenerator, AhbLite3MasterReadChecker, AhbLite3Terminaison
from spinal.common.misc import setBit, randSignal, assertEquals, truncUInt, sint, ClockDomainAsyncReset, randBoolSignal, \
    BoolRandomizer, StreamRandomizer,StreamReader, FlowRandomizer, Bundle, simulationSpeedPrinter, readIHex, log2Up



@cocotb.coroutine
def assertions(dut):

    for i in xrange(2500000):
        yield RisingEdge(dut.uut.io_vgaClk)

@cocotb.test()
def test1(dut):
    random.seed(0)
    uut = dut.uut

    cocotb.fork(simulationSpeedPrinter(uut.io_axiClk))
    yield loadIHex(uut,"../hex/vga.hex",uut.io_axiClk,uut.io_asyncReset)
    pinsecClockGen(dut)

    yield assertions(uut)
