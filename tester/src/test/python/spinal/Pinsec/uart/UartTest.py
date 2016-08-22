import random
from Queue import Queue

import cocotb
from cocotb.result import TestFailure, TestSuccess
from cocotb.triggers import Timer, Edge, RisingEdge, Join, FallingEdge

from spinal.Pinsec.common.HexLoader import loadIHex
from spinal.common.AhbLite3 import AhbLite3MasterDriver, AhbLite3SlaveMemory, AhbLite3MasterIdle, AhbLite3TraficGenerator, AhbLite3MasterReadChecker, AhbLite3Terminaison
from spinal.common.misc import setBit, randSignal, assertEquals, truncUInt, sint, ClockDomainAsyncReset, randBoolSignal, \
    BoolRandomizer, StreamRandomizer,StreamReader, FlowRandomizer, Bundle, simulationSpeedPrinter, readIHex, log2Up


@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    random.seed(0)

    cocotb.fork(simulationSpeedPrinter(dut.io_axiClk))
    yield loadIHex(dut,"../hex/uart.hex",dut.io_axiClk,dut.io_asyncReset)
    cocotb.fork(ClockDomainAsyncReset(dut.io_axiClk, dut.io_asyncReset))



    yield Timer(1000*5000)

    dut.log.info("Cocotb test done")
