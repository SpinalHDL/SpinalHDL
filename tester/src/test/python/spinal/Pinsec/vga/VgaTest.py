import random

import cocotb
from cocotb.triggers import RisingEdge

from cocotblib.misc import simulationSpeedPrinter
from spinal.Pinsec.common.HexLoader import loadIHex
from spinal.Pinsec.common.Misc import pinsecClockGen


@cocotb.coroutine
def assertions(dut):

    for i in range(2500000):
        yield RisingEdge(dut.uut.io_vgaClk)

@cocotb.test()
def test1(dut):
    random.seed(0)
    uut = dut.uut

    cocotb.fork(simulationSpeedPrinter(uut.io_axiClk))
    yield loadIHex(uut,"../hex/vga.hex",uut.io_axiClk,uut.io_asyncReset)
    pinsecClockGen(dut)

    yield assertions(uut)
