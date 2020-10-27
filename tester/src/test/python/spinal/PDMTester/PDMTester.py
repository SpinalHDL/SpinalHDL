import random
from queue import Queue

import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import Timer, Edge, RisingEdge, Join, FallingEdge

@cocotb.coroutine
def genClock(dut, nCycles):
     for cycle in range(nCycles):
        dut.clk = 0
        yield Timer(10)
        dut.clk = 1
        yield Timer(10)

@cocotb.coroutine
def testDensity(dut, density):
    dut.io_enable = 0
    yield genClock(dut, 1)
    dut.io_enable = 1
    dut.io_density = density
    yield genClock(dut, 1)
    pulseCounter = 0
    for cycle in range(256):
        dut.clk = 0
        pulseCounter = pulseCounter + 1 if (dut.io_output == 1) else pulseCounter
        yield Timer(10)
        dut.clk = 1
        yield Timer(10)
    dut._log.info("Pulse Count : %s expected %s" % (str(pulseCounter),str(density)))
    if pulseCounter != density:
        raise TestFailure("FAIL: Pulse Count : %d != %d" % (int(pulseCounter), int(density)))

@cocotb.test()
def main_test(dut):
    dut.reset = 1
    yield genClock(dut, 2)
    dut.reset = 0

    for i in range(50):
        yield testDensity(dut, random.randint(0,256))

    yield testDensity(dut, 0)
    yield testDensity(dut, 256)

