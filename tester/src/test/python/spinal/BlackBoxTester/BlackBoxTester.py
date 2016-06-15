import random
from Queue import Queue

import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge, Join, Event, FallingEdge

from spinal.common.misc import setBit, randSignal, assertEquals, truncInt, sint, ClockDomainAsyncReset, randBoolSignal, \
    BoolRandomizer, simulationSpeedPrinter





@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")

    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    outA_ref = 0
    outB_ref = 0
    for i in range(0,1000):
        randSignal(dut.io_inA)
        randSignal(dut.io_inB)
        yield RisingEdge(dut.clk)
        assertEquals(outA_ref,dut.io_outA,"io_outA")
        assertEquals(outB_ref, dut.io_outB, "io_outB")
        outA_ref = truncInt(outA_ref + int(dut.io_inA), dut.io_outA)
        outB_ref = truncInt(outB_ref + int(dut.io_inB), dut.io_outB)
    dut.log.info("Cocotb test done")
