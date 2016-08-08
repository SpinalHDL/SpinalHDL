import random
from Queue import Queue

import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import Timer, Edge, RisingEdge, Join, FallingEdge

from spinal.common.misc import setBit, randSignal, assertEquals, truncUInt, sint, ClockDomainAsyncReset, randBoolSignal, \
    BoolRandomizer, StreamRandomizer,StreamReader, FlowRandomizer



@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    random.seed(0)


    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))



    dut.log.info("Cocotb test done")
