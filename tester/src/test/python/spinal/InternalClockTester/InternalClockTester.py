import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge

from spinal.common.misc import setBit, randSignal, assertEquals, truncInt, sint, ClockDomainAsyncReset


class Ref:
    def __init__(self,dut):
        pass

@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    #random.seed(0)

    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    while True:
        yield RisingEdge(dut.clk)
        if int(dut.io_internalClkCounter) == 2:
            break
    counter = 3
    for i in range(0,1000):
        yield RisingEdge(dut.clk)
        yield RisingEdge(dut.clk)
        assertEquals(counter, dut.io_internalClkCounter, "io_internalClkCounter")
        counter = truncInt(counter + 1,dut.io_internalClkCounter)

    dut.log.info("Cocotb test done")
