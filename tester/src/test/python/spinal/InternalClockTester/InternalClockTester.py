import cocotb
from cocotb.triggers import RisingEdge

from cocotblib.misc import assertEquals, truncUInt, ClockDomainAsyncReset


class Ref:
    def __init__(self,dut):
        pass

@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    from cocotblib.misc import cocotbXHack
    cocotbXHack()
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
        counter = truncUInt(counter + 1, dut.io_internalClkCounter)

    dut.log.info("Cocotb test done")
