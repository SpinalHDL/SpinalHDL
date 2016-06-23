import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge

from spinal.common.misc import setBit, randSignal, assertEquals, truncUInt, sint, ClockDomainAsyncReset


class Ref:
    def __init__(self,dut):
        pass

@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    #random.seed(0)

    cocotb.fork(ClockDomainAsyncReset(dut.clk, None))

    for i in range(0,1000):
        randSignal(dut.io_inSIntA)
        randSignal(dut.io_inSIntB)
        yield Timer(1000)
        ref = Ref(dut)
        assertEquals(dut.io_outSInt, dut.io_outSIntRef, "io_outSInt")

    dut.log.info("Cocotb test done")
