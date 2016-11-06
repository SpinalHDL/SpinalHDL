import cocotb
from cocotb.triggers import Timer

from cocotblib.misc import randSignal, assertEquals, ClockDomainAsyncReset


class Ref:
    def __init__(self,dut):
        pass

@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    from cocotblib.misc import cocotbXHack
    cocotbXHack()
    #random.seed(0)

    cocotb.fork(ClockDomainAsyncReset(dut.clk, None))

    for i in range(0,1000):
        randSignal(dut.io_inSIntA)
        randSignal(dut.io_inSIntB)
        yield Timer(1000)
        ref = Ref(dut)
        assertEquals(dut.io_outSInt, dut.io_outSIntRef, "io_outSInt")

    dut.log.info("Cocotb test done")
