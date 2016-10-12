import cocotb
from cocotb.triggers import RisingEdge

from cocotblib.misc import randSignal, assertEquals, truncUInt, ClockDomainAsyncReset


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
        outA_ref = truncUInt(outA_ref + int(dut.io_inA), dut.io_outA)
        outB_ref = truncUInt(outB_ref + int(dut.io_inB), dut.io_outB)
    dut.log.info("Cocotb test done")
