import cocotb

from spinal.common.misc import ClockDomainAsyncReset


def pinsecClockGen(dut):
    cocotb.fork(ClockDomainAsyncReset(dut.io_axiClk, dut.io_asyncReset,20000))