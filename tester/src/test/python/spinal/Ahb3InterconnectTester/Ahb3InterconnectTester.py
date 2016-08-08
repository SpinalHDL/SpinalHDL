import random
from Queue import Queue

import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import Timer, Edge, RisingEdge, Join, FallingEdge

from spinal.common.Ahb3 import Ahb3MasterDriver, Ahb3SlaveMemory, Ahb3MasterIdle
from spinal.common.misc import setBit, randSignal, assertEquals, truncUInt, sint, ClockDomainAsyncReset, randBoolSignal, \
    BoolRandomizer, StreamRandomizer,StreamReader, FlowRandomizer, Bundle


@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    random.seed(0)


    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))


    # ahbMaster0 = Bundle(dut,"ahbMasters_0")
    Ahb3MasterDriver(Bundle(dut, "ahbMasters_0"), dut.clk, dut.reset)
    Ahb3MasterIdle(Bundle(dut, "ahbMasters_1"))
    Ahb3MasterIdle(Bundle(dut, "ahbMasters_2"))
    # Ahb3MasterDriver(Bundle(dut, "ahbMasters_1"), dut.clk, dut.reset)
    # Ahb3MasterDriver(Bundle(dut, "ahbMasters_2"), dut.clk, dut.reset)
    Ahb3SlaveMemory(Bundle(dut, "ahbSlaves_0"),0x0000,0x4000, dut.clk, dut.reset)
    Ahb3SlaveMemory(Bundle(dut, "ahbSlaves_1"),0x4000,0x4000, dut.clk, dut.reset)
    Ahb3SlaveMemory(Bundle(dut, "ahbSlaves_2"),0x8000,0x4000, dut.clk, dut.reset)
    Ahb3SlaveMemory(Bundle(dut, "ahbSlaves_3"),0xC000,0x4000, dut.clk, dut.reset)
    # ahbMaster0.HADDR <= 0
    for i in xrange(100):
        yield RisingEdge(dut.clk)
    #     ahbMaster0.HADDR <= int(ahbMaster0.HADDR) + 1



    dut.log.info("Cocotb test done")
