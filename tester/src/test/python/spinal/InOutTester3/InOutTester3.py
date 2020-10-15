import random

import cocotb
from cocotb.triggers import Timer, RisingEdge

from cocotblib.misc import randSignal, assertEquals, truncUInt, ClockDomainAsyncReset, Bundle


@cocotb.test()
def test1(dut):

    dut.log.info("Cocotb test boot")

    for i in range(100):
        randSignal(dut.bus_cmd_writeenable)
        randSignal(dut.bus_cmd_write)
        yield Timer(10)
        writeEnable = str(dut.bus_cmd_writeenable)
        write = str(dut.bus_cmd_write)
        expected = ""
        for i in range(8):
            if(writeEnable[i] == '0'):
                expected = expected + "z"
            else:
                expected = expected + str(write[i])
        assert expected == str(dut.bus_cmd_read).lower()
        assert expected == str(dut.bus_gpio).lower()



    dut.log.info("Cocotb test done")
