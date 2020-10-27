import random

import cocotb
from cocotb.triggers import Timer, RisingEdge

from cocotblib.misc import randSignal, assertEquals, truncUInt, ClockDomainAsyncReset, Bundle


@cocotb.test()
def test1(dut):

    dut.log.info("Cocotb test boot")

    def assertGpio(value):
        assert(str(dut.cmd_read).lower() == value)
        assert(str(dut.bus_gpio).lower() == value)
        assert(str(dut.bus_cmd_read).lower() == value)
        assert(str(dut.buscpy_gpio_readed).lower() == value)

    @cocotb.coroutine
    def stim(drivers):
        for i in range(100):
            for toidle in drivers:
                randSignal(toidle.write)
                toidle.writeenable <= 0
            driver = random.choice(drivers)
            randSignal(driver.writeenable)
            yield Timer(10)
            if driver.writeenable == False:
                assertGpio("z")
            elif driver.write == False:
                assertGpio("0")
            else:
                assertGpio("1")


    drivers = [Bundle(dut,"bus_cmd"),Bundle(dut,"cmd"),Bundle(dut,"cmdbb")]
    yield stim(drivers)



    dut.log.info("Cocotb test done")
