import random
from queue import Queue

import cocotb
from cocotb.triggers import RisingEdge, Timer

from cocotblib.misc import assertEquals, ClockDomainAsyncReset
from cocotblib.Apb3 import Apb3

class UutModel:
    def __init__(self,dut):
        self.dut = dut

        cocotb.fork(self.loop())

    @cocotb.coroutine
    def loop(self):
        dut = self.dut
        ok1_en, ok2_en = False, False
        while True:
            yield RisingEdge(dut.clk)
            if ok1_en:
                assertEquals(dut.io_ok1, 1,"io_ok1")
            if ok2_en:
                assertEquals(dut.io_ok2, 1,"io_ok2")
            ok1_en, ok2_en = False, False

            if int(dut.io_bus_PWRITE) & int(dut.io_bus_PENABLE) == 1 :
                if dut.io_bus_PADDR == 0x0 and int(dut.io_bus_PWDATA) & 1 == 1:
                    ok1_en = True
                if dut.io_bus_PADDR == 0x0040 and int(dut.io_bus_PWDATA) & 2 == 2:
                    ok2_en = True


@cocotb.test()
def test1(dut):
    yield Timer(1)
    dut.log.info("Cocotb test boot")
    random.seed(0)
    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    apb3 = Apb3(dut, "io_bus", dut.clk)
    apb3.write(random.randint(0, 0x100), random.randint(0, 0xffffffff))
    uutModel = UutModel(dut)
    for i in range(0,5000):
        yield apb3.write(random.randint(0, 0x100), random.randint(0, 0xffffffff))

    dut.log.info("Cocotb test done")

