import random
from queue import Queue

import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import RisingEdge

from cocotblib.misc import randSignal, assertEquals, truncUInt, ClockDomainAsyncReset


class UutModel:
    def __init__(self,dut):
        self.dut = dut
        self.regA = 44
        self.regB = 44
        self.readAddresses = Queue()
        cocotb.fork(self.loop())

    @cocotb.coroutine
    def loop(self):
        dut = self.dut
        while True:
            yield RisingEdge(dut.clk)
            assertEquals(dut.io_nonStopWrited,truncUInt(int(dut.io_bus_w_payload_data) >> 4,dut.io_nonStopWrited),"io_nonStopWrited")
            # when read to addr=2 and write to addr=7 happen at the same cycle
            # the former takes precedence
            regBassigned = False
            if int(dut.io_bus_r_valid) & int(dut.io_bus_r_ready) == 1:
                if self.readAddresses.empty():
                    raise TestFailure("FAIL readAddresses is empty")
                addr = self.readAddresses.get()
                if addr == 9:
                    assertEquals(dut.io_bus_r_payload_data,self.regA << 10,"io_bus_r_payload_data")
                if addr == 7:
                    assertEquals(dut.io_bus_r_payload_data,self.regB << 10,"io_bus_r_payload_data")

                if addr == 2:
                    self.regB = 33
                    regBassigned = True


            if int(dut.io_bus_ar_valid) & int(dut.io_bus_ar_ready) == 1:
                addr = int(dut.io_bus_ar_payload_addr)
                self.readAddresses.put(addr)

            if (int(dut.io_bus_aw_valid) & int(dut.io_bus_aw_ready) & int(dut.io_bus_w_valid) & int(dut.io_bus_w_ready)) == 1:
                addr = int(dut.io_bus_aw_payload_addr)
                if addr == 9:
                    self.regA = truncUInt(int(dut.io_bus_w_payload_data) >> 10,20)
                if addr == 7 and not regBassigned:
                    self.regB = truncUInt(int(dut.io_bus_w_payload_data) >> 10,20)
                if addr == 15:
                    self.regA = 11




@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    random.seed(0)
    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))

    uutModel = UutModel(dut)
    for i in range(0,5000):
        randSignal(dut.io_bus_aw_valid)
        randSignal(dut.io_bus_aw_payload_addr)
        randSignal(dut.io_bus_w_valid)
        randSignal(dut.io_bus_w_payload_data)
        randSignal(dut.io_bus_ar_valid)
        randSignal(dut.io_bus_ar_payload_addr)
        randSignal(dut.io_bus_b_ready)
        randSignal(dut.io_bus_r_ready)
        yield RisingEdge(dut.clk)



    dut.log.info("Cocotb test done")
