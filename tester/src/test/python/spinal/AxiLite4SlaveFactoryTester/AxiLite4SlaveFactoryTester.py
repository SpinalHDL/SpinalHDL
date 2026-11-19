import random

import cocotb
from cocotb.triggers import RisingEdge

from cocotblib.misc import randSignal, assertEquals, truncUInt, ClockDomainAsyncReset


class UutModel:
    def __init__(self,dut):
        self.dut = dut
        self.regA = 44
        self.regB = 44
        self.readAddress = None
        self.readResponse = None
        cocotb.fork(self.loop())

    @cocotb.coroutine
    def loop(self):
        dut = self.dut
        while True:
            yield RisingEdge(dut.clk)
            assertEquals(dut.io_nonStopWrited,truncUInt(int(dut.io_bus_w_payload_data) >> 4,dut.io_nonStopWrited),"io_nonStopWrited")
            if int(dut.reset):
                self.regA = 44
                self.regB = 44
                self.readAddress = None
                self.readResponse = None
                continue

            responseValid = self.readResponse is not None
            responseReady = int(dut.io_bus_r_ready) == 1
            assertEquals(dut.io_bus_ar_ready,self.readAddress is None,"io_bus_ar_ready")
            assertEquals(dut.io_bus_r_valid,responseValid,"io_bus_r_valid")
            if responseValid:
                assertEquals(dut.io_bus_r_payload_data,self.readResponse,"io_bus_r_payload_data")
                assertEquals(dut.io_bus_r_payload_resp,0,"io_bus_r_payload_resp")
                if responseReady:
                    self.readResponse = None

            # when read capture at addr=2 and write to addr=7 happen at the same cycle
            # the former takes precedence
            regBassigned = False
            if self.readAddress is not None and (not responseValid or responseReady):
                addr = self.readAddress
                self.readAddress = None
                self.readResponse = 0
                if addr == 9*4:
                    self.readResponse = self.regA << 10
                if addr == 7*4:
                    self.readResponse = self.regB << 10

                if addr == 2*4:
                    self.regB = 33
                    regBassigned = True


            if int(dut.io_bus_ar_valid) & int(dut.io_bus_ar_ready) == 1:
                self.readAddress = int(dut.io_bus_ar_payload_addr)

            if (int(dut.io_bus_aw_valid) & int(dut.io_bus_aw_ready) & int(dut.io_bus_w_valid) & int(dut.io_bus_w_ready)) == 1:
                addr = int(dut.io_bus_aw_payload_addr)
                if addr == 9*4:
                    self.regA = truncUInt(int(dut.io_bus_w_payload_data) >> 10,20)
                if addr == 7*4 and not regBassigned:
                    self.regB = truncUInt(int(dut.io_bus_w_payload_data) >> 10,20)
                if addr == 15*4:
                    self.regA = 11




@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    random.seed(0)
    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    dut.io_bus_w_payload_strb = 0b1111

    uutModel = UutModel(dut)
    for i in range(0,5000):
        randSignal(dut.io_bus_aw_valid)
        dut.io_bus_aw_payload_addr = random.randint(0, 15)*4
        randSignal(dut.io_bus_w_valid)
        randSignal(dut.io_bus_w_payload_data)
        randSignal(dut.io_bus_ar_valid)
        dut.io_bus_ar_payload_addr = random.randint(0, 15)*4
        randSignal(dut.io_bus_b_ready)
        randSignal(dut.io_bus_r_ready)
        yield RisingEdge(dut.clk)

    dut.io_bus_aw_valid = 0
    dut.io_bus_w_valid = 0
    dut.io_bus_ar_valid = 0
    dut.io_bus_b_ready = 1
    dut.io_bus_r_ready = 1
    for i in range(6):
        yield RisingEdge(dut.clk)
    assert uutModel.readAddress is None
    assert uutModel.readResponse is None



    dut.log.info("Cocotb test done")
