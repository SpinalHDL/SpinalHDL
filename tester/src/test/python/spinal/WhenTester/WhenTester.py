import random

import cocotb
from cocotb.triggers import Timer, RisingEdge

from cocotblib.misc import randSignal, ClockDomainAsyncReset, assertEquals


class Ref:
    def __init__(self,dut):
        if int(dut.io_conds_0) == 1:
            self.io_outDefault = dut.io_data_1
        else:
            self.io_outDefault = dut.io_data_0

        if int(dut.io_conds_0) == 1:
            self.io_outComplex = dut.io_data_0
        elif int(dut.io_conds_1) == 1:
            self.io_outComplex = dut.io_data_1
            if int(dut.io_data_3) == int(dut.io_data_4):
                self.io_outComplex = dut.io_data_5
            elif int(dut.io_data_3) == int(dut.io_data_6):
                self.io_outComplex = dut.io_data_7
            elif int(dut.io_data_3) == 0x55:
                if int(dut.io_conds_2) == 1:
                    self.io_outComplex = 0xAA
                elif int(dut.io_conds_3) == 1:
                    self.io_outComplex = dut.io_data_8
            else:
                self.io_outComplex = dut.io_data_11
        else:
            if int(dut.io_conds_4) == 1:
                self.io_outComplex = dut.io_data_9
            else:
                self.io_outComplex = dut.io_data_10

        self.io_outDefault =  int(self.io_outDefault)
        self.io_outComplex = int(self.io_outComplex)


@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    random.seed(0)
    cocotb.fork(ClockDomainAsyncReset(dut.clk, None))

    for i in range(0,1000):
        randSignal(dut.io_conds_0)
        randSignal(dut.io_conds_1)
        randSignal(dut.io_conds_2)
        randSignal(dut.io_conds_3)
        randSignal(dut.io_conds_4)
        randSignal(dut.io_conds_5)
        randSignal(dut.io_conds_6)
        randSignal(dut.io_conds_7)
        randSignal(dut.io_data_0 )
        randSignal(dut.io_data_1 )
        randSignal(dut.io_data_2 )
        randSignal(dut.io_data_3 )
        randSignal(dut.io_data_4 )
        randSignal(dut.io_data_5 )
        randSignal(dut.io_data_6 )
        randSignal(dut.io_data_7 )
        randSignal(dut.io_data_8 )
        randSignal(dut.io_data_9 )
        randSignal(dut.io_data_10)
        randSignal(dut.io_data_11)
        yield RisingEdge(dut.clk)
        ref = Ref(dut)
        assertEquals(ref.io_outDefault,dut.io_outDefault,"io_outDefault")
        assertEquals(ref.io_outComplex, dut.io_outComplex, "io_outComplex")
        yield Timer(1)
        assertEquals(ref.io_outComplex, dut.io_outRegComplex, "io_outRegComplex")


    dut.log.info("Cocotb test done")
