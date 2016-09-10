import cocotb
from cocotb.triggers import Timer

from cocotblib.misc import randSignal, assertEquals


class Ref:
    def __init__(self,dut):
        if int(dut.io_conds_0) == 1:
            self.io_outAA_a = int(dut.io_inA_0_a)
            self.io_outAA_c = int(dut.io_inA_0_c)
            self.io_outAA_b = int(dut.io_inAA_0_b)
            self.io_outAA_d = int(dut.io_inAA_0_d)
        else:
            self.io_outAA_a = 0
            self.io_outAA_c = 0
            self.io_outAA_b = 0
            self.io_outAA_d = 0


        if int(dut.io_conds_1) == 1:
            self.io_outAA_a = int(dut.io_inAA_1_a)
            self.io_outAA_c = int(dut.io_inAA_1_c)
            self.io_outAA_b = int(dut.io_inAA_1_b)
            self.io_outAA_d = int(dut.io_inAA_1_d)


@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    #random.seed(0)


    for i in range(0,1000):
        randSignal(dut.io_conds_0)
        randSignal(dut.io_conds_1)
        randSignal(dut.io_conds_2)
        randSignal(dut.io_conds_3)
        randSignal(dut.io_conds_4)
        randSignal(dut.io_conds_5)
        randSignal(dut.io_conds_6)
        randSignal(dut.io_conds_7)
        randSignal(dut.io_inAA_0_a)
        randSignal(dut.io_inAA_0_c)
        randSignal(dut.io_inAA_0_b)
        randSignal(dut.io_inAA_0_d)
        randSignal(dut.io_inAA_1_a)
        randSignal(dut.io_inAA_1_c)
        randSignal(dut.io_inAA_1_b)
        randSignal(dut.io_inAA_1_d)
        randSignal(dut.io_inAA_2_a)
        randSignal(dut.io_inAA_2_c)
        randSignal(dut.io_inAA_2_b)
        randSignal(dut.io_inAA_2_d)
        randSignal(dut.io_inA_0_a)
        randSignal(dut.io_inA_0_c)
        yield Timer(1000)
        ref = Ref(dut)
        assertEquals(ref.io_outAA_a, dut.io_outAA_a, "io_outAA_a")
        assertEquals(ref.io_outAA_b, dut.io_outAA_b, "io_outAA_b")
        assertEquals(ref.io_outAA_c, dut.io_outAA_c, "io_outAA_c")
        assertEquals(ref.io_outAA_d, dut.io_outAA_d, "io_outAA_d")

    dut.log.info("Cocotb test done")
