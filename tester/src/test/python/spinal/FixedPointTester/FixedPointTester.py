import cocotb
from cocotb.triggers import Timer

from cocotblib.misc import randSignal, assertEquals, truncUInt, sint


class Ref:
    def __init__(self,dut):
        self.io_outSFix_0 = truncUInt(sint(dut.io_inSFix_0) + ((sint(dut.io_inSFix_1) << 2)), dut.io_outSFix_0)
        self.io_outSFix_1 = truncUInt((sint(dut.io_inSFix_0) * sint(dut.io_inSFix_1)) >> 5, dut.io_outSFix_1)
        self.io_outBundleA_a_sfix = truncUInt(sint(dut.io_inBundleA_a_sfix) >> 2, dut.io_outBundleA_a_sfix)
        sfix2 = sint(dut.io_inSFix2)
        self.io_outSFix2 = truncUInt(((sfix2 << 1) + sfix2) << 1, dut.io_outSFix2)

@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    #random.seed(0)


    for i in range(0,1000):
        randSignal(dut.io_inSFix_0)
        randSignal(dut.io_inSFix_1)
        randSignal(dut.io_inBundleA_a_sfix)
        randSignal(dut.io_inSFix2)
        yield Timer(1000)
        ref = Ref(dut)
        assertEquals(ref.io_outSFix_0, dut.io_outSFix_0, "io_outSFix_0")
        assertEquals(ref.io_outSFix_1, dut.io_outSFix_1, "io_outSFix_1")
        assertEquals(ref.io_outSFix2, dut.io_outSFix2, "io_outSFix2")
        assertEquals(ref.io_outBundleA_a_sfix, dut.io_outBundleA_a_sfix, "io_outBundleA_a_sfix")

    dut.log.info("Cocotb test done")
