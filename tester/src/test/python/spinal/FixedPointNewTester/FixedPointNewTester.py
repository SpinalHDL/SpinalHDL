import math

import cocotb
from cocotb.triggers import Timer
from cocotb.binary import BinaryValue

from cocotblib.misc import randSignal, assertEquals, truncUInt, sint, uint


class Ref:

    @classmethod
    def wrap_around(cls, n, wholeBits, fracBits, signed):
        """
        Wrap around fixed-point number
        :param n: The fixed-point number
        :param wholeBits: Number of whole bits
        :param fracBits: Number of fractional bits
        :param signed: Is signed
        :return:
        """
        totalBits = wholeBits + fracBits
        n = n*(2**fracBits)
        if signed:
            if n >= (2**(totalBits-1))-1:
                n -= (2**totalBits)
            elif n < -(2**(totalBits-1)):
                n += (2**totalBits)
        else:
            if n >= (2**totalBits):
                n -= (2**totalBits)
            else:
                n = 0
        n = n/(2**fracBits)
        return n

    @classmethod
    def trunc_fraction(cls, n, fracBits):
        """
        Truncates fixed-point fractional bits. Used by the unit test to avoid out-of-range fractional values, those
        smaller than representable.
        :param n: The fixed-point number
        :param fracBits: Number of fractional bits to keep
        :return: Truncated number
        """
        return math.trunc(n*(2**fracBits))/(2**fracBits)

    def __init__(self,dut):
        self.io_inFix_0 = sint(dut.io_inFix_0)/(2**4)
        self.io_inFix_1 = sint(dut.io_inFix_1)/(2**6)
        self.io_outFix = sint(dut.io_outFix)
        opMode = uint(dut.io_opMode)
        roundMode = uint(dut.io_roundMode)

        opResult = 0.0
        if opMode == 0:
            opResult = self.io_inFix_0 + self.io_inFix_1
            opResult = self.wrap_around(opResult, 12, 10, True)
        elif opMode == 1:
            opResult = self.io_inFix_0 - self.io_inFix_1
            opResult = self.wrap_around(opResult, 12, 10, True)
        elif opMode == 2:
            opResult = self.io_inFix_0 * self.io_inFix_1
        elif opMode == 3:
            opResult = self.io_inFix_0 / self.io_inFix_1
            opResult = self.trunc_fraction(opResult, 13)
        elif opMode == 4:
            opResult = self.io_inFix_0 % self.io_inFix_1

        roundResult = 0.0
        if roundMode == 0:
            # Ceil
            roundResult = math.ceil(opResult)
        elif roundMode == 1:
            # Floor
            roundResult = math.floor(opResult)
        elif roundMode == 2:
            # Floor to zero
            if (opResult >= 0):
                roundResult = math.floor(opResult)
            else:
                roundResult = math.ceil(opResult)
        elif roundMode == 3:
            # Ceil to infinity
            if (opResult >= 0):
                roundResult = math.ceil(opResult)
            else:
                roundResult = math.floor(opResult)
        elif roundMode == 4:
            # Half up
            roundResult = math.floor(opResult + 0.5)
        elif roundMode == 5:
            # Half down
            roundResult = math.ceil(opResult - 0.5)
        elif roundMode == 6:
            # Half to zero
            roundResult = math.ceil(abs(opResult) - 0.5)
            if opResult < 0:
                roundResult = -roundResult
        elif roundMode == 7:
            # Half to infinity
            roundResult = math.floor(abs(opResult) + 0.5)
            if opResult < 0:
                roundResult = -roundResult
        elif roundMode == 8:
            # Half to even
            frac_msb = math.trunc(abs(opResult)*10.0) % 10
            frac_rem = (abs(opResult)*10.0) - math.trunc(abs(opResult)*10.0)
            whole_lsb = math.trunc(abs(opResult))
            if frac_msb == 5 and frac_rem == 0:
                # Make the number even
                roundResult = abs(math.trunc(opResult)) + (whole_lsb % 2)
            else:
                # Half to zero
                roundResult = math.ceil(abs(opResult) - 0.5)
            if opResult < 0:
                roundResult = -roundResult
        elif roundMode == 9:
            # Half to odd
            frac_msb = math.trunc(abs(opResult)*10.0) % 10
            frac_rem = (abs(opResult)*10.0) - math.trunc(abs(opResult)*10.0)
            whole_lsb = math.trunc(abs(opResult))
            if frac_msb == 5 and frac_rem == 0:
                # Make the number even
                roundResult = abs(math.trunc(opResult)) + (1 - whole_lsb % 2)
            else:
                # Half to infinity
                roundResult = math.floor(abs(opResult) + 0.5)
            if opResult < 0:
                roundResult = -roundResult

        self.io_outFix_expected = roundResult


def check_results(dut, op, mode):

    # op_str = ["+", "-", "*", "/", "%"]
    # round_str = ["ceil", "floor", "floorToZero", "ceilToInf", "halfUp", "halfDown", "halfToZero", "halfToInf", "halfEven", "halfOdd"]

    ref = Ref(dut)
    # print()
    # inFix0_int = sint(dut.io_inFix_0)/(2**4)
    # inFix1_int = sint(dut.io_inFix_1)/(2**6)
    # outRaw_int = sint(dut.io_outRaw)/(2**13)
    # outFix_int = sint(dut.io_outFix)
    # inFix0_str = f"{dut.io_inFix_0}"
    # inFix1_str = f"{dut.io_inFix_1}"
    # outRaw_str = f"{dut.io_outRaw}"
    # print()
    # print(f"{inFix0_int:^{len(inFix0_str)}} {op_str[op]} {inFix1_int:^{len(inFix1_str)}} = {outRaw_int:^{len(outRaw_str)}}")
    # print(f"{inFix0_str} {op_str[op]} {inFix1_str} = {outRaw_str}")
    # round_func_str = f"{round_str[mode]}({outRaw_int})"
    # print(f"{round_func_str:>{len(inFix0_str)+len(inFix1_str)+3}} = {dut.io_outFix} ({outFix_int})")
    assertEquals(ref.io_outFix, ref.io_outFix_expected, "io_outFix")


def to_sint(i: int):
    if i >= 0:
        return i
    else:
        return (1 << i.bit_length())-(abs(i)-1)


@cocotb.test()
def random_test(dut):
    dut.log.info("Cocotb test boot")

    for op in range(0, 3):
        for mode in range(0, 10):
            for i in range(0, 100):
                randSignal(dut.io_inFix_0)
                randSignal(dut.io_inFix_1)
                dut.io_opMode.value = op
                dut.io_roundMode.value = mode
                yield Timer(1000)
                check_results(dut, op=op, mode=mode)

    dut.log.info("Cocotb test done")


@cocotb.test()
def half_quarter_test(dut):
    dut.log.info("Cocotb test boot")

    for mode in range(0, 10):
        dut.io_roundMode.value = mode
        dut.io_opMode.value = 0
        # Specific cases to check the half rounding
        for i in [-2.75, -2.5, -2.25, -2, -1.75, -1.5, -1.25, -1, -0.75, -0.5, -0.25,
                  0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5, 2.75]:
            # Avoid division by zero by shifting a 1 to the right-hand side
            dut.io_inFix_0.value = (math.trunc((i-1)*(2**4)))
            dut.io_inFix_1.value = 1*(2**6)
            yield Timer(1000)
            check_results(dut, op=0, mode=mode)

    dut.log.info("Cocotb test done")
